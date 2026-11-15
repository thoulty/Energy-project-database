library(tidyverse)
library(here)
library(jsonlite)
library(sf)

raw <- here("5.Checks", "raw")
out <- here("5.Checks", "output")
dir.create(raw, recursive = TRUE, showWarnings = FALSE)
dir.create(out, recursive = TRUE, showWarnings = FALSE)

fetch <- function(name, expr) {
  path <- file.path(raw, paste0(name, ".csv"))
  df <- tryCatch(expr, error = function(e) {
    message("!! ", name, " download failed: ", conditionMessage(e)); NULL
  })
  if (!is.null(df) && nrow(df) > 0) {
    df <- as_tibble(df) |> mutate(across(everything(), as.character))
    write_csv(df, path)
    message("Downloaded ", name, ": ", nrow(df), " rows")
    return(df)
  }
  if (file.exists(path)) {
    message("Using saved copy of ", name)
    return(read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE))
  }
  NULL
}

find_col <- function(df, pattern, required = TRUE) {
  hit <- names(df)[str_detect(names(df), regex(pattern, ignore_case = TRUE))]
  if (length(hit) == 0) {
    if (required) stop("No column matching '", pattern, "'. Columns are:\n",
                       paste(names(df), collapse = "\n"))
    return(NA_character_)
  }
  hit[1]
}

num <- function(x) suppressWarnings(parse_number(as.character(x)))

results <- list()
add_result <- function(source, measure, gw, note = "") {
  results[[length(results) + 1]] <<- tibble(source, measure, gw = round(gw, 2), note)
}

tec_raw <- fetch("tec", fromJSON(
  "https://api.neso.energy/api/3/action/datastore_search?resource_id=17becbab-e3e8-473f-b303-3806f43a6a10&limit=50000"
)$result$records)

cmr_raw <- fetch("cmr", {
  res <- fromJSON("https://api.neso.energy/api/3/action/package_show?id=capacity-market-register")$result$resources
  url <- res |> filter(name == "Components", str_to_upper(format) == "CSV") |> pull(url) |> first()
  read_csv(url, col_types = cols(.default = col_character()), show_col_types = FALSE)
})

bmu_raw <- fetch("bmu", fromJSON("https://data.elexon.co.uk/bmrs/api/v1/reference/bmunits/all"))

ecr_raw <- fetch("ecr", {
  ids <- c("all-dnos-ecrs-combined1", "all-dnos-ecrs-combined",
           "ecr_manual_combine_test", "embedded-capacity-register")
  df <- NULL
  for (id in ids) {
    url <- paste0("https://northernpowergrid.opendatasoft.com/api/explore/v2.1/catalog/datasets/",
                  id, "/exports/csv?delimiter=%2C")
    df <- tryCatch(read_csv(url, col_types = cols(.default = col_character()), show_col_types = FALSE),
                   error = function(e) NULL)
    if (!is.null(df) && nrow(df) > 0 && ncol(df) > 10) { message("ECR dataset: ", id); break }
    df <- NULL
  }
  df
})

if (is.null(ecr_raw)) {
  message("!! No ECR data. Download it from ",
          "https://opennetzero.org/northern-powergrid-northeast/all-dnos-ecrs-combined1 ",
          "and save it as 5.Checks/raw/ecr.csv, then re-run.")
}

bats <- read_rds(here("2.DataClean", "bats.rds")) |>
  filter(is.na(country) | country != "Northern Ireland")

repd_op     <- bats |> filter(str_detect(stage, "Operational"))
repd_uc_old <- bats |> filter(str_detect(stage, "Under construction"),
                              under_construction < today() - months(18))
repd_queue  <- bats |> filter(str_detect(stage, "In planning|Consented|Under construction"))

add_result("REPD", "Operational", sum(repd_op$capacity_mw, na.rm = TRUE) / 1000,
           paste(nrow(repd_op), "projects"))
add_result("REPD", "Operational + under construction 18+ months",
           (sum(repd_op$capacity_mw, na.rm = TRUE) + sum(repd_uc_old$capacity_mw, na.rm = TRUE)) / 1000,
           "upper bound if every long-running build is finished")
add_result("REPD", "Planning + consented + under construction",
           sum(repd_queue$capacity_mw, na.rm = TRUE) / 1000,
           paste(nrow(repd_queue), "projects"))

tec <- NULL
if (!is.null(tec_raw)) {
  tec <- tec_raw |>
    filter(str_detect(`Plant Type`, "Energy Storage System")) |>
    mutate(mw_connected = num(`MW Connected`),
           mw_total     = num(`Cumulative Total Capacity (MW)`),
           id           = coalesce(`Project ID`, `Project Name`),
           battery_only = `Plant Type` == "Energy Storage System") |>
    group_by(id) |>
    summarise(name = first(`Project Name`), status = first(`Project Status`),
              plant = first(`Plant Type`), battery_only = first(battery_only),
              mw_connected = suppressWarnings(max(mw_connected, na.rm = TRUE)),
              mw_total     = suppressWarnings(max(mw_total, na.rm = TRUE)),
              .groups = "drop") |>
    mutate(across(c(mw_connected, mw_total), \(x) if_else(is.infinite(x), NA_real_, x)))

  message("\n=== TEC register: storage projects by status (GW)")
  tec |> count(battery_only, status, wt = mw_total / 1000, name = "gw") |> print(n = Inf)

  tec_conn <- tec |> filter(battery_only, mw_connected > 0)
  add_result("TEC", "Standalone batteries connected (transmission)",
             sum(tec_conn$mw_connected) / 1000, paste(nrow(tec_conn), "projects"))
  add_result("TEC", "Hybrid sites with storage connected (whole-site MW)",
             sum(tec$mw_connected[!tec$battery_only], na.rm = TRUE) / 1000,
             "can't split battery from solar/wind; not added to totals")
  add_result("TEC", "Standalone batteries in the queue (all statuses)",
             sum(tec$mw_total[tec$battery_only], na.rm = TRUE) / 1000,
             "compare with REPD planning + consented + under construction")

  message("\n=== TEC: largest connected standalone batteries")
  tec_conn |> arrange(desc(mw_connected)) |> select(name, status, mw_connected) |> print(n = 20)

  write_csv(tec, file.path(out, "tec_batteries.csv"))
}

ecr_conn <- NULL
if (!is.null(ecr_raw)) {
  area_col <- find_col(ecr_raw, "licen[cs]e.?area|dno", required = FALSE)
  if (!is.na(area_col)) {
    message("\n=== ECR licence areas covered (should include every DNO):")
    print(count(ecr_raw, .data[[area_col]]), n = Inf)
  }

  tech_cols  <- names(ecr_raw)[str_detect(names(ecr_raw), regex("energy.?source|conversion.?tech", TRUE))]
  status_col <- find_col(ecr_raw, "connection.?status")
  cap_col    <- find_col(ecr_raw, "already.?connected.*capacity|connected.*registered.*capacity")
  east_col   <- find_col(ecr_raw, "x.?coord|easting", required = FALSE)
  north_col  <- find_col(ecr_raw, "y.?coord|northing", required = FALSE)
  name_col   <- find_col(ecr_raw, "customer.?site|site.?name", required = FALSE)
  message("ECR columns used: technology = ", paste(tech_cols, collapse = ", "),
          "; status = ", status_col, "; capacity = ", cap_col,
          "; coords = ", east_col, " / ", north_col)

  ecr_batt <- ecr_raw |>
    mutate(tech_text  = do.call(paste, c(across(all_of(tech_cols)), sep = " | ")),
           is_storage = str_detect(tech_text, regex("batter|stor", TRUE)) &
                        !str_detect(tech_text, regex("pump", TRUE)),
           hybrid = str_detect(tech_text, regex("solar|photo|wind|gas|diesel|biomass|waste|chp", TRUE)),
           status = .data[[status_col]],
           mw     = num(.data[[cap_col]]),
           site   = if (!is.na(name_col))  .data[[name_col]]       else NA_character_,
           x      = if (!is.na(east_col))  num(.data[[east_col]])  else NA_real_,
           y      = if (!is.na(north_col)) num(.data[[north_col]]) else NA_real_) |>
    filter(is_storage)

  message("\n=== ECR storage by connection status (GW)")
  ecr_batt |> count(hybrid, status, wt = mw / 1000, name = "gw") |> print(n = Inf)

  ecr_conn <- ecr_batt |> filter(str_detect(status, regex("^connected", TRUE)))
  add_result("ECR", "Standalone batteries connected (distribution)",
             sum(ecr_conn$mw[!ecr_conn$hybrid], na.rm = TRUE) / 1000,
             paste(sum(!ecr_conn$hybrid), "sites"))
  add_result("ECR", "Hybrid sites with storage connected (whole-site MW)",
             sum(ecr_conn$mw[ecr_conn$hybrid], na.rm = TRUE) / 1000,
             "can't split battery from solar/wind; not added to totals")

  if (!is.null(tec)) {
    add_result("TEC + ECR", "Standalone batteries connected (GB total)",
               (sum(tec_conn$mw_connected) + sum(ecr_conn$mw[!ecr_conn$hybrid], na.rm = TRUE)) / 1000,
               "best estimate of what's actually connected")
  }

  write_csv(ecr_batt, file.path(out, "ecr_batteries.csv"))
}

if (!is.null(cmr_raw)) {
  cmr <- cmr_raw |>
    filter(`Primary Fuel of Component` == "Storage - Battery") |>
    mutate(mw = num(`Connection or DSR Capacity`))

  cmr_by_year <- cmr |>
    distinct(`Delivery Year`, `Component ID`, .keep_all = TRUE) |>
    group_by(`Delivery Year`) |>
    summarise(components = n(), gw = sum(mw, na.rm = TRUE) / 1000)

  message("\n=== Capacity Market: battery capacity by delivery year")
  print(cmr_by_year, n = Inf)

  this_year <- cmr_by_year |> filter(str_detect(`Delivery Year`, "^2026")) |> slice_head(n = 1)
  if (nrow(this_year) == 1) {
    add_result("Capacity Market", paste("Batteries contracted for delivery year", this_year$`Delivery Year`),
               this_year$gw, "includes new-build not yet running; excludes sites without contracts")
  }
}

if (!is.null(bmu_raw)) {
  bmu <- bmu_raw |> mutate(mw = num(generationCapacity))

  message("\n=== Elexon: fuel types in the register")
  print(count(bmu, fuelType, sort = TRUE), n = Inf)

  bmu_batt <- bmu |>
    filter(str_detect(paste(bmUnitName, leadPartyName), regex("batter|storage|bess", TRUE)),
           !str_detect(paste(bmUnitName, leadPartyName),
                       regex("pump|dinorwig|ffestiniog|cruachan|foyers", TRUE)),
           !fuelType %in% "PS")

  add_result("Elexon", "Battery-named BM units (generation capacity)",
             sum(bmu_batt$mw, na.rm = TRUE) / 1000,
             paste(nrow(bmu_batt), "units; lower bound, as many batteries aren't named as such"))

  write_csv(bmu_batt, file.path(out, "bmu_batteries.csv"))
}

if (!is.null(ecr_conn) && any(!is.na(ecr_conn$x))) {
  ecr_sf <- ecr_conn |>
    filter(!is.na(x), !is.na(y), between(x, 0, 700000), between(y, 0, 1300000)) |>
    st_as_sf(coords = c("x", "y"), crs = 27700)

  repd_sf <- bats |>
    filter(!is.na(x_fixed), !is.na(y_fixed),
           between(x_fixed, 0, 700000), between(y_fixed, 0, 1300000)) |>
    st_as_sf(coords = c("x_fixed", "y_fixed"), crs = 27700)

  likely_built <- repd_sf |>
    filter(str_detect(stage, "Consented|Under construction")) |>
    st_join(ecr_sf |> select(ecr_site = site, ecr_mw = mw),
            join = st_is_within_distance, dist = 1000, left = FALSE) |>
    st_drop_geometry() |>
    group_by(ref_id, site_name, operator_or_applicant, stage, capacity_mw) |>
    summarise(ecr_site = first(ecr_site), ecr_mw = sum(ecr_mw, na.rm = TRUE), .groups = "drop") |>
    filter(ecr_mw >= 0.5 * capacity_mw) |>
    distinct(ecr_site, .keep_all = TRUE) |>
    arrange(desc(capacity_mw))

  message("\n=== REPD projects not marked operational, but with a connected battery within 1 km")
  likely_built |> count(stage, wt = capacity_mw / 1000, name = "gw") |> print()
  print(head(likely_built, 20))
  add_result("REPD vs ECR", "Not operational in REPD but connected battery within 1 km",
             sum(likely_built$capacity_mw, na.rm = TRUE) / 1000,
             paste(nrow(likely_built), "projects; transmission sites not covered"))
  write_csv(likely_built, file.path(out, "repd_likely_operational.csv"))

  missing_from_repd <- ecr_sf |>
    filter(mw >= 1) |>
    st_join(repd_sf |> select(ref_id), join = st_is_within_distance, dist = 1000) |>
    filter(is.na(ref_id)) |>
    st_drop_geometry() |>
    distinct(site, status, hybrid, mw) |>
    arrange(desc(mw))

  message("\n=== Connected ECR batteries (1 MW+) with no REPD battery within 1 km")
  missing_from_repd |> count(hybrid, wt = mw / 1000, name = "gw") |> print()
  print(head(missing_from_repd, 20))
  add_result("REPD vs ECR", "Connected ECR batteries with no REPD battery nearby",
             sum(missing_from_repd$mw, na.rm = TRUE) / 1000,
             paste(nrow(missing_from_repd), "sites; hybrids may sit in REPD under solar/wind"))
  write_csv(missing_from_repd, file.path(out, "ecr_not_in_repd.csv"))
} else {
  message("\n!! Skipped the site-by-site check: no ECR data with coordinates.")
}

repd_not_op <- bats |>
  filter(str_detect(stage, "Consented|Under construction")) |>
  select(ref_id, site_name, stage, capacity_mw)

cand_list <- list()

if (!is.null(tec)) {
  generic <- "\\b(battery|batteries|energy|storage|bess|bes|system|facility|substation|tertiary|connection|zenobe|alcemi|sse|phase|solar|development|project|power|station|kv|mw)\\b"

  cand_list$tec <- tec |>
    filter(battery_only, status == "Built", mw_connected > 0) |>
    mutate(key = name |> str_to_lower() |> str_remove_all("[^a-z ]") |>
             str_remove_all(generic) |> str_squish() |> word(1)) |>
    filter(!is.na(key), nchar(key) >= 4) |>
    cross_join(repd_not_op) |>
    filter(str_detect(str_to_lower(site_name), paste0("\\b", key, "\\b"))) |>
    transmute(ref_id, site_name, stage, capacity_mw, evidence = "TEC Built",
              matched_to = name, matched_mw = mw_connected) |>
    filter(between(matched_mw / capacity_mw, 0.6, 1.6))
}

if (exists("likely_built")) {
  cand_list$ecr <- likely_built |>
    transmute(ref_id, site_name, stage, capacity_mw, evidence = "ECR connected",
              matched_to = ecr_site, matched_mw = ecr_mw)
}

candidates <- bind_rows(cand_list)
if (nrow(candidates) > 0) {
  candidates <- candidates |>
    group_by(ref_id) |>
    summarise(across(c(site_name, stage, capacity_mw), first),
              evidence   = paste(unique(evidence), collapse = " + "),
              matched_to = paste(unique(matched_to), collapse = "; "),
              matched_mw = max(matched_mw), .groups = "drop") |>
    arrange(desc(capacity_mw)) |>
    mutate(confirm = "")

  write_csv(candidates, file.path(out, "stage_candidates.csv"))
  message("\n=== ", nrow(candidates), " stage-adjustment candidates (",
          round(sum(candidates$capacity_mw, na.rm = TRUE) / 1000, 2),
          " GW) saved to 5.Checks/output/stage_candidates.csv")
  print(head(candidates, 20))

  if (!file.exists(here("5.Checks", "stage_overrides.csv"))) {
    message("Next: copy it to 5.Checks/stage_overrides.csv and mark correct matches with 'y'.")
  }
}

summary_tbl <- bind_rows(results)
message("\n==================== SUMMARY (GW) ====================")
print(summary_tbl, n = Inf, width = Inf)
write_csv(summary_tbl, file.path(out, "summary.csv"))