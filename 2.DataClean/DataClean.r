library(tidyverse)
library(janitor)
library(here)

repd_file <- here("1.DataRaw", "REPD.csv")
stopifnot(file.exists(repd_file))

repd_raw <- read_csv(
  repd_file,
  locale         = locale(encoding = "latin1"),
  col_types      = cols(.default = col_character()),
  na             = c("", "NA", "N/A", "n/a", "-"),
  show_col_types = FALSE
)

repd <- repd_raw |>
  clean_names() |>
  remove_empty(c("rows", "cols")) |>
  mutate(across(everything(), str_squish))

date_cols <- c("planning_application_submitted", "planning_permission_granted",
               "appeal_granted", "secretary_of_state_granted",
               "planning_permission_refused", "appeal_refused",
               "planning_permission_expired", "under_construction", "operational")

duplicate_ids <- c("13644")

bats <- repd |>
  filter(technology_type == "Battery",
         !ref_id %in% duplicate_ids) |>
  mutate(
    capacity_mw = parse_number(installed_capacity_m_welec),
    across(any_of(date_cols),
           \(x) as_date(parse_date_time(x, orders = c("dmy", "ymd"), quiet = TRUE))),
    year         = year(planning_application_submitted),
    consent_date = coalesce(planning_permission_granted, appeal_granted,
                            secretary_of_state_granted),
    co_located   = !is.na(storage_co_location_repd_ref_id),
    x            = as.numeric(x_coordinate),
    y            = as.numeric(y_coordinate),
    swapped      = !is.na(x) & !is.na(y) & ((x > 700000 & y < 700000) | (region == "Scotland" & x > 500000 & y < 500000)),     
    x_fixed      = if_else(swapped, y, x),
    y_fixed      = if_else(swapped, x, y),
    segment = case_when(
      is.na(capacity_mw) ~ NA_character_,
      capacity_mw < 1    ~ "Micro (<1 MW)",
      TRUE               ~ "Grid-scale (1 MW+)"
    ),
    status = str_to_lower(development_status_short),
    stage  = case_when(
      str_detect(status, "decommissioned")                          ~ "X. Inactive",
      str_detect(status, "operational")                             ~ "5. Operational",
      str_detect(status, "under construction")                      ~ "4. Under construction",
      str_detect(status, "awaiting construction|granted|no application required")
                                                                    ~ "3. Consented",
      str_detect(status, "submitted|appeal lodged|intervened")      ~ "2. In planning",
      str_detect(status, "refused|withdrawn|abandoned|expired|revised")
                                                                    ~ "X. Inactive",
      TRUE                                                          ~ "Other"
    )
  )
print(count(bats, development_status_short, stage, sort = TRUE), n = Inf)
print(summary(bats$capacity_mw))
print(count(bats, segment))
message(nrow(bats), " battery projects")

write_rds(bats, here("2.DataClean", "bats.rds"))