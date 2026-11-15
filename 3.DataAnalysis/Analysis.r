library(tidyverse)
library(here)
library(scales)

clean_developer <- function(name) {
  name <- name |>
    str_to_lower() |>
    str_remove_all("\\b(ltd|limited|plc|llp|uk)\\b|[.,]") |>
    str_squish() |>
    str_to_title()

  case_when(
    str_detect(name, "^West Burton")   ~ "West Burton Energy",
    str_detect(name, "^Gresham House") ~ "Gresham House",
    str_detect(name, "^Alcemi")        ~ "Alcemi",
    str_detect(name, "^Sandbrook")     ~ "Sandbrook Capital",
    str_detect(name, "^Natpower")      ~ "NatPower",
    str_detect(name, "^Newton Energ")  ~ "Newton Energy",
    TRUE                              ~ name
  )
}

bats <- read_rds(here("2.DataClean", "bats.rds"))
dir.create(here("3.DataAnalysis", "plots"), showWarnings = FALSE)

green <- "#2a7f62"
source_note <- "Source: DESNZ Renewable Energy Planning Database. Grid-scale projects (1 MW+) only."

live <- bats |>
  filter(str_starts(stage, "[2-5]"), segment == "Grid-scale (1 MW+)")

write_rds(live, here("3.DataAnalysis", "live.rds"))

funnel <- live |>
  group_by(stage) |>
  summarise(projects = n(), gw = sum(capacity_mw) / 1000)

p_funnel <- ggplot(funnel, aes(gw, fct_rev(stage))) +
  geom_col(fill = green) +
  geom_text(aes(label = paste0(round(gw, 1), " GW | ", comma(projects), " projects")),
            hjust = -0.05, size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.4))) +
  labs(title = "UK battery storage pipeline",
       x = "Capacity (GW)", y = NULL, caption = source_note) +
  theme_minimal()

ggsave(here("3.DataAnalysis", "plots", "funnel.png"), p_funnel,
       width = 8, height = 4, dpi = 300)

by_region <- live |>
  mutate(region = replace_na(region, "Unknown")) |>
  group_by(region, stage) |>
  summarise(gw = sum(capacity_mw) / 1000, .groups = "drop")

p_region <- ggplot(by_region, aes(gw, fct_reorder(region, gw, sum), fill = fct_rev(stage))) +
  geom_col() +
  scale_fill_manual(values = c("5. Operational"        = "#1b5e45",
                               "4. Under construction" = "#2a7f62",
                               "3. Consented"          = "#7fbfa5",
                               "2. In planning"        = "#cfe8dd")) +
  labs(title = "Battery pipeline by region",
       x = "Capacity (GW)", y = NULL, fill = NULL, caption = source_note) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(here("3.DataAnalysis", "plots", "by_region.png"), p_region,
       width = 8, height = 6, dpi = 300)

segment_counts <- bats |>
  filter(between(year, 2016, 2026), !is.na(segment)) |>
  count(year, segment) |>
  pivot_wider(names_from = segment, values_from = n, values_fill = 0)

micro_devs <- bats |>
  filter(segment == "Micro (<1 MW)") |>
  mutate(developer = clean_developer(operator_or_applicant)) |>
  filter(!is.na(developer), developer != "") |>
  count(developer, sort = TRUE) |>
  head(5)

size_stats <- bats |>
  filter(segment == "Grid-scale (1 MW+)", between(year, 2016, 2025)) |>
  group_by(year) |>
  summarise(
    projects = n(),
    p25      = quantile(capacity_mw, 0.25),
    median   = median(capacity_mw),
    p75      = quantile(capacity_mw, 0.75),
    p90      = quantile(capacity_mw, 0.90)
  )

p_size <- ggplot(size_stats, aes(year)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = green, alpha = 0.2) +
  geom_line(aes(y = median), colour = green, linewidth = 1) +
  geom_line(aes(y = p90), colour = green, linetype = "dashed") +
  geom_hline(yintercept = 50, colour = "grey50", linetype = "dotted") +
  annotate("text", x = 2016.2, y = 58, label = "50 MW: former consent threshold (removed 2020)",
           hjust = 0, vjust = 0, size = 2.8, colour = "grey40") +
  scale_x_continuous(breaks = 2016:2025) +
  labs(
    title    = "Grid-scale battery projects: bigger at the top end",
    subtitle = "Solid line = median · shaded band = middle 50% · dashed line = largest 10%",
    x = "Application year", y = "Capacity (MW)",
    caption  = paste("Source: DESNZ REPD. Grid-scale projects (1 MW+) only; excludes micro-batteries",
                     "such as AMP Clean Energy's 0.2 MW Battery Box sites and records without capacity.",
                     "Batteries over 50 MW needed national consent in England and Wales until July 2020.",
                     sep = "\n")
  ) +
  theme_minimal() +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(size = 8, hjust = 0))

ggsave(here("3.DataAnalysis", "plots", "size_trend.png"), p_size,
       width = 8, height = 5, dpi = 300)

timeline_data <- bats |>
  filter(segment == "Grid-scale (1 MW+)") |>
  mutate(
    months_to_consent = time_length(consent_date - planning_application_submitted, "months"),
    months_to_build   = time_length(operational - consent_date, "months")
  )

timelines <- timeline_data |>
  summarise(
    median_months_to_consent = median(months_to_consent, na.rm = TRUE),
    n_consent                = sum(!is.na(months_to_consent)),
    median_months_to_build   = median(months_to_build, na.rm = TRUE),
    n_build                  = sum(!is.na(months_to_build))
  )

approval <- timeline_data |>
  mutate(approved = !is.na(consent_date),
         refused  = str_detect(status, "refused")) |>
  filter(approved | refused) |>
  summarise(decisions = n(), approval_rate = mean(approved))

consent_trend <- timeline_data |>
  filter(between(year, 2016, 2024), !is.na(months_to_consent), months_to_consent >= 0) |>
  group_by(year) |>
  summarise(median_months = median(months_to_consent), projects = n())

top_devs <- live |>
  mutate(developer = clean_developer(operator_or_applicant)) |>
  filter(!is.na(developer), developer != "") |>
  group_by(developer) |>
  summarise(projects = n(), gw = sum(capacity_mw) / 1000) |>
  slice_max(gw, n = 15)

top15_share <- sum(top_devs$gw) / (sum(live$capacity_mw) / 1000)

p_devs <- ggplot(top_devs, aes(gw, fct_reorder(developer, gw))) +
  geom_col(fill = green) +
  geom_text(aes(label = if_else(projects == 1, "1 project", paste(projects, "projects"))),
            hjust = -0.1, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Top 15 developers by live grid-scale battery pipeline",
       subtitle = paste0("Together they hold ", percent(top15_share, accuracy = 1),
                         " of the live pipeline"),
       x = "Capacity (GW)", y = NULL,
       caption = "Source: DESNZ REPD. Live = in planning, consented, under construction or operational.\nDeveloper = planning applicant, which may differ from the current owner.") +
  theme_minimal()

ggsave(here("3.DataAnalysis", "plots", "developers.png"), p_devs,
       width = 8, height = 6, dpi = 300)

print(funnel)
print(segment_counts, n = Inf)
print(micro_devs)
print(size_stats)
print(timelines)
print(approval)
print(consent_trend)
print(top_devs, n = 15)
message("Top 15 share of live pipeline: ", percent(top15_share, accuracy = 0.1))