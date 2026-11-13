library(tidyverse)
library(here)
library(sf)
library(rnaturalearth)

live <- read_rds(here("3.DataAnalysis", "live.rds"))
stopifnot("x_fixed" %in% names(live))

live_sf <- live |>
  filter(!is.na(x_fixed), !is.na(y_fixed),
         country != "Northern Ireland",
         between(x_fixed, 0, 700000), between(y_fixed, 0, 1300000)) |>
  st_as_sf(coords = c("x_fixed", "y_fixed"), crs = 27700)

uk <- ne_countries(country = "United Kingdom", scale = "medium", returnclass = "sf") |>
  st_transform(27700)

p_map <- ggplot() +
  geom_sf(data = uk, fill = "grey95", colour = "grey75", linewidth = 0.2) +
  geom_sf(data = live_sf, aes(size = capacity_mw), colour = "#2a7f62", alpha = 0.5) +
  scale_size_area(max_size = 6, name = "Capacity (MW)") +
  facet_wrap(~ stage, nrow = 1) +
  labs(title = "Where the UK's grid-scale battery projects are",
       caption = "Source: DESNZ REPD. Grid-scale projects (1 MW+). Northern Ireland excluded.") +
  theme_void() +
  theme(legend.position = "bottom",
        strip.text      = element_text(face = "bold", margin = margin(b = 4)),
        plot.title      = element_text(face = "bold", margin = margin(b = 10)),
        plot.caption    = element_text(size = 8, hjust = 0),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin     = margin(10, 10, 10, 10))

ggsave(here("4.Map", "map.png"), p_map, width = 12, height = 6, dpi = 300, bg = "white")

message(nrow(live) - nrow(live_sf), " projects not mapped (missing coordinates or NI)")