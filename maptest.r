# Map.r — every live battery project on a UK map
library(tidyverse)
library(here)
library(sf)
library(rnaturalearth)

live <- read_rds(here("3.DataAnalysis", "live.rds"))

# Turn the coordinates into map points (British National Grid = EPSG:27700)
live_sf <- live |>
  mutate(x = as.numeric(x_coordinate),
         y = as.numeric(y_coordinate)) |>
  filter(!is.na(x), !is.na(y), country != "Northern Ireland") |>
  st_as_sf(coords = c("x", "y"), crs = 27700)

# UK outline, converted to the same coordinate system
uk <- ne_countries(country = "United Kingdom", scale = "medium", returnclass = "sf") |>
  st_transform(27700)

p_map <- ggplot() +
  geom_sf(data = uk, fill = "grey95", colour = "grey75", linewidth = 0.2) +
  geom_sf(data = live_sf, aes(size = capacity_mw),
          colour = "#2a7f62", alpha = 0.4) +
  scale_size_area(max_size = 5, name = "Capacity (MW)") +
  facet_wrap(~ stage, nrow = 1) +
  labs(title = "Where the UK's battery projects are",
       caption = "Source: DESNZ Renewable Energy Planning Database. Northern Ireland excluded.") +
  theme_void() +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", margin = margin(b = 4)),
        plot.title = element_text(face = "bold", margin = margin(b = 10)),
        plot.caption = element_text(size = 8, hjust = 0),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(10, 10, 10, 10))

print(p_map)

ggsave(here("4.Map", "map.png"), p_map, width = 12, height = 6, dpi = 300, bg = "white")