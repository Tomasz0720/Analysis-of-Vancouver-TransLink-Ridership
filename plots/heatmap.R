# Heat Map
# Purpose: Show ridership by bus stop (bus stops with higher ridership have deeper colours).

install.packages("raster")

# Imports
library(sf)
library(tidyverse)
library(maptiles)
library(tidyterra)
library(scales)

stops <- read_csv("data/TransLink_Bus_Stop_Ridership_2019_2024.csv")

stops_sf <- stops |>
  st_as_sf(coords = c("Stop_Longitude", "Stop_Latitude"), crs = 4326) |>
  st_transform(32610) |>
  mutate(Stop_Avg_Daily_Boardings_Week = Stop_Avg_Daily_Boardings_MF + Stop_Avg_Daily_Boardings_Sat + Stop_Avg_Daily_Boardings_SunHol) |>
  filter(!is.na(Stop_Avg_Daily_Boardings_Week))


coords <- st_coordinates(stops_sf)

stops_sf_coords <- stops_sf |>
  mutate(lon = as.numeric(coords[,1]), lat = as.numeric(coords[,2]))


bg_tiles_bus <- get_tiles(stops_sf, provider = "CartoDB.Positron", zoom = 11)


ggplot(stops_sf_coords) +
  
  geom_spatraster_rgb(data = bg_tiles_bus) +
  
  stat_density_2d_filled(aes(x = lon,
                             y = lat,
                             fill = after_stat(level),
                             weight = Stop_Avg_Daily_Boardings_Week),
                         alpha = 0.6,
                         bins = 15,
                         contour_var = "ndensity") +
  
  scale_fill_viridis_d(option = "rocket",
                       direction = -1,
                       labels = c("Low", rep("", 12), "High")) +
  
  facet_wrap(~TSPR_Year) +
  
  coord_sf(xlim = c(482000, 530000), 
           ylim = c(5428000, 5475000), 
           expand = FALSE) +
  
  theme_void() +
  
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold", margin = margin(b = 5)),
    legend.margin = margin(l = 10),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(1.5, "cm"),
    
    strip.text = element_text(face = "bold", size = 12, margin = margin(b = 5)),
    plot.title.position = "plot",
    plot.title    = element_text(hjust = 0, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0, size = 11, margin = margin(b = 10)),
    plot.caption  = element_text(hjust = 0, color = "grey40", size = 8),
    
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10),
    panel.spacing = unit(1, "lines")
  ) +
  
  guides(fill = guide_coloursteps(
    title = "Ridership Density",
    title.position = "top",
    title.hjust = 0.5,
    ticks = FALSE,
    frame.colour = "black",
    frame.linewidth = 0.2,
    show.limits = TRUE,
    direction = "vertical"
  )) +
  
  labs(title = "Metro Vancouver Bus Ridership Trends",
       subtitle = "Weighted Heatmap of Average Weekly Boardings (2019–2024)",
       caption = "Source: TransLink Open Data")

