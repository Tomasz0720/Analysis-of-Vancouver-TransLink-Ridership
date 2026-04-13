# Bubble Map
# Purpose: Show ridership by SkyTrain station based on how big the radius of the circle is.

# Imports
if (!require("maptiles")) install.packages("maptiles")
if (!require("tidyterra")) install.packages("tidyterra")
if (!require("dplyr")) install.packages("dplyr")
if (!require("sf")) install.packages("sf")

library(ggplot2)
library(sf)
library(maptiles)
library(tidyterra)
library(scales)


stations_sf <- st_read("data/TransLink_SkyTrain_Stations_Geo/Rail_Station.shp") |>
  st_transform(crs = 4326)

lines_sf <- st_read("data/TransLink_SkyTrain_Lines_Geo/Rail_Line.shp") |>
  st_transform(crs = 4326)

line_Colours <- c("Canada Line" = "#009ddc", "Expo/Millennium Line" = "#00355f", "West Coast Express" = "#820c8e")

View(lines_sf)

# Automatically figure out the area based on stations
bg_tiles <- get_tiles(stations_sf, provider = "CartoDB.Positron", zoom = 11)

ggplot() +
  # Add background tiles
  geom_spatraster_rgb(data = bg_tiles) +
  
  geom_sf(data = stations_sf,
          aes(size = Annual_Boa),
          fill = "#003366",
          colour = "white",
          stroke = 0.5,
          alpha = 0.5,
          shape = 21) +
  
  geom_sf(data = lines_sf,
          aes(colour = Line),
          linewidth = 1,
          show.legend = "line") +
  
  scale_colour_manual(values = line_Colours,
                      name = "Line") +
  
  scale_size_area(max_size = 20,
                  breaks = c(1e6, 5e6, 10e6, 15e6, 20e6),
                  limits = c(0, 25000000),
                  labels = label_comma(),
                  name = "Annual Ridership",
                  guide = guide_legend(
                    override.aes = list(colour = "#003366",  # match your fill
                                        linetype = 0)        # removes the line
                  )) +

  theme_void() +
  
  theme(
    plot.title.position = "plot",    # aligns title to the plot panel edge, not the figure
    plot.caption.position = "plot",  # same fix for the caption
    plot.title    = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0),
    plot.caption  = element_text(hjust = 0),
    plot.margin = margin(t = 5, r = 10, b = 10, l = 0)
    ) +

  labs(title = "Metro Vancouver SkyTrain Ridership",
       subtitle = "Annual Station Intensity (2024)",
       caption = "Source: TransLink Open Data")

