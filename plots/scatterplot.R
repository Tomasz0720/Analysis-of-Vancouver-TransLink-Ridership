# ScatterPlot
# Purpose: See if the density of jobs and people actually predicts how busy a station is.
# Population + Employment (Total Density) on the X-axis vs. Annual_Boardings on the Y-axis.

# Imports
library(ggplot2)
library(tidyverse)
library(scales)

density <- read_csv("data/TransLink_SkyTrain_Stations.csv") |>
  mutate(Pop_Emp = Population + Employment,
         Total_Bus_Connections = str_count(Route_Connections, ";") + 1) |>
  filter(!is.na(Pop_Emp))


Line_Colours <- c("Canada Line" = "#009ddc", "Expo/Millennium Line" = "#00355f", "West Coast Express" = "#820c8e")


ggplot(density, aes(x = Pop_Emp, y = Annual_Boardings, colour = Line, size = Total_Bus_Connections)) +
  
  geom_point(alpha = 0.6) +
  
  scale_x_continuous(
    labels = label_number(suffix = "K", scale = 1e-3),
    limits = c(0, NA),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6)) +
  
  geom_smooth(method = "lm", se = FALSE, aes(size = NULL, group = Line), linewidth = 0.8, show.legend = FALSE) +
  
  scale_size_continuous(range = c(2, 10), name = "Bus Connections") + 
  
  scale_colour_manual(values = Line_Colours) +
  
  guides(
    size = guide_legend(override.aes = list(colour = "grey40", fill = "grey40"))
  ) +
  
  labs(
    title = "Density vs. Station Success (2024)",
    subtitle = "Analyzing the relationship between local 'Jobs + People' and annual boardings",
    caption = "Source: TransLink Open Data",
    x = "Total Catchment Density (Population + Employment)",
    y = "Annual Boardings (Millions)"
  )

