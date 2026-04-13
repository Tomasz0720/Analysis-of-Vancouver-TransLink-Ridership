# Imports

library(ggplot2)
library(sf)
library(maptiles)
library(tidyterra)
library(scales)
library(dplyr)

# Bar Graph
# Purpose: Show on time % for different bus types (artic, standard, mini, trolley, etc).

bus <- read.csv("data/TransLink_Bus_Line_With_Connections.csv")


bus_summary <- bus |>
  mutate(On_Time_Performance_Percentage = as.numeric(gsub("%", "", On_Time_Performance_Percentage))) |>
  filter(Predominant_Vehicle_Type != "SeaBus") |>
  group_by(Predominant_Vehicle_Type) |>
  summarize(Mean_Performance = mean(On_Time_Performance_Percentage, na.rm = TRUE)) |>
  mutate(Predominant_Vehicle_Type = reorder(Predominant_Vehicle_Type, Mean_Performance))


View(bus_summary)


ggplot(bus_summary, aes(x = Mean_Performance, y = Predominant_Vehicle_Type)) +

  geom_segment(aes(x = 70, xend = Mean_Performance, yend = Predominant_Vehicle_Type), 
               colour = "grey50", size = 0.8) +
  
  geom_point(size = 5, colour = "steelblue") +
  
  geom_text(aes(label = paste0(round(Mean_Performance, 1), "%")), 
            hjust = -0.5, size = 3.5, fontface = "bold") +
  
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(70, 88)) +
  
  labs(
    title = "On-Time Performance by Vehicle Type",
    subtitle = "Average performance across all TransLink bus routes",
    x = "Mean On-Time Performance",
    y = NULL,
    caption = "Data: TransLink Open Data"
  ) +
  
  theme_minimal() +
  
  theme(panel.grid.major.y = element_blank())

