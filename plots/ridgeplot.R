# Ridge Plot
# Purpose: Show the distribution of the weekend_ratio (your new column) for each Municipality.

install.packages("ggridges")

library(ggplot2)
library(ggridges)
library(tidyverse)

bus_ridership <- read_csv("data/TransLink_Bus_Stop_Ridership_2019_2024.csv") |>
  filter(TSPR_Year == 2024) |>
  mutate(
    weekend_avg = (Stop_Avg_Daily_Boardings_Sat + Stop_Avg_Daily_Boardings_SunHol) / 2,
    weekend_ratio = weekend_avg / Stop_Avg_Daily_Boardings_MF
  ) |>
  filter(Stop_Avg_Daily_Boardings_MF > 0 & !is.na(weekend_ratio)) |>
  filter(!is.na(Municipality)) |>
  mutate(Municipality = fct_reorder(Municipality, weekend_ratio, .fun = median))



ggplot(bus_ridership, aes(x = weekend_ratio, y = Municipality, fill = Municipality)) +
  
  geom_density_ridges(alpha = 0.7, scale = 2, rel_min_height = 0.01) +
  
  theme_ridges() +
  
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "red") +
  
  labs(
    title = "Transit Usage Patterns by Municipality (2024)",
    subtitle = "Higher ratios indicate higher weekend demand relative to weekday commuting",
    x = "Weekend-to-Weekday Ridership Ratio",
    y = NULL
  ) +
  
  theme(legend.position = "none") +
  
  coord_cartesian(xlim = c(0,2))

