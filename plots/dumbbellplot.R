# Dumbbell Plot
# Show one dot for 2019 ridership and one dot for 2024 ridership, connected by a line, for the Top 15 busiest bus routes.

# Imports
library(ggplot2)
library(tidyverse)
library(scales)

ridership <- read_csv("data/TransLink_Bus_Line_With_Connections.csv") |>
  filter(TSPR_Year %in% c(2019, 2024)) |>
  select(Line, Line_Name, TSPR_Year, Annual_Boardings) |>
  group_by(Line, TSPR_Year) |>
  summarise(
    Annual_Boardings = sum(Annual_Boardings),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = TSPR_Year,
    values_from = Annual_Boardings,
    names_prefix = "y"
  ) |>
  drop_na(y2019, y2024) |>
  slice_max(y2019, n = 15) |>
  left_join(
    read_csv("data/TransLink_Bus_Line_With_Connections.csv") |>
      filter(TSPR_Year == 2019) |>
      distinct(Line, Line_Name),
    by = "Line"
  )


ggplot(ridership, aes(y = fct_reorder(Line_Name, y2019))) +

  geom_segment(aes(x = y2019, xend = y2024, yend = Line_Name), 
               color = "#e0e0e0", size = 1.5) +

  geom_point(aes(x = y2019), color = "#0881c4", size = 4) +

  geom_point(aes(x = y2024), color = "#fdc427", size = 4) +

  scale_x_continuous(labels = label_number(suffix = "M", scale = 1e-6)) +
  
  theme_minimal() +
  
  labs(
    title = "Vancouver Bus Ridership Still Below Pre-Pandemic Levels",
    subtitle = "Annual boardings on TransLink's 15 busiest routes, 2019 (yellow) vs. 2024 (blue)",
    caption = "Source: TransLink Open Data",
    x = "Annual Boardings (Millions)",
    y = NULL
  )

