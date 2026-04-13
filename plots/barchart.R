# Bar Chart
# Top 10 most "Connected" SkyTrain stations.

# Imports
library(ggplot2)
library(tidyverse)
library(scales)


connections <- read_csv("data/TransLink_SkyTrain_Stations.csv") |>
  group_by(Name) |>
  slice(1) |>  # keep only first row per station
  ungroup() |>
  mutate(
    Total_Bus_Connections = str_count(Route_Connections, ";") + 1,
    Total_Rail_SeaBus_Connections = str_count(
      str_remove(Rail_SeaBus_Connections, fixed(Line)),
      ";"
    ),
    Total_Connections = Total_Bus_Connections + Total_Rail_SeaBus_Connections
  ) |>
  slice_max(Total_Connections, n = 10) |>
  mutate(Name = str_remove(Name, " Station"),
         Name = fct_reorder(Name, Total_Connections, .desc = TRUE)) |>
  pivot_longer(
    cols = c(Total_Bus_Connections, Total_Rail_SeaBus_Connections),
    names_to = "Connection_Type",
    values_to = "Count"
  ) |>
  mutate(
    Connection_Type = recode(Connection_Type,
                             "Total_Bus_Connections" = "Bus",
                             "Total_Rail_SeaBus_Connections" = "Rail & SeaBus"
                             )
  )


ggplot(connections, aes(x = Name, y = Count, fill = Connection_Type)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 most connected SkyTrain stations",
    x = NULL,
    y = "Total connections",
    fill = "Connection type",
    caption = "Source: TransLink Open Data"
  )

