library(ggplot2)
library(maps)

# Get world map data
world_map <- map_data("world")

# Create the map
ggplot() +
  # Draw world map base
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "lightgray",
    color = "white",
    linewidth = 0.2
  ) +
  # Add society points
  geom_point(
    data = coords_df,
    aes(x = origLong, y = origLat, color = region),
    size = 2,
    alpha = 0.8
  ) +
  # Add society labels
  geom_text(
    data = coords_df,
    aes(x = origLong, y = origLat, label = pref_name_for_society, color = region),
    size = 2.2,
    vjust = -0.8,
    hjust = 0.5,
    check_overlap = TRUE
  ) +
  # Coordinate system
  coord_fixed(1.3, xlim = c(-180, 180), ylim = c(-90, 90)) +
  # Theme and labels
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue", color = NA),
    panel.grid = element_line(color = "white", linewidth = 0.3),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  ) +
  labs(
    title = "World Map of Societies",
    x = "Longitude",
    y = "Latitude",
    color = "Region"
  )