


# Biplots ####
pred_resp %>%
  ggplot(aes(x = Axis.1, y = Axis.2)) +
  # geom_point(aes(color = complexity), size = 2.5, alpha = 0.8) +
  geom_point() +
  scale_color_gradient2(low = "#1A5276", mid = "white", high = "#B03A2E",
                        midpoint = 0, name = "Complexity") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  labs(x = "Axis 1", y = "Axis 2") +
  theme_minimal(base_size = 13) 
# ggrepel::geom_text_repel((aes(label = soc_id)))


# Subsistence ####

codes_SCCS820 <- codes %>% 
  filter(var_id == "SCCS820") %>% 
  select(code, description) %>% 
  rename(SCCS820 = code) %>% 
  mutate(SCCS820 = as.factor(SCCS820))

society_subsistence_type <- sccs %>% 
  filter(soc_id %in% pred_resp$soc_id) %>% 
  filter(var_id %in% c("SCCS820", "SCCS833")) %>%
  select(soc_id, var_id, code) %>% 
  pivot_wider(names_from = var_id, values_from = code) %>% 
  # filter(!SCCS820 == 8) %>% 
  mutate(SCCS820 = as.factor(SCCS820)) %>% 
  left_join(societies %>% select(soc_id, pref_name_for_society)) %>% 
  left_join(codes_SCCS820) %>% 
  mutate(label = paste(SCCS820, description, sep = "_")) 

society_subsistence_type %>% 
  group_by(SCCS820) %>% 
  summarize(n = n())

pcoa_biplot_subsistence <- pred_resp %>%
  left_join(society_subsistence_type, by = "soc_id") %>% 
  left_join(coords_df) %>% 
  filter(!str_detect(label, "Trade")) %>%  # remove trade panel
  ggplot(aes(x = Axis.1, y = Axis.2, shape = region)) +
  geom_point() +
  # ggrepel::geom_text_repel(aes(label = pref_name_for_society)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  labs(x = "Axis 1", y = "Axis 2") +
  theme_minimal(base_size = 13) +
  facet_wrap(~label) +
  scale_shape_manual(values = c(16, 17, 15, 18, 1, 2, 0, 5)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  theme(
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),  # or fill = "gray97"
    panel.spacing = unit(1.5, "lines")
  ) +
  theme(
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),
    panel.spacing = unit(1.5, "lines"),
    # strip.background = element_rect(fill = "gray20", color = NA),
    # strip.text = element_text(color = "white", face = "bold"),
    panel.grid.minor = element_blank()
  ) 

ggsave("draft/fig/pcoa_biplot_subsistence.png", pcoa_biplot_subsistence)


boxplot_subsistence_status <- pred_resp %>%
  left_join(society_subsistence_type) %>%
  filter(!str_detect(label, "Trade"), !is.na(label)) %>%
  mutate(SCCS820_num = as.numeric(as.character(SCCS820))) %>%
  add_count(label, name = "n") %>%
  mutate(
    label_n = paste0(label, " (n=", n, ")"),
    label_n = fct_rev(fct_reorder(label_n, SCCS820_num, .fun = min))
  ) %>%
  mutate(Axis.1_cr = (Axis.1 - mean(Axis.1))/sd(Axis.1) ) %>% 
  ggplot(aes(x = Axis.1_cr, y = label_n)) +
  geom_boxplot(outliers = F) +
  geom_point(size = 0.5) +
  # geom_jitter(height = 0.15, alpha = 0.6, size = 2) +
  # stat_summary(fun = median, geom = "point",
  #              shape = 124, size = 8, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(x = "Women's status gradient", y = NULL) +
  theme_minimal(base_size = 11)

ggsave("output/boxplot_subsistence_status.png",boxplot_subsistence_status, width = 5, height = 4)

# map societies ####
library(maps)

# Get world map data
world_map <- map_data("world")

# Create the map
map_societies <- ggplot() +
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
    aes(x = origLong, y = origLat), #, color = region
    size = 1,
    alpha = 0.8
  ) +
  # Add society labels
  geom_text(
    data = coords_df,
    aes(x = origLong, y = origLat, label = pref_name_for_society), #, color = region
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
    # legend.position = "bottom",
    # legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  ) +
  labs(
    title = "World Map of Societies",
    x = "Longitude",
    y = "Latitude",
    color = "Region"
  )

map_societies

ggsave("output/map-societies.png", map_societies)



