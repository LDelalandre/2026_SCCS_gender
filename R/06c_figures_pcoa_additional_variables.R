

## Biplot pcoa ####

additional_response_vars <- c("SCCS793", "SCCS794", "SCCS795", # (Ross: political participation)
                              "SCCS667", # Sanday, rape
                              "SCCS754") # Wife-beating (Broude)
vars_bradley <- c("SCCS1271", "SCCS1272", "SCCS1273","SCCS1274", "SCCS1275")

additional_response_vars <- c(additional_response_vars, vars_bradley)

bradley_wide <- sccs %>% 
  filter(var_id %in% vars_bradley) %>% 
  filter(!is.na(code)) %>% 
  group_by(soc_id, var_id) %>%
  slice_max(year, na_rm = TRUE, n = 1) %>%
  ungroup() %>%
  dplyr::select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code) %>% 
  filter(!if_all(-soc_id, is.na))

bradley_soc <- bradley_wide$soc_id

intersect(bradley_soc, whyte_data_wide$soc_id)



sccs_vars <- sccs %>%
  filter(var_id %in% additional_response_vars) %>%
  select(soc_id, var_id, code)

sccs_loadings <- map_dfr(1:10, function(ax) {
  ax_col <- paste0("Ax", ax)
  
  map_dfr(additional_response_vars, function(v) {
    
    joined <- sccs_vars %>%
      filter(var_id == v) %>%
      inner_join(coords_df %>% select(soc_id, all_of(ax_col)),
                 by = "soc_id") %>%
      filter(!is.na(code))
    
    r <- cor(joined[[ax_col]], joined$code,
             method = "spearman", use = "complete.obs")
    
    data.frame(
      var_id     = v,
      pco          = paste0("PCo", ax),
      loading      = r,
      r2_this_axis = r^2,
      n_societies  = nrow(joined)
    )
  })
}) %>%
  group_by(var_id) %>%
  mutate(r2_total = sum(r2_this_axis, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct = factor(pco, levels = paste0("PCo", 1:10)))  # use pco column, not 1:10 vector


sccs_loadings %>% 
  left_join(variables %>% select(var_id, title), by = "var_id") 


plot_biplot <- function(ax1 = 1, ax2 = 2, loading_threshold = 0.3,
                        show_sites = FALSE, sccs_vars = NULL) {
  
  pco1 <- paste0("PCo", ax1)
  pco2 <- paste0("PCo", ax2)
  col1 <- paste0("Ax", ax1)
  col2 <- paste0("Ax", ax2)
  
  # Main arrows from loading_long_unfiltered
  vars_to_show <- loading_long_unfiltered %>%
    filter(pco %in% c(pco1, pco2)) %>%
    group_by(variable) %>%
    summarise(max_loading = max(abs(loading)), .groups = "drop") %>%
    filter(max_loading >= loading_threshold) %>%
    pull(variable)
  
  arrows_df <- loading_long_unfiltered %>%
    filter(pco %in% c(pco1, pco2), variable %in% vars_to_show) %>%
    select(variable, title, pco, loading) %>%
    pivot_wider(names_from = pco, values_from = loading) %>%
    rename(x = !!pco1, y = !!pco2) %>%
    mutate(title_trunc = str_wrap(as.character(title), width = 20),
           label       = paste0(variable, "\n", title_trunc))
  
  scale_factor <- max(abs(coords_df[[col1]]), abs(coords_df[[col2]])) /
    max(abs(c(arrows_df$x, arrows_df$y))) * 0.7
  
  arrows_df <- arrows_df %>%
    mutate(x = x * scale_factor, y = y * scale_factor)
  
  # SCCS arrows (optional)
  if (!is.null(sccs_vars)) {
    sccs_arrows_df <- sccs_loadings %>%
      filter(var_id %in% sccs_vars, pco %in% c(pco1, pco2)) %>%
      select(var_id, pco, loading) %>%
      pivot_wider(names_from = pco, values_from = loading) %>%
      rename(variable = var_id, x = !!pco1, y = !!pco2) %>%
      mutate(label = variable,
             x = x * scale_factor,
             y = y * scale_factor)
  }
  
  # Base plot
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.3)
  
  if (show_sites) {
    p <- p +
      geom_point(data = coords_df,
                 aes(x = .data[[col1]], y = .data[[col2]], color = region),
                 alpha = 0.7, size = 2) +
      scale_color_brewer(palette = "Set2", name = "Region")
  }
  
  # Main variable arrows (grey)
  p <- p +
    geom_segment(data = arrows_df,
                 aes(x = 0, y = 0, xend = x, yend = y),
                 arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
                 color = "grey30", linewidth = 0.4) +
    ggrepel::geom_label_repel(data = arrows_df,
                              aes(x = x, y = y,
                                  # label = paste0(variable, "\n",
                                  #                str_wrap(as.character(title), width = 20))),
                                  label =  str_wrap(as.character(title), width = 20)),
                              size          = 2.2,
                              color         = "grey10",
                              bg.color      = "white",
                              bg.r          = 0.08,
                              box.padding   = 0.4,
                              point.padding = 0.2,
                              max.overlaps  = 30,
                              segment.size  = 0.2,
                              segment.color = "grey60",
                              force         = 3)
  
  # SCCS variable arrows (colored)
  if (!is.null(sccs_vars)) {
    p <- p +
      geom_segment(data = sccs_arrows_df,
                   aes(x = 0, y = 0, xend = x, yend = y),
                   arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
                   color = "#cc0000", linewidth = 0.7) +
      ggrepel::geom_text_repel(data = sccs_arrows_df,
                               aes(x = x, y = y, label = label),
                               size = 2.5, color = "#cc0000", fontface = "bold",
                               bg.color = "white", bg.r = 0.08,
                               box.padding = 0.4, point.padding = 0.2,
                               max.overlaps = 30, segment.size = 0.3,
                               segment.color = "#cc0000", force = 3)
  }
  
  p +
    labs(
      x        = paste0(pco1, " (", round(variance_explained[ax1] * 100, 1), "%)"),
      y        = paste0(pco2, " (", round(variance_explained[ax2] * 100, 1), "%)"),
      title    = paste0("PCoA biplot — axes ", ax1, " and ", ax2),
      subtitle = paste0("Arrows show variables with |loading| ≥ ", loading_threshold,
                        " on ", pco1, " or ", pco2)
    ) +
    theme_minimal(base_size = 11) +
    coord_equal()
}


biplot <- plot_biplot(1, 2, loading_threshold = 0.4, sccs_vars = c("SCCS793", "SCCS667"))


ggsave(here::here("output/2.2_pcoa_figures_biplot.png"), biplot, height = 6, width = 6)

## Additional variables ####

# Assuming your data is in sccs_loadings with columns:
# var_id, pco, loading, r2_this_axis, n_societies, r2_total, title

# Step 1: clean short titles
sccs_loadings <- sccs_loadings %>%
  mutate(title_short = case_when(
    var_id == "SCCS793" ~ "Female public political participation",
    var_id == "SCCS794" ~ "Female private political participation",
    var_id == "SCCS795" ~ "Gender diff. in political authority",
    var_id == "SCCS667" ~ "Rape",
    var_id == "SCCS754" ~ "Wife-beating",
    var_id == "SCCS1271" ~ "Land clearance (who does it)",
    var_id == "SCCS1272" ~ "Soil preparation (who does it)",
    var_id == "SCCS1273" ~ "Planting (who does it)",
    var_id == "SCCS1274" ~ "Crop tending (who does it)",
    var_id == "SCCS1275" ~ "Harvesting (who does it)",
    TRUE ~ var_id
  )) %>%
  mutate(group = case_when(
    var_id %in% c("SCCS793", "SCCS794", "SCCS795") ~ "Political participation",
    var_id %in% c("SCCS667", "SCCS754")             ~ "Violence",
    var_id %in% paste0("SCCS", 1271:1275)           ~ "Agricultural tasks"
  ))

# Step 2: heatmap across all axes
sccs_loadings %>%
  filter(pco %in% paste0("PCo", 1:6)) %>%
  mutate(
    pco = factor(pco, levels = paste0("PCo", 1:6)),
    significant = abs(loading) > 0.30
  ) %>%
  ggplot(aes(x = pco, 
             y = reorder(title_short, -r2_total),
             fill = loading)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(significant, 
                               round(loading, 2), 
                               "")),
            size = 3) +
  scale_fill_gradient2(
    low = "tomato", mid = "white", high = "steelblue",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Spearman r"
  ) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  theme_bw(base_size = 10) +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0),
    strip.background = element_blank(),
    panel.spacing = unit(0.3, "lines")
  ) +
  labs(x = NULL, y = NULL,
       title = "Additional variables projected onto PCoA axes",
       subtitle = "Values shown where |r| > 0.30")

# Step 3: biplot projection on axes 1 and 2
sccs_loadings %>%
  filter(pco %in% c("PCo1", "PCo2")) %>%
  select(var_id, title_short, group, pco, loading) %>%
  pivot_wider(names_from = pco, values_from = loading) %>%
  ggplot(aes(x = PCo1, y = PCo2, 
             color = group, 
             label = title_short)) +
  geom_segment(aes(xend = 0, yend = 0),
               arrow = arrow(length = unit(0.2, "cm"),
                             ends = "first"),
               linewidth = 0.8) +
  geom_point(size = 2) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.4) +
  scale_color_brewer(palette = "Set2", direction = -1, name = "Group") +
  theme_bw(base_size = 10) +
  labs(x = "PCo1 (formal domination)",
       y = "PCo2 (work segregation)",
       title = "Additional variables projected on main PCoA axes")