

# Correct direction: positive = good for women ####

# For each variable, find the description of the maximum code
# This tells you what "high score" means

max_code_descriptions <- codes %>%
  filter(var_id %in% whyte_category$var_id) %>%
  filter(!is.na(code)) %>%
  group_by(var_id, title) %>%
  slice_max(code, n = 1) %>%
  ungroup() %>%
  select(var_id, title, max_code = code, max_description = description) %>%
  left_join(whyte_category %>% select(var_id, category),
            by = "var_id") %>%
  arrange(category) %>%
  select(var_id, category, title, max_code, max_description) 

write.csv(max_code_descriptions, "output/2026-04-08_pcoa_category_max_code_descriptions.csv", row.names = F)

direction_table <- read.csv("output/2026-04-08_pcoa_category_max_code_descriptions_edited.csv") %>% 
  select(var_id, women_favorable)

# Correlate original variables with society PCoA scores
# This gives you loadings: how much each variable contributes to each axis
var_loadings <- cor(
  mat_filtered,
  pred_resp %>% select(Axis.1:Axis.10),
  use = "pairwise.complete.obs"
) %>%
  as.data.frame() %>%
  rownames_to_column("var_id_whyte") %>%
  left_join(whyte_category %>% select(var_id, var_id_whyte, title, title_shortened, category),
            by = "var_id_whyte")


# Join direction to variable loadings
var_loadings_corrected <- var_loadings %>%
  left_join(direction_table, by = "var_id") %>%
  mutate(across(
    starts_with("Axis."),
    ~ . * women_favorable
  ))

var_loadings_corrected <- var_loadings_corrected %>%
  mutate(title_short = case_when(
    str_detect(title, "Solely by Men")   ~ "Control fruits: men's labour",
    str_detect(title, "by Men and Women")   ~ "Control fruits: joint labour",
    str_detect(title, "Solely by Women") ~ "Control fruits: women's labour",
    str_detect(title, "Final Authority.*Infant") ~ "Authority: infants",
    str_detect(title, "Final Authority.*Post") ~ "Authority: older children",
    str_detect(title, "Double Standard.*Premarital") ~ "No double standard: premarital sex",
    str_detect(title, "Double Standard.*Extramarital") ~ "No double standard: extramarital sex",
    str_detect(title, "Explicitly Male Work") ~ "Male work groups",
    str_detect(title, "Explicitly Female Work") ~ "Female work groups",
    TRUE ~ str_trunc(title, 45)
  ))


var_loadings_corrected %>%
  pivot_longer(cols = starts_with("Axis."),
               names_to = "axis",
               values_to = "loading") %>%
  mutate(axis = factor(axis, 
                       levels = paste0("Axis.", 1:10))) %>%
  filter(axis %in% paste0("Axis.", 1:5)) %>%  # first 5 axes
  ggplot(aes(x = category, y = loading, fill = category)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ axis, nrow = 1) +
  coord_flip() +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = NULL, y = "Loading on axis",
       title = "Variable categories by PCoA axis") +
  theme(legend.position = "none")


axis_category_summary <- var_loadings_corrected %>%
  pivot_longer(cols = starts_with("Axis."),
               names_to = "axis",
               values_to = "loading") %>%
  filter(axis %in% paste0("Axis.", 1:6)) %>%
  group_by(axis, category) %>%
  summarise(
    mean_loading = mean(loading, na.rm = TRUE),
    mean_abs_loading = mean(abs(loading), na.rm = TRUE),
    sd_loading = sd(loading, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

axis_category_summary %>% 
  select(-c(mean_abs_loading, sd_loading, n)) %>%  
  pivot_wider(names_from = axis, values_from = mean_loading)

axis_category_summary %>%
  filter(axis %in% paste0("Axis.", 1:6)) %>%
  ggplot(aes(x = axis, y = category, fill = mean_loading)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(mean_loading, 2),
                color = abs(mean_loading) > 0.1),
            size = 3.5) +
  scale_color_manual(values = c("grey60", "black"), guide = "none") +
  scale_fill_gradient2(
    low = "tomato", mid = "white", high = "steelblue",
    midpoint = 0,
    limits = c(-0.3, 0.3)
  ) +
  theme_bw() +
  labs(x = NULL, y = NULL,
       fill = "Mean loading",
       title = "Mean signed loading by category and axis",
       subtitle = "Positive = female-favorable pole; Negative = male-dominant pole")


var_order <- var_loadings_corrected %>%
  left_join(whyte_vars %>% select(var_id_whyte, category2),
            by = "var_id_whyte") %>%
  arrange(desc(category), category2, title_shortened) %>%
  pull(title_shortened) %>%
  unique()


vars_plot <- var_loadings_corrected %>%
  left_join(whyte_vars %>% select(var_id_whyte, category2),
            by = "var_id_whyte") %>%
  pivot_longer(cols = c(Axis.1, Axis.2, Axis.4),
               names_to = "axis",
               values_to = "loading") %>%
  filter(!is.na(loading)) %>%
  mutate(
    significant = abs(loading) > 0.30,
    title_shortened = factor(title_shortened, levels = var_order),
    category = factor(category, levels = c(
      "domination",
      "power",
      "sexual_autonomy",
      "role_material",
      "role_symbolic"
    ))
  )



# Plot ####

# Step 1: define colors once
category_colors <- c(
  "domination"      = "#66C2A5",
  "power"           = "#FC8D62", 
  "sexual_autonomy" = "#8DA0CB",
  "role_material"   = "#E78AC3",
  "role_symbolic"   = "#A6D854"
)

# Step 2: for each label in y axis order, what color should it be?
# ggplot draws y axis from bottom to top = factor levels in order
# axis.text.y color vector must match that order exactly
y_axis_colors <- vars_plot %>%
  distinct(title_shortened, category) %>%
  mutate(title_shortened = factor(title_shortened, 
                                  levels = levels(vars_plot$title_shortened))) %>%
  arrange(title_shortened) %>%        # same order ggplot uses internally
  mutate(color = category_colors[as.character(category)]) %>%
  pull(color)

# Step 3: plot
loadings_plot <- ggplot(vars_plot, aes(x = loading,
                      y = title_shortened,
                      fill = category,
                      alpha = significant)) +
  geom_col() +
  geom_vline(xintercept = c(-0.30, 0.30),
             linetype = "dotted", color = "grey40", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  facet_wrap(~ axis, nrow = 1) +
  scale_fill_manual(values = category_colors, name = "Domain") +
  scale_alpha_manual(values = c("FALSE" = 0.25, "TRUE" = 1), guide = "none") +
  theme_bw(base_size = 9) +
  theme(
    axis.text.y = element_text(size = 7, color = y_axis_colors),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  labs(x = "Loading (corrected for coding direction)", y = NULL)


loadings_plot

ggsave("output/2026-04-08_pcoa_category_loadings_plot.png", loadings_plot,
       width = 9, height = 6)

