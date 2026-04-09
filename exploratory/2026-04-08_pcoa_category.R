

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
  left_join(whyte_category %>% select(var_id, var_id_whyte, title, category),
            by = "var_id_whyte")


# Join direction to variable loadings
var_loadings_corrected <- var_loadings %>%
  left_join(direction_table, by = "var_id") %>%
  mutate(across(
    starts_with("Axis."),
    ~ . * women_favorable
  ))

axis_category_summary <- var_loadings %>%
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

# Recompute category summaries with corrected loadings
axis_category_corrected <- var_loadings_corrected %>%
  pivot_longer(cols = starts_with("Axis."),
               names_to = "axis",
               values_to = "loading") %>%
  filter(axis %in% paste0("Axis.", 1:6)) %>%
  group_by(axis, category) %>%
  summarise(
    mean_loading = mean(loading, na.rm = TRUE),
    sd_loading   = sd(loading, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )


var_loadings %>%
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

