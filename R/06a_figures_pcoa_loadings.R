
# VARIABLE IMPORT AND DEFINITION ####

pcoa_result <-  readRDS(here::here("output/pcoa_result"))




## Correct direction: positive = good for women ####

direction_table <- read.csv("data/gender_response_variables_recode.csv") %>%
  select(var_id, title, women_favorable)

# Correlate original variables with society PCoA scores
var_loadings <- cor(
  mat_filtered,
  pred_resp %>% select(Axis.1:Axis.5),
  use = "pairwise.complete.obs"
) %>%
  as.data.frame() %>%
  rownames_to_column("var_id_whyte") %>%
  left_join(whyte_category %>% select(var_id, var_id_whyte, title, title_shortened, category),
            by = "var_id_whyte")


# Change the sign of the loading if it was coded as high = bad for women
var_loadings_corrected <- var_loadings %>%
  left_join(direction_table, by = "var_id") %>%
  mutate(across(starts_with("Axis."), ~ . * women_favorable))

var_order <- var_loadings_corrected %>%
  left_join(whyte_vars %>% select(var_id_whyte, category2),
            by = "var_id_whyte") %>%
  arrange(desc(category), category2, title_shortened) %>%
  pull(title_shortened) %>%
  unique()


# Scree plot ####
eigenvalues       <- pcoa_result$values$Eigenvalues
pos_eigen         <- eigenvalues[eigenvalues > 0]
# NB: negative eigenvalues represent distortion or misfit in the distance matrix — the degree to which your dissimilarity metric violates Euclidean geometry. 
variance_explained <- pos_eigen / sum(pos_eigen)

# 1. Compute broken stick expected values
n           <- length(pos_eigen)
total       <- sum(pos_eigen)

broken_stick_expected <- sapply(1:n, function(k){
  total * sum(1/(k:n)) / n
})

# 2. How many axes to keep
n_keep <- sum(pos_eigen > broken_stick_expected)

# 3. Plot
scree_df <- data.frame(
  axis         = 1:n,
  observed     = pos_eigen / total,
  broken_stick = broken_stick_expected / total
)

sum(scree_df$observed[1:7])

scree_plot <- ggplot(scree_df %>% filter(axis <= 15), aes(x = axis)) +
  geom_col(aes(y = observed), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = broken_stick), colour = "red", linewidth = 1) +
  geom_point(aes(y = broken_stick), colour = "red") +
  geom_vline(xintercept = n_keep + 0.5, 
             linetype = "dashed", colour = "darkred") +
  labs(x = "Axis", y = "Variance explained",
       title = "Broken stick model",
       subtitle = paste("Retain", n_keep, "axes (bars above red line)")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

ggsave("output/2.2_pcoa_figures_scree_plot.png", scree_plot)





# Loading plot ####

category_colors <- c(
  "Gender Ideology"       = "#66C2A5",
  "Gendered violence"     = "#FC8D62",
  "Politics"              = "#8DA0CB",
  "Family & Household"    = "#E78AC3",
  "Subsistence & Economy" = "#A6D854",
  "Sex & Marriage"        = "#FFD92F",
  "Religion & Rituals"    = "#E5C494"
)

# Step 1: build vars_plot with category_Angarika-based ordering
vars_plot <- var_loadings_corrected %>%
  left_join(whyte_vars %>% select(var_id_whyte, category2), by = "var_id_whyte") %>%
  left_join(whyte_category %>% select(var_id_whyte, category_Angarika)) %>%
  pivot_longer(cols = c(Axis.1, Axis.2, Axis.3, Axis.4, Axis.5),
               names_to = "axis", values_to = "loading") %>%
  filter(!is.na(loading)) %>%
  mutate(
    significant = abs(loading) > 0.30,
    category_Angarika = factor(category_Angarika, levels = names(category_colors))
  ) %>%
  arrange(category_Angarika, title_shortened) %>%
  mutate(title_shortened = factor(title_shortened, levels = unique(title_shortened)))

# Step 2: y-axis label colors
y_axis_colors <- vars_plot %>%
  distinct(title_shortened, category_Angarika) %>%
  mutate(title_shortened = factor(title_shortened,
                                  levels = levels(vars_plot$title_shortened))) %>%
  arrange(title_shortened) %>%
  mutate(color = category_colors[as.character(category_Angarika)]) %>%
  pull(color)

# Step 3: plot
loadings_plot <- ggplot(vars_plot, aes(x = loading,
                                       y = title_shortened,
                                       fill = category_Angarika,
                                       alpha = significant)) +
  geom_col() +
  geom_vline(xintercept = c(-0.30, 0.30),
             linetype = "dotted", color = "grey40", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  # facet_wrap(~ axis, nrow = 1) +
  scale_fill_manual(values = category_colors, name = "Domain") +
  scale_alpha_manual(values = c("FALSE" = 0.25, "TRUE" = 1), guide = "none") +
  scale_y_discrete(limits = rev) +
  theme_bw(base_size = 9) +
  theme(
    axis.text.y = element_text(size = 7, color = rev(y_axis_colors)),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none",
    panel.spacing = unit(1, "lines")
  ) +
  labs(x = "Loading (corrected for coding direction)", y = NULL)

# Step 4: standalone legend
legend_plot <- ggplot(
  data.frame(category = factor(names(category_colors), levels = names(category_colors))),
  aes(x = 1, y = category, fill = category)
) +
  geom_tile() +
  scale_fill_manual(values = category_colors) +
  theme(legend.position = "right")

standalone_legend <- cowplot::get_legend(legend_plot)

loadings_plot2 <- cowplot::plot_grid(
  standalone_legend,
  loadings_plot,
  rel_widths = c(0.4, 1)
)
loadings_plot2

ggsave("draft/fig/appendix_loadings_plot_full.png", loadings_plot2,
       width = 17, height = 6)
ggsave("draft/fig/loadings_plot.png", loadings_plot2,
       width = 8, height = 6)

