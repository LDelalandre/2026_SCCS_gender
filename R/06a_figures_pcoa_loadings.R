
# VARIABLE IMPORT AND DEFINITION ####

pcoa_result <-  readRDS(here::here("output/pcoa_result"))

perm_long <- read.csv("output/02c_perm_long.csv")

df_perm_mean_loadings <- read.csv("output/02C_df_perm_mean_loadings.csv")


## Correct direction: positive = good for women ####

# direction_table <- read.csv("data/gender_response_variables_recode.csv") %>%
#   select(var_id, title, women_favorable)

# Correlate original variables with society PCoA scores
var_loadings <- cor(
  mat_filtered,
  response_axes %>% select(Axis.1:Axis.5),
  use = "pairwise.complete.obs"
) %>%
  as.data.frame() %>%
  rownames_to_column("var_id") %>%
  left_join(whyte_category %>% select(var_id, var_id_whyte, title, title_shortened, category_Angarika),
            by = "var_id")


# Change the sign of the loading if it was coded as high = bad for women
# var_loadings_corrected <- var_loadings %>%
#   left_join(direction_table, by = "var_id") %>%
#   mutate(across(starts_with("Axis."), ~ . * women_favorable))

var_order <- var_loadings %>%
  arrange(desc(category_Angarika), title_shortened) %>%
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
vars_plot <- var_loadings %>%
  # left_join(whyte_vars %>% select(var_id_whyte, category2), by = "var_id_whyte") %>%
  left_join(whyte_category_angarika %>% select(var_id_whyte, category_Angarika)) %>%
  pivot_longer(cols = c(Axis.1, Axis.2, Axis.3, Axis.4, Axis.5),
               names_to = "axis", values_to = "loading") %>%
  filter(!is.na(loading)) %>%
  mutate(significant = if_else(var_id %in% nvar_significant, T, F)) %>% 
  mutate(
    # significant = abs(loading) > 0.30, # OR WHEN REALLY SIGNIFICANT
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

# # Step 3: plot
# loadings_plot <- ggplot(vars_plot, aes(x = loading,
#                                        y = title_shortened,
#                                        fill = category_Angarika,
#                                        alpha = significant)) +
#   geom_col() +
#   geom_vline(xintercept = c(-0.30, 0.30),
#              linetype = "dotted", color = "grey40", alpha = 0.7) +
#   geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
#   facet_wrap(~ axis, nrow = 1) +
#   scale_fill_manual(values = category_colors, name = "Domain") +
#   scale_alpha_manual(values = c("FALSE" = 0.25, "TRUE" = 1), guide = "none") +
#   scale_y_discrete(limits = rev) +
#   theme_bw(base_size = 9) +
#   theme(
#     axis.text.y = element_text(size = 7, color = rev(y_axis_colors)),
#     strip.background = element_blank(),
#     strip.text = element_text(face = "bold", size = 10),
#     legend.position = "none",
#     panel.spacing = unit(1, "lines")
#   ) +
#   labs(x = "Loading (corrected for coding direction)", y = NULL)

# Standalone legend
legend_plot <- ggplot(
  data.frame(category = factor(names(category_colors), levels = names(category_colors))),
  aes(x = 1, y = category, fill = category)
) +
  geom_tile() +
  scale_fill_manual(values = category_colors) +
  theme(legend.position = "right")

standalone_legend <- cowplot::get_legend(legend_plot)

make_loading_plot <- function(axes = c("Axis.1", "Axis.2", "Axis.3", 
                                       "Axis.4", "Axis.5"),
                              ncol_facet = length(axes), significance = FALSE) {
  
  data_filtered <- vars_plot %>%
    filter(axis %in% axes) %>%
    mutate(axis = factor(axis, levels = axes))
  
  if (significance) {
    p <- ggplot(data_filtered, aes(x = loading, y = title_shortened,
                                   fill = category_Angarika, alpha = significant))
  } else {
    p <- ggplot(data_filtered, aes(x = loading, y = title_shortened,
                                   fill = category_Angarika))
  }
  
  p <- p +
    geom_col() +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
    scale_fill_manual(values = category_colors, name = "Domain") +
    scale_y_discrete(limits = rev) +
    theme_bw(base_size = 9) +
    theme(
      axis.text.y    = element_text(size = 7, color = rev(y_axis_colors)),
      strip.background = element_blank(),
      strip.text     = element_text(face = "bold", size = 10),
      legend.position = "none",
      panel.spacing  = unit(1, "lines")
    ) +
    labs(x = "Loading (corrected for coding direction)", y = NULL)
  
  if (significance) {
    p <- p + scale_alpha_manual(values = c("FALSE" = 0.25, "TRUE" = 1), guide = "none")
  }
  
  if (length(axes) > 1) {
    p <- p + facet_wrap(~ axis, nrow = 1)
  }
  
  list(standalone_legend, p)
}

# Axis 1 only
loadings_plot <- make_loading_plot("Axis.1", significance = T)
loadings_plot[[1]] %>% plot
loadings_plot[[2]]
loadings_plot_legend <- cowplot::plot_grid(loadings_plot[[1]], loadings_plot[[2]], rel_widths = c(0.4, 1))

# All axes
loadings_plot_full <- make_loading_plot()
cowplot::plot_grid(loadings_plot_full[[1]], loadings_plot_full[[2]], rel_widths = c(0.4, 1))

# Custom subset
make_loading_plot(c("Axis.1", "Axis.2"))

ggsave("draft/fig/loadings_plot_full.png", loadings_plot_full,
       width = 17, height = 13, units = "cm")


ggsave("draft/fig/loadings_plot.png", loadings_plot_legend,
       width = 8, height = 6)


## Plus permutation results ####

appendix_pcoa_permutation_variance_axis1 <- ggplot(perm_long %>% filter(axis == "PCo1"), 
                                                   aes(x = variance * 100)) +
  geom_histogram(fill = "grey70", color = "white", bins = 40) +
  geom_vline(data = obs_df %>% filter(axis == "PCo1"), 
             aes(xintercept = observed), color = "red", linewidth = 0.8) +
  # facet_wrap(~ axis, scales = "free_x") +
  labs(x        = "Variance explained \n by Axis 1(%)",
       y        = "Count") +
  theme_classic(base_size = 11)


appendix_pcoa_mean_loading_axis1 <- ggplot(df_perm_mean_loadings, aes(x = Ax1)) +
  geom_histogram(fill = "grey70", color = "white", bins = 40) +
  geom_vline(xintercept = obs_mean_loading, color = "red", linewidth = 0.8) +
  labs(x        = "Mean variable loading \n on Axis 1",
       y        = "Count"
       # title    = "Is the positive shift in PCo1 loadings beyond chance?",
       # subtitle = paste0("Observed mean = ", round(obs_mean_loading, 3),
       #                   " — permutation p = ", round(p_mean, 3))
  ) +
  theme_classic(base_size = 11)

A <- loadings_plot[[2]] +
  ggtitle("A.")

B <- appendix_pcoa_permutation_variance_axis1 +
  ggtitle("B.") 
  # theme(plot.title = element_text(size = 30)) # , face = "bold"
C <- appendix_pcoa_mean_loading_axis1 +
  ggtitle("C.")

legend <- loadings_plot[[1]]
legend_wrapped <- wrap_elements(full = legend)

layout <- "
AB
AC
AD
"
plot_loadings_permutation <- A + B + C  + legend_wrapped +
  plot_layout(design = layout, 
              widths = c(1,0.4), 
              heights = c(0.1,0.1, 0.2))
plot_loadings_permutation

ggsave("draft/fig/plot_loadings_permutation.png",
       plot_loadings_permutation,
       width = 18, height = 14, units = "cm",   # match journal spec exactly
       dpi = 300)


# Loading boxplot ####

make_loading_boxplot <- function(axes = c("Axis.1", "Axis.2", "Axis.3",
                                          "Axis.4", "Axis.5"),
                                 ncol_facet = length(axes)) {
  
  p <- vars_plot %>%
    filter(axis %in% axes) %>%
    mutate(axis = factor(axis, levels = axes)) %>%
    ggplot(aes(x = loading, y = category_Angarika, fill = category_Angarika)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.5, width = 0.6) +
    geom_jitter(aes(alpha = significant),
                height = 0.15, width = 0,
                shape = 21, size = 1.6, color = "black", stroke = 0.2) +
    geom_vline(xintercept = c(-0.30, 0.30),
               linetype = "dotted", color = "grey40", alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
    scale_fill_manual(values = category_colors, name = "Domain") +
    scale_alpha_manual(values = c("FALSE" = 0.35, "TRUE" = 1), guide = "none") +
    scale_y_discrete(limits = rev(names(category_colors))) +
    theme_bw(base_size = 9) +
    theme(
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", size = 10),
      legend.position   = "none",
      panel.spacing     = unit(1, "lines")
    ) +
    labs(x = "Loading (corrected for coding direction)", y = NULL)
  
  if (length(axes) > 1) {
    p <- p + facet_wrap(~ axis, nrow = 1)
  }
  p
  # cowplot::plot_grid(standalone_legend, p, rel_widths = c(0.4, 1))
}


make_loading_boxplot(axes = c("Axis.1"), ncol_facet = 1)
