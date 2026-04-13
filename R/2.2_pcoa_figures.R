

pcoa_result <-  readRDS(here::here("output/pcoa_result"))

coords_df <- as.data.frame(pcoa_result$vectors[, 1:10])
colnames(coords_df) <- paste0("Ax", 1:10)
coords_df$soc_id <- rownames(coords_df)

coords_df <- coords_df %>%
  left_join(societies %>% select(soc_id, pref_name_for_society, 
                                 origLat, origLong),
            by = "soc_id")

# ── Derive broad region from coordinates ─────────────────────────────────────

coords_df <- coords_df %>%
  mutate(region = case_when(
    # Africa
    origLong >= -20  & origLong <= 55  & origLat >= -35 & origLat <= 38  ~ "Africa",
    # Europe
    origLong >= -12  & origLong <= 45  & origLat >  38  & origLat <= 72  ~ "Europe",
    # Middle East
    origLong >= 25   & origLong <= 65  & origLat >= 10  & origLat <= 43  ~ "Middle East",
    # South & SE Asia
    origLong >= 65   & origLong <= 145 & origLat >= -12 & origLat <= 38  ~ "Asia",
    # East Asia
    origLong >= 100  & origLong <= 145 & origLat >= 38  & origLat <= 55  ~ "East Asia",
    # Pacific & Oceania
    origLong >= 130  & origLat  < 0                                       ~ "Pacific",
    origLong >= 145  & origLat  >= 0  & origLat <= 30                    ~ "Pacific",
    # North America
    origLong <= -50  & origLat  >= 15                                     ~ "N. America",
    # South America
    origLong <= -35  & origLat  < 15                                      ~ "S. America",
    TRUE ~ "Other"
  ))

# Check the distribution
table(coords_df$region)




## Scree plot ####
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

## Table variables per axes ####

# Society coordinates on PCoA axes
coords <- pcoa_result$vectors  # societies × axes

# Correlate each variable with each axis (Spearman, handles ordinal)
n_axes <- 10  # however many you decided to keep

variable_loadings <- matrix(NA, 
                            nrow = ncol(mat_filtered), 
                            ncol = n_axes,
                            dimnames = list(colnames(mat_filtered),
                                            paste0("PCo", 1:n_axes)))

for(i in 1:n_axes){
  variable_loadings[, i] <- apply(mat_filtered, 2, function(x){
    cor(x, coords[, i],
        method = "spearman",
        use    = "pairwise.complete.obs")
  })
}

# R-squared of each variable across all axes
r2_total <- apply(variable_loadings[, 1:n_axes], 1, function(x){
  sum(x^2, na.rm = TRUE)
})

# How much of each variable is captured by your 10 axes?
r2_distrib <- data.frame(
  variable  = names(r2_total),
  r2_total  = round(r2_total, 3)
) %>%
  left_join(whyte_vars %>% select(var_id_whyte, short_title, domain),
            by = c("variable" = "var_id_whyte")) %>%
  arrange(desc(r2_total))
# Variables of axis 1
# filter(variable %in% c("DV5", "DV15", "DV20", "DV34", "DV36", "DV40", "DV50", "DV51")) %>% 


ggplot(r2_distrib, aes(x = r2_total)) + 
  geom_histogram() +
  ggtitle("r2 in the variable explained by all the axes")

# Specificity of each variable to each axis
r2_matrix <- variable_loadings[, 1:10]^2

# For each variable, what proportion of its explained variance is on each axis?
r2_proportion <- sweep(r2_matrix, 1, rowSums(r2_matrix, na.rm = TRUE), "/")

# Variables most specific to axis 1
r2_total_axis <- data.frame(
  variable     = rownames(r2_proportion),
  r2_axis1     = round(r2_matrix[, 1], 3),
  r2_total     = round(rowSums(r2_matrix, na.rm = TRUE), 3),
  pct_on_axis1 = round(r2_proportion[, 1] * 100, 1)
) %>%
  left_join(whyte_vars %>% select(var_id_whyte, short_title),
            by = c("variable" = "var_id_whyte")) %>%
  arrange(desc(pct_on_axis1)) %>%
  head(15) 



# For each axis, show variables where:
# 1. r2_total >= 0.3 (variable is reasonably captured overall)
# 2. pct_on_this_axis >= 30% (this axis is its primary home)
# 3. Sorted by absolute loading (effect size)

library(knitr)
library(kableExtra)

# ── Tune these ────────────────────────────────────────────────────────────────
r2_threshold  <- 0.3
pct_threshold <- 30

# ── Function ──────────────────────────────────────────────────────────────────
interpret_axis <- function(ax, r2_min = r2_threshold, pct_min = pct_threshold){
  data.frame(
    variable     = rownames(r2_matrix),
    loading      = variable_loadings[, ax],
    r2_total     = rowSums(r2_matrix, na.rm = TRUE),
    r2_this_axis = r2_matrix[, ax]
  ) %>%
    mutate(pct_this_axis = r2_this_axis / r2_total * 100) %>%
    filter(r2_total >= r2_min, pct_this_axis >= pct_min) %>%
    left_join(whyte_vars %>% select(var_id_whyte, title, category1, category2),
              by = c("variable" = "var_id_whyte")) %>%
    arrange(desc(abs(loading))) %>%
    select(variable, title, category1, category2, loading, r2_total, pct_this_axis) %>%
    mutate(across(where(is.numeric), round, 2))
}

# ── Build table ───────────────────────────────────────────────────────────────
all_axes_table <- map_dfr(1:10, function(ax){
  interpret_axis(ax) %>%
    mutate(pco = ax, .before = everything())
})


# ── Render ────────────────────────────────────────────────────────────────────
all_axes_table %>%
  kable(
    caption   = "Variable loadings across PCoA axes",
    col.names = c("PCo", "Variable", "Title", "Category1" , "Category2", 
                  "Loading", "R² total", "% this axis"),
    align     = c("c", "l", "l", "l", "l", "r", "r", "r")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width        = FALSE,
    position          = "left"
  ) %>%
  column_spec(3, width = "8cm") %>%
  column_spec(6, 
              color = ifelse(all_axes_table$loading > 0, 
                             "#1a7a1a", "#cc0000"), 
              bold  = TRUE) %>%
  pack_rows(
    index = setNames(
      as.vector(table(all_axes_table$pco)),
      paste0("PCo ", names(table(all_axes_table$pco)))
    )
  ) %>%
  row_spec(0, bold = TRUE, background = "#f0f0f0")


## Heatmap loading ####
loading_long_unfiltered <- map_dfr(1:10, function(ax) {
  data.frame(
    variable     = rownames(r2_matrix),
    loading      = variable_loadings[, ax],
    r2_total     = rowSums(r2_matrix, na.rm = TRUE),
    r2_this_axis = r2_matrix[, ax]
  ) %>%
    mutate(pco = paste0("PCo", ax))
}) %>%
  left_join(whyte_vars %>% select(var_id_whyte, title, domain),
            by = c("variable" = "var_id_whyte")) %>%
  left_join(
    data.frame(
      pco = paste0("PCo", 1:10),
      pct = paste0("PCo", 1:10, "\n(", round(variance_explained[1:10]*100, 1), "%)")
    ),
    by = "pco"
  ) %>%
  mutate(pct = factor(pct, levels = paste0("PCo", 1:10, "\n(", round(variance_explained[1:10]*100, 1), "%)"))) %>%
  mutate(title = reorder(title, as.numeric(factor(variable))))


# Create ordered y-axis labels
loading_long_unfiltered <- loading_long_unfiltered %>%
  arrange(variable) %>%
  mutate(
    var_label = paste0(variable, " – ", title),
    var_label = factor(var_label, levels = unique(var_label)),
    low_r2    = r2_total < 0.3 | r2_this_axis < 0.3
  )

loading_long_unfiltered <- loading_long_unfiltered %>%
  arrange(as.numeric(str_extract(variable, "[0-9]+"))) %>%
  mutate(
    var_label = paste0(variable, " – ", title),
    var_label = factor(var_label, levels = unique(var_label)),
    low_r2    = r2_total < 0.3 | r2_this_axis < 0.3
  )

loading_long_unfiltered <- loading_long_unfiltered %>%
  mutate(
    pct_this_axis = r2_this_axis / r2_total * 100,
    low_r2        = r2_total < r2_threshold | pct_this_axis < pct_threshold
  ) %>% 
  mutate(weighted_loading = loading * sqrt(r2_total))

loadings_axes_unfiltered <- ggplot(loading_long_unfiltered, aes(x = pct, y = var_label, fill = loading)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = round(loading, 2),
                color = abs(loading) > 0.4,
                alpha = ifelse(low_r2, 0.4, 1)),
            size = 2.8) +
  scale_fill_gradient2(
    low      = "#cc0000",
    mid      = "white",
    high     = "#1a7a1a",
    midpoint = 0,
    name     = "Loading\n(Spearman r)"
  ) +
  scale_color_manual(values = c("TRUE" = "white", "FALSE" = "grey30"), guide = "none") +
  scale_alpha_identity() +
  labs(
    x        = NULL,
    y        = NULL,
    title    = "Variable loadings on PCoA axes",
    subtitle = "Faded tiles: r² < 0.3 and < 30% variance on this axis" 
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x     = element_text(size = 9),
    axis.text.y     = element_text(size = 9),
    panel.grid      = element_blank(),
    legend.position = "right"
  ) +
  scale_y_discrete(limits = rev)

ggsave(here::here("output/2.2_pcoa_figures_loadings_axes_unfiltered.png"), loadings_axes_unfiltered, width = 13, height = 10)
loadings_axes_unfiltered

# Plot PCoA ####
# ── Society coordinates with metadata ────────────────────────────────────────


additional_response_vars <- c("SCCS793", "SCCS794", "SCCS795", # (Ross: political participation)
                              "SCCS667", # Sanday, rape
                              "SCCS754") # Wife-beating (Broude)


# pred_resp %>%
#   ggplot(aes(x = Axis.1, y = Axis.2)) +
#   geom_point(aes(color = complexity), size = 2.5, alpha = 0.8) +
#   scale_color_gradient2(low = "#1A5276", mid = "white", high = "#B03A2E",
#                         midpoint = 0, name = "Complexity") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
#   labs(x = "Axis 1", y = "Axis 2") +
#   theme_minimal(base_size = 13)

# Excel file with clickable HRAF links ####
# ── Build context_wide from key SCCS variables ────────────────────────────────

context_vars <- c(
  "SCCS1",   # Subsistence economy
  "SCCS4",   # Settlement pattern
  "SCCS5",   # Community size
  "SCCS6",   # Jurisdictional hierarchy
  "SCCS156", # Region
  "SCCS157"  # Subsistence economy (Murdock)
)

society_profile <- societies %>%
  select(soc_id, main_focal_year,HRAF_name_ID, HRAF_link)

# Check which exist in your data
variables %>% 
  filter(var_id %in% context_vars) %>%
  select(var_id, title) %>%
  print()

context_wide <- sccs %>%
  filter(var_id %in% context_vars) %>%
  group_by(soc_id, var_id) %>%
  slice_max(year, na_rm = TRUE, n = 1) %>%
  ungroup() %>%
  select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code) %>%
  # join readable names for context variables
  rename_with(~ variables$title[match(.x, variables$var_id)],
              any_of(context_vars)) 

# ── Master coords table with all metadata ────────────────────────────────────

coord_societies <- coords_df %>%
  left_join(society_profile, by = "soc_id") %>%
  left_join(context_wide, by = "soc_id")


# Save with URL recognition

library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "Sheet1")

writeData(wb, "Sheet1", coord_societies)

url_col_index <- which(names(coord_societies) == "HRAF_link")

writeFormula(wb, "Sheet1",
             x = paste0('HYPERLINK("', coord_societies$HRAF_link, '","', coord_societies$HRAF_link, '")'),
             startCol = url_col_index,
             startRow = 2
)

saveWorkbook(wb, here::here("output/2.2_pcoa_figures_coord_societies.xlsx"), overwrite = TRUE)



# PLOT LOADINGS


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


# Change the sign of the loading if it was coded as high = bad for women
var_loadings_corrected <- var_loadings %>%
  left_join(direction_table, by = "var_id") %>%
  mutate(across(
    starts_with("Axis."),
    ~ . * women_favorable
  ))

# var_loadings_corrected <- var_loadings_corrected %>%
#   mutate(title_short = case_when(
#     str_detect(title, "Solely by Men")   ~ "Control fruits: men's labour",
#     str_detect(title, "by Men and Women")   ~ "Control fruits: joint labour",
#     str_detect(title, "Solely by Women") ~ "Control fruits: women's labour",
#     str_detect(title, "Final Authority.*Infant") ~ "Authority: infants",
#     str_detect(title, "Final Authority.*Post") ~ "Authority: older children",
#     str_detect(title, "Double Standard.*Premarital") ~ "No double standard: premarital sex",
#     str_detect(title, "Double Standard.*Extramarital") ~ "No double standard: extramarital sex",
#     str_detect(title, "Explicitly Male Work") ~ "Male work groups",
#     str_detect(title, "Explicitly Female Work") ~ "Female work groups",
#     TRUE ~ str_trunc(title, 45)
#   ))


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

corplot_axes_category <- axis_category_summary %>%
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

ggsave("output/2026-04-08_pcoa_category_corplot_axes_category.png", corplot_axes_category,
       width = 6, height = 4)


var_order <- var_loadings_corrected %>%
  left_join(whyte_vars %>% select(var_id_whyte, category2),
            by = "var_id_whyte") %>%
  arrange(desc(category), category2, title_shortened) %>%
  pull(title_shortened) %>%
  unique()


vars_plot <- var_loadings_corrected %>%
  left_join(whyte_vars %>% select(var_id_whyte, category2),
            by = "var_id_whyte") %>%
  pivot_longer(cols = c(Axis.1, Axis.2, Axis.3, Axis.4, Axis.5, Axis.6, Axis.7, Axis.8),
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



# Loadings plot ####

# Step 1: define colors once
category_colors <- c(
  "domination"      = "#66C2A5",
  "power"           = "#FC8D62", 
  "sexual_autonomy" = "#8DA0CB",
  "role_material"   = "#E78AC3",
  "role_symbolic"   = "#A6D854"
)

legend_plot <- ggplot(
  data.frame(category = names(category_colors)),
  aes(x = 1, y = category, fill = category)
) +
  geom_tile() +
  scale_fill_manual(values = category_colors) +
  theme(legend.position = "right")

standalone_legend <- cowplot::get_legend(legend_plot)

cowplot::plot_grid(standalone_legend)

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

loadings_plot2 <- cowplot::plot_grid(standalone_legend,
                                     loadings_plot + theme(legend.position = "none"),
                                     rel_widths = c(0.2, 1))


ggsave("output/2026-04-08_pcoa_category_loadings_plot.png", loadings_plot2,
       width = 17, height = 6)



# Data for Angarika to look at ####

