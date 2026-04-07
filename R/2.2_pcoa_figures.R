
## Scree plot ####
pcoa_result <- readRDS(here::here("output/pcoa_result"))

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
    left_join(whyte_vars %>% select(var_id_whyte, title, domain),
              by = c("variable" = "var_id_whyte")) %>%
    arrange(desc(abs(loading))) %>%
    select(variable, title, domain, loading, r2_total, pct_this_axis) %>%
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
    col.names = c("PCo", "Variable", "Title", "Domain", 
                  "Loading", "R² total", "% this axis"),
    align     = c("c", "l", "l", "l", "r", "r", "r")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width        = FALSE,
    position          = "left"
  ) %>%
  column_spec(3, width = "8cm") %>%
  column_spec(5, 
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
coords_df <- as.data.frame(pcoa_result$vectors[, 1:10])
colnames(coords_df) <- paste0("Ax", 1:10)
coords_df$soc_id <- rownames(coords_df)

coords_df <- coords_df %>%
  left_join(societies %>% select(soc_id, pref_name_for_society, 
                                 origLat, origLong),
            by = "soc_id")

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

