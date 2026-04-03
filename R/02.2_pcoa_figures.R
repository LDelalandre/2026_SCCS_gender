

# Women's status variables ####

## Dispersion explained ####
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

ggplot(scree_df %>% filter(axis <= 15), aes(x = axis)) +
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


# Coordinates and loadings ####

# Society coordinates on PCoA axes
coords <- pcoa_result$vectors  # societies × axes

# Correlate each variable with each axis (Spearman, handles ordinal)
n_axes <- 12  # however many you decided to keep

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

## Variables well captured ####

n_axes <- 10

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
  geom_histogram()

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