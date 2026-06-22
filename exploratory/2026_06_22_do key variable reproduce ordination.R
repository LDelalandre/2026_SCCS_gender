# ── Helper: run one PCoA on a variable subset and return Spearman r vs ref ───
run_subset_pcoa <- function(var_subset, label) {
  mat_sub <- mat_ord_fixed[, var_subset, drop = FALSE]
  
  # Remove societies with all NAs for the selected variables
  all_na_rows <- apply(mat_sub, 1, function(x) all(is.na(x)))
  if (any(all_na_rows)) {
    message(label, ": dropping ", sum(all_na_rows), " societies with all NAs.")
    mat_sub <- mat_sub[!all_na_rows, , drop = FALSE]
  }
  
  gd <- tryCatch(
    daisy(mat_sub, metric = "gower",
          type = list(ordratio = 1:ncol(mat_sub))),
    error = function(e) { warning(label, ": daisy failed — ", e$message); NULL }
  )
  if (is.null(gd) || any(!is.finite(as.matrix(gd)))) return(NULL)
  
  pc <- tryCatch(
    pcoa(gd),
    error = function(e) { warning(label, ": pcoa failed — ", e$message); NULL }
  )
  if (is.null(pc) || sum(pc$values$Eigenvalues > 0) < 1) return(NULL)
  
  # Align ref_coords to the same societies
  ref_sub <- ref_coords[!all_na_rows, , drop = FALSE]
  
  # ... then use ref_sub instead of ref_coords in procrustes/cor
  pro <- procrustes(ref_sub, pc$vectors[, 1, drop = FALSE])
  r   <- cor(ref_sub[, 1], pro$Yrot[, 1], method = "spearman")
  
  
  data.frame(
    label    = label,
    n_vars   = length(var_subset),
    spearman_r = round(r, 3)
  )
}


# 3. Manual subset comparison ####
test_subsets <- list(
  list(label = "All",                   vars = c("DV46", "DV40", "DV51", "DV15", "DV36", "DV48", "DV5")),
  list(label = "Gender ideology",       vars = c("DV46", "DV40", "DV51")),
  list(label = "Family and household",  vars = c("DV15")),
  list(label = "Sex and marriage",      vars = c("DV36")),
  list(label = "Religion and rituals",  vars = c("DV48", "DV5"))
)

results_subsets <- map_dfr(test_subsets, ~ run_subset_pcoa(.x$vars, .x$label))
print(results_subsets)


# 4. Cumulative variable importance ####
priority_vars <- c("DV46", "DV40", "DV51", "DV15", "DV36", "DV48", "DV5")
priority_vars <- names(sort(abs(variable_loadings[priority_vars, "PCo1"]), decreasing = TRUE))

cumulative_subsets <- map(seq_along(priority_vars), function(n) {
  list(label = paste0("Top ", n, " vars"), vars = priority_vars[1:n])
})

results_cumulative <- map_dfr(cumulative_subsets, ~ run_subset_pcoa(.x$vars, .x$label))
print(results_cumulative)