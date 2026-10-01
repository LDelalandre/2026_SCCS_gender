# PCoA on categories ####

## Coverage ####


# Warfare: remove the unresolved rating, and unclear
  # mutate(code = case_when (var_id %in% c("SCCS1649", "SCCS1650") & code == 0 ~ NA,
  #                          var_id %in% c("SCCS1649", "SCCS1650") & code == 88 ~ NA,
  #                          TRUE ~ code))

coverage_by_cat <- predictors_data %>%
  filter(soc_id %in% whyte_societies) %>%
  group_by(var_id) %>%
  summarise(
    n_obs    = sum(!is.na(code)),
    coverage = round(mean(!is.na(code)), 3),
    .groups  = "drop"
  ) %>%
  left_join(predictors_detailed %>% dplyr::select(var_id, title, category), 
            by = "var_id") %>%
  arrange(category, desc(coverage))

coverage_by_cat

# ── 0. Reshape data to wide format ────────────────────────────────────────────
# assumes predictors_data has columns: soc_id, var_id, value

predictors_wide <- predictors_data %>%
  dplyr::select(soc_id, var_id, code) %>%
  tidyr::pivot_wider(names_from = var_id, values_from = code) %>%
  tibble::column_to_rownames("soc_id") 
  # mutate(# WARFARE
  #   SCCS1649 = log(1+SCCS1649),
  #   SCCS1650 = log(1+SCCS1650)) 

# align to the same societies as in the response PCoA
common_soc <- intersect(rownames(predictors_wide), 
                        rownames(pcoa_result$vectors))
predictors_wide <- predictors_wide[common_soc, ]


# ── 1. Function: PCoA on one category + scree ─────────────────────────────────
run_category_pcoa <- function(var_ids, label) {
  
  mat <- predictors_wide[, var_ids, drop = FALSE]
  
  # drop variables that are more than 50% NA
  mat <- mat[, colSums(!is.na(mat)) > nrow(mat) * 0.5]
  
  if(ncol(mat) < 2) {
    message(label, ": too few variables after NA filter")
    return(NULL)
  }
  
  gd <- daisy(mat, metric = "gower",
              type = list(ordratio = 1:ncol(mat)))
  
  pc <- pcoa(gd)
  
  eigenvalues <- pc$values$Eigenvalues
  pos_eigen   <- eigenvalues[eigenvalues > 0]
  var_exp     <- pos_eigen / sum(pos_eigen)
  
  list(
    pcoa     = pc,
    var_exp  = var_exp,
    label    = label,
    vars     = colnames(mat),
    societies = rownames(mat)
  )
}

# ── 2. Run per category ────────────────────────────────────────────────────────
categories <- split(predictors_detailed$var_id, predictors_detailed$category)

categories[["technology"]] <- NULL
categories[["hunting"]] <- NULL
categories[["warfare"]] <- NULL
cat_results <- lapply(names(categories), function(cat) {
  run_category_pcoa(categories[[cat]], cat)
})
names(cat_results) <- names(categories)



# ── 3. Inspect variance explained per category ────────────────────────────────
# How many axes does each category need?
var_explained <- lapply(names(cat_results), function(cat) {
  res <- cat_results[[cat]]
  if(is.null(res)) return(NULL)
  cumvar <- cumsum(res$var_exp)
  round(data.frame(
    axis    = seq_along(res$var_exp),
    var_exp = round(res$var_exp, 3),
    cumvar  = round(cumvar, 3)
  ), 3)
})
names(var_explained) <- names(cat_results)


# ── Scree plots ───────────────────────────────────────────────────────────────
scree_plots <- lapply(names(cat_results), function(cat) {
  res <- cat_results[[cat]]
  if(is.null(res)) return(NULL)
  
  df <- data.frame(
    axis    = seq_along(res$var_exp),
    var_exp = res$var_exp
  )
  
  ggplot(df, aes(x = axis, y = var_exp)) +
    geom_line(color = "grey50") +
    geom_point(size = 2.5, color = "#2166ac") +
    geom_hline(yintercept = 0.1, linetype = "dashed", 
               color = "grey60", linewidth = 0.4) +
    scale_x_continuous(breaks = df$axis) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = cat, x = "Axis", y = "Variance explained") +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))
})
names(scree_plots) <- names(cat_results)

appendix_predictors_pcoa <- wrap_plots(scree_plots, ncol = 3)
ggsave("draft/fig/appendix_predictors_pcoa.png", appendix_predictors_pcoa)


## Inspect the PCoAs ####

get_loadings <- function(cat, n_axes = 3) {
  
  soc_ids    <- rownames(cat_results[[cat]]$pcoa$vectors)
  # axis_names <- paste0(toupper(substr(cat, 1, 1)), "Co", 1:n_axes)
  axis_names <- paste0("Co", 1:n_axes)
  
  axes_df <- cat_results[[cat]]$pcoa$vectors %>%
    as.data.frame() %>%
    dplyr::select(1:n_axes) %>%
    setNames(axis_names) %>%
    tibble::rownames_to_column("soc_id")
  
  predictors_data %>%
    filter(var_id %in% categories[[cat]]) %>%
    filter(soc_id %in% soc_ids) %>%
    left_join(predictors_detailed %>% dplyr::select(var_id, title), by = "var_id") %>%
    left_join(axes_df, by = "soc_id") %>%
    group_by(title) %>%
    summarise(
      across(all_of(axis_names),
             ~ cor(.x, code, method = "spearman",
                   use = "pairwise.complete.obs")),
      .groups = "drop"
    )
}

get_loadings("complexity",     n_axes = 4) %>% print(n = 31)

pred_loadings <- names(cat_results)[names(cat_results) != "male_solidarity"] %>%
  map_dfr(function(cat_name) {
    get_loadings(cat_name, n_axes = 2) %>%
      mutate(category = cat_name, .before = everything())
  })

pred_loadings <- names(cat_results) %>%
  set_names() %>%
  map_dfr(function(cat_name) {
    result <- tryCatch(
      get_loadings(cat_name, n_axes = 4),
      error = function(e) {
        get_loadings(cat_name, n_axes = 2) %>%
          mutate(Co3 = NA_real_, Co4 = NA_real_)
      }
    )
    result
  }, .id = "category") %>% 
  mutate(Co1 = if_else(category == "agriculture", -Co1, Co1)) # CHANGING THE SING FOR DOWNSTREAM INTERPRETATION


# write.csv2(all_loadings, "output/3.1_predictors_pred_loadings.csv", row.names = F)

# Extract predictors per category ####

complexity <- cat_results[["complexity"]]$pcoa$vectors %>%
  as.data.frame() %>%
  dplyr::select(complexity = Axis.1) %>% # positive = complex
  tibble::rownames_to_column("soc_id")


agriculture <- cat_results[["agriculture"]]$pcoa$vectors %>%
  as.data.frame() %>%
  mutate(Axis.2 = - Axis.2) %>% 
  dplyr::select(agriculture_cereals = Axis.1, agriculture_importance = Axis.2) %>% # importance positive = much agri (and cereal positive = rely mostly on cereals)
  tibble::rownames_to_column("soc_id") %>% 
  mutate(agriculture_cereals = -agriculture_cereals) # CHANGING THE SING FOR DOWNSTREAM INTERPRETATION

herding <- cat_results[["herding"]]$pcoa$vectors %>%
  as.data.frame() %>%
  dplyr::select(herding_importance = Axis.1, herding_milk = Axis.2) %>% # positive = importance for 1, milked for 2
  tibble::rownames_to_column("soc_id")

# warfare <- cat_results[["warfare"]]$pcoa$vectors %>%
#   as.data.frame() %>%
#   dplyr::select(warfare_intensity = Axis.1, warfare_location = Axis.2) %>% # location positive = internal warfare
#   tibble::rownames_to_column("soc_id")

matri <- cat_results[["kinship"]]$pcoa$vectors %>%
  as.data.frame() %>%
  dplyr::select(matri = Axis.1) %>% # positive = matrilineal/local
  tibble::rownames_to_column("soc_id")

hunting <- predictors_data %>% 
  filter(var_id == "SCCS730") %>%  # importance of hunting and gathering in subsistence
  rename(hunting = code) %>% 
  dplyr::select(soc_id, hunting)

warfare <- predictors_data %>% 
  filter(var_id == "SCCS693") %>% # frequency of intercommunity armed conflict
  rename(warfare = code) %>% 
  dplyr::select(soc_id, warfare)
  
all_axes <- complexity %>%
  full_join(agriculture, by = "soc_id") %>%
  full_join(herding,     by = "soc_id") %>%
  full_join(matri, by = "soc_id") %>% 
  left_join(hunting, by = "soc_id") %>% 
  full_join(warfare,     by = "soc_id")

response_axes <- pcoa_result$vectors[,c(1:5)] %>%
  as.data.frame() %>%
  rownames_to_column("soc_id")

pred_resp <- all_axes %>% 
  left_join(response_axes, by = "soc_id") %>%
  # recode matri as a categorical variable with 3 values
  mutate(matri_cat = case_when(
    matri < 0      ~ "non_matri",
    matri < 0.5    ~ "intermediate",
    matri >= 0.5   ~ "matri"
  ) %>% ordered(levels = c("non_matri", "intermediate", "matri"))) %>% 
  filter(!is.na(complexity)) %>% 
  mutate(hunting = max(hunting) + 1 - hunting) # RECODE HUNTING


