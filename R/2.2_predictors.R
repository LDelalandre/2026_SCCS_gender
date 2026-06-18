library(cluster)
library(ape)
library(patchwork)

whyte_societies <- sccs %>% 
  filter(var_id %in% whyte_vars$var_id) %>% 
  filter(!is.na(code)) %>% 
  pull(soc_id) %>% 
  unique()

# Define predictors ####

predictors_putative <- gender_predictors %>% 
  dplyr::select(var_id, title) %>% 
  left_join(variables) 

write.csv(predictors_putative, here::here("output/03_predictors_predictors_putative.csv"))



predictors_putative <- read.csv(here::here("output/03_predictors_predictors_putative_edited.csv")) %>% 
  filter(category != "") %>% 
  filter(predictor !="no") %>% 
  dplyr::select(var_id, category)

predictors_additional_id <- c(
  # WARFARE
  "SCCS1649", "SCCS1650", # warfare from Ember - this is overall: "SCCS1648"
  # "SCCS1779", # weapons used in warfare
  
  # FERTILITY
  "SCCS44", "SCCS45", # weaning
  "SCCS34", "SCCS240", # post partum sex taboo duration (see if these are the same!)
  "SCCS1130", "SCCS64", # population density
  
  # SYMBOLIC
  # "SCCS238" , # high gods absent/otiose (paresseur) / active but not supporting morality / active and supporting it
  "SCCS676" # Sanday's variable on Origin symbolism: "creation stories"
  
)

predictors_additional_category <- c(rep("warfare", 2), rep("fertility", 6),
                                    rep("symbolic", 1))

predictors_additional <- data.frame(var_id = predictors_additional_id,
                                    category = predictors_additional_category)

predictors <- bind_rows(predictors_putative, predictors_additional) %>% 
  filter(!category == "method")

predictors_detailed <- predictors %>% 
  left_join(variables %>% dplyr::select(var_id, title)) %>% 
  arrange(category) %>% 
  filter(!var_id == "SCCS693") %>%  # remove Whyte's binary code
  filter(!var_id %in% c("SCCS688", "SCCS689")) %>%  # REMOVED post-european contact husbandry
  filter(!var_id == "SCCS713") %>% # REMOVED pre-classical religion. Or should be in complexity?
  filter(!var_id == "SCCS714") %>% #sex ratio
  filter(!var_id == "SCCS715") # systematic absence of married males
# filter(!var_id %in% c("SCCS695", "SCCS694")) # male solidarity, male initiation ceremonies


write.csv(predictors_detailed, here::here("output/03_predictors_predictors_detailed.csv"), row.names = F) 

# see the codes

codes %>% 
  filter(var_id %in% predictors_detailed$var_id) %>% 
  View
  

# PCoA on categories ####

## Coverage ####

predictors_detailed <- read.csv("output/03_predictors_predictors_detailed.csv") %>% 
  mutate(category = case_when(
    
    # Guttman scale technologies
    title == "Plow"         ~ "guttman",
    title == "Irrigation"   ~ "guttman",
    title == "True Weaving" ~ "guttman",
    
    # # Upstream complexity — material and technological
    # var_id == "SCCS704" ~ "complexity_upstream",  # private property
    # var_id == "SCCS705" ~ "complexity_upstream",  # settlement type
    # var_id == "SCCS732" ~ "complexity_upstream",  # trade importance
    # var_id == "SCCS706" ~ "complexity_upstream",  # metalworking
    # var_id == "SCCS707" ~ "complexity_upstream",  # pottery
    # var_id == "SCCS711" ~ "complexity_upstream",  # societal complexity Guttman
    # 
    # # Downstream complexity — political and organisational
    # var_id == "SCCS699" ~ "complexity_downstream",  # political organisation
    # var_id == "SCCS700" ~ "complexity_downstream",  # crimes punished
    # var_id == "SCCS701" ~ "complexity_downstream",  # full-time bureaucrats
    # var_id == "SCCS702" ~ "complexity_downstream",  # part of kingdom
    # var_id == "SCCS703" ~ "complexity_downstream",  # kingdom in past
    # var_id == "SCCS709" ~ "complexity_downstream",  # stratification larger
    # var_id == "SCCS710" ~ "complexity_downstream",  # stratification local
    
    TRUE ~ category
  ))


predictors_data <- sccs %>% 
  filter(var_id %in% predictors_detailed$var_id) 

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

print(coverage_by_cat)

# ── 0. Reshape data to wide format ────────────────────────────────────────────
# assumes predictors_data has columns: soc_id, var_id, value

predictors_wide0 <- predictors_data %>%
  dplyr::select(soc_id, var_id, code) %>%
  tidyr::pivot_wider(names_from = var_id, values_from = code) %>%
  tibble::column_to_rownames("soc_id") %>% 
  mutate(# WARFARE
    SCCS1649 = log(1+SCCS1649),
    SCCS1650 = log(1+SCCS1650)) 

predictors_wide <- predictors_wide0 %>% 
  dplyr::select(-SCCS676) # creation stories, just one variable, no pcoa to do


# align to the same societies as your response PCoA
common_soc <- intersect(rownames(predictors_wide), 
                        rownames(pcoa_result$vectors))
predictors_wide <- predictors_wide[common_soc, ]


# ── 1. Function: PCoA on one category + scree ─────────────────────────────────
run_category_pcoa <- function(var_ids, label) {
  
  mat <- predictors_wide[, var_ids, drop = FALSE]
  
  # drop variables that are entirely NA
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

categories[["symbolic"]] <- NULL
categories[["extended_families"]] <- NULL
categories[["guttman"]] <- NULL
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

wrap_plots(scree_plots, ncol = 3)


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
  }, .id = "category")

# write.csv2(all_loadings, "output/3.1_predictors_pred_loadings.csv", row.names = F)

# Extract predictors per category ####

# complexity <- cat_results[["complexity"]]$pcoa$vectors %>%
#   as.data.frame() %>%
#   dplyr::select(complexity = Axis.1) %>% # positive = complex
#   tibble::rownames_to_column("soc_id")

# complexity_upstream <- cat_results[["complexity_upstream"]]$pcoa$vectors %>%
#   as.data.frame() %>%
#   dplyr::select(complexity_upstream = Axis.1) %>% # positive = complex
#   tibble::rownames_to_column("soc_id")

# complexity_downstream <- cat_results[["complexity_downstream"]]$pcoa$vectors %>%
#   as.data.frame() %>%
#   dplyr::select(complexity_downstream = Axis.1) %>% # positive = complex
#   tibble::rownames_to_column("soc_id")

complexity <- cat_results[["complexity"]]$pcoa$vectors %>%
  as.data.frame() %>%
  dplyr::select(complexity = Axis.1) %>% # positive = complex
  tibble::rownames_to_column("soc_id")

hunting <- cat_results[["hunting"]]$pcoa$vectors %>%
  as.data.frame() %>%
  dplyr::select(hunting = Axis.1) %>% # positive = hunting important
  tibble::rownames_to_column("soc_id")

agriculture <- cat_results[["agriculture"]]$pcoa$vectors %>%
  as.data.frame() %>%
  mutate(Axis.2 = - Axis.2) %>% 
  dplyr::select(agriculture_cereals = Axis.1, agriculture_importance = Axis.2) %>% # importance positive = much agri (and cereal positive = rely mostly on cereals)
  tibble::rownames_to_column("soc_id")

herding <- cat_results[["herding"]]$pcoa$vectors %>%
  as.data.frame() %>%
  dplyr::select(herding_importance = Axis.1, herding_milk = Axis.2) %>% # positive = importance for 1, milked for 2
  tibble::rownames_to_column("soc_id")

warfare <- cat_results[["warfare"]]$pcoa$vectors %>%
  as.data.frame() %>%
  dplyr::select(warfare_intensity = Axis.1, warfare_location = Axis.2) %>% # location positive = internal warfare
  tibble::rownames_to_column("soc_id")

kinship <- cat_results[["kinship"]]$pcoa$vectors %>%
  as.data.frame() %>%
  dplyr::select(matri = Axis.1) %>% # positive = matrilineal/local
  tibble::rownames_to_column("soc_id")

creation_stories <- predictors_wide0 %>% 
  dplyr::select("SCCS676") %>% #creation stories, Sanday's variable
  rename(creation_stories = SCCS676) %>% # high value = male_dominated
  tibble::rownames_to_column("soc_id")

fertility <- predictors_wide0 %>% 
  dplyr::select(SCCS45, SCCS240, SCCS1130) %>% 
  rename(weaning = SCCS45,
         taboo = SCCS240,
         density = SCCS1130) %>% 
  tibble::rownames_to_column("soc_id")


all_axes <- complexity %>%
  full_join(agriculture, by = "soc_id") %>%
  full_join(herding,     by = "soc_id") %>%
  full_join(hunting, by = "soc_id") %>%
  full_join(warfare,     by = "soc_id") %>%
  full_join(kinship, by = "soc_id") %>%
  left_join(creation_stories,     by = "soc_id") %>%
  left_join(fertility,     by = "soc_id")

# all_axes <- complexity_upstream %>%
#   full_join(complexity_downstream, by = "soc_id") %>%
#   full_join(agriculture, by = "soc_id") %>%
#   full_join(herding,     by = "soc_id") %>% 
#   full_join(hunting, by = "soc_id") %>%
#   full_join(warfare,     by = "soc_id") %>% 
#   full_join(kinship, by = "soc_id") %>%
#   left_join(creation_stories,     by = "soc_id") %>% 
#   left_join(fertility,     by = "soc_id") 


# Correlations among predictors 
library(corrplot)
predictors_selected <- all_axes %>% dplyr::select(-soc_id)
cor(predictors_selected, use = "pairwise.complete.obs") %>%
  corrplot(method = "number")



response_axes <- pcoa_result$vectors %>%
  as.data.frame() %>%
  rownames_to_column("soc_id")

pred_resp <- all_axes %>% 
  left_join(response_axes, by = "soc_id") %>%
  # recode matri as a categorical variable with 3 values
  mutate(matri_cat = case_when(
    matri < 0      ~ "non_matri",
    matri < 0.5    ~ "intermediate",
    matri >= 0.5   ~ "matri"
  ) %>% ordered(levels = c("non_matri", "intermediate", "matri")))

## Correlations of predictors and axis ####

predictors <- c("complexity", "agriculture_cereals", 
                "agriculture_importance", "herding_importance",
                "herding_milk", "hunting", "warfare_intensity",
                "warfare_location", "matri")

# map_dfr(predictors, ~{
#   cor.test(pred_resp[[.x]], pred_resp$Axis.1, 
#            use = "complete.obs") %>%
#     broom::tidy() %>%
#     mutate(predictor = .x)
# }) %>%
#   dplyr::select(predictor, estimate, p.value) %>%
#   arrange(p.value)

dropped <- c("weaning", "taboo", "creation_stories", "density")

map_dfr(dropped, ~{
  cor.test(pred_resp[[.x]], pred_resp$Axis.1, 
           method = "spearman",
           use = "complete.obs") %>%
    broom::tidy() %>%
    mutate(predictor = .x)
}) %>%
  dplyr::select(predictor, estimate, p.value) %>%
  mutate(p.adjusted = p.adjust(p.value, method = "BH")) %>%
  arrange(p.value)




