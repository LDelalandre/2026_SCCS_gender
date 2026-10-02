source("R/functions.R")

library(cluster)    # for daisy/Gower
library(ape)        # for pcoa
library(vegan) # for robustness


# pcoa ####
## select variables ####

whyte_vars_to_remove <- c("DV34") # (matri)locality

whyte_vars <- gender_response_recode %>% 
  filter(column_number_whyte78 %in% rep(1:52)) %>% 
  select(var_id, title,var_id_whyte, column_number_whyte78, domain, domain_name, title_shortened) %>%
  mutate(title = if_else(var_id == "SCCS625", paste("(No)", title), title)) %>%  # high = no high value on men being agressive
  filter(!var_id_whyte %in% whyte_vars_to_remove) # Remove locality

# Categories of variables
whyte_category <- whyte_vars %>% 
  select(var_id, var_id_whyte, title, title_shortened) %>% 
  left_join(whyte_category_angarika)

whyte_codes <- variables %>% 
  filter(title %in% whyte_vars$title) %>% 
  select(var_id, category) %>% 
  merge(codes_raw %>% 
          filter(var_id %in% whyte_vars$var_id), 
        by = "var_id")

## Recode six variables ####

# For these variables, I reverse the coding order, i.e., if variables
# is coded from 1 to 3, 1 becomes 3, 2 becomes 2, and 3 becomes 1.

vars_to_recode <- gender_response_recode %>%
  select(var_id, title, women_favorable) %>% 
  filter(women_favorable == -1)

whyte_codes_corrected <- whyte_codes %>% 
  filter(var_id %in% vars_to_recode$var_id) %>% 
  group_by(var_id) %>% 
  mutate(max_value = max(code, na.rm = T)) %>% 
  mutate(code = max_value + 1 - code) %>% 
  select(-max_value) %>% 
  select(-category)

codes <- codes_raw %>% 
  filter(!var_id %in% vars_to_recode$var_id) %>% 
  rbind(whyte_codes_corrected)

sccs_recoded_vars <- sccs_raw %>% 
  filter(var_id %in% vars_to_recode$var_id) %>% 
  group_by (var_id) %>% 
  mutate(max_value = max(code, na.rm = T)) %>% 
  mutate(code = max_value + 1 - code) %>% 
  select(-max_value)
  
sccs <- sccs_raw %>% 
  filter(!var_id %in% vars_to_recode$var_id) %>% 
  rbind(sccs_recoded_vars)

whyte_data <- sccs %>% 
  filter(var_id %in% whyte_vars$var_id) %>% 
  group_by(soc_id, var_id) %>%
  ungroup()


## Subsistence type of societies ####
codes_SCCS820 <- codes %>% 
  filter(var_id == "SCCS820") %>% 
  select(code, description) %>% 
  rename(SCCS820 = code) %>% 
  mutate(SCCS820 = as.factor(SCCS820))

society_subsistence_type <- sccs %>% 
  filter(soc_id %in% whyte_data$soc_id) %>% 
  filter(var_id %in% c("SCCS820", "SCCS833")) %>%
  select(soc_id, var_id, code) %>% 
  pivot_wider(names_from = var_id, values_from = code) %>% 
  # filter(!SCCS820 == 8) %>% 
  mutate(SCCS820 = as.factor(SCCS820)) %>% 
  left_join(societies %>% select(soc_id, pref_name_for_society)) %>% 
  left_join(codes_SCCS820) %>% 
  mutate(label = paste(SCCS820, description, sep = "_")) 

society_subsistence_type %>% 
  group_by(SCCS820) %>% 
  summarize(n = n())

# Subset on the agricultural societies only (OPTIONAL)
subset_agri <- F
if (subset_agri){
  whyte_data <- whyte_data %>% 
    left_join(society_subsistence_type %>% select(soc_id, description)) %>% 
    filter(description %in% c("Incipient Agriculture",
                              "Extensive Agriculture",
                              "Intensive Agriculture",
                              "Domestic Animals"))
}

  
## Build a matrix ####
whyte_data_wide <- whyte_data %>%
  dplyr::select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code) %>% 
  filter(!if_all(-soc_id, is.na))

mat <- whyte_data_wide %>%
  dplyr::select(-soc_id) %>% 
  as.matrix()
rownames(mat) <- whyte_data_wide$soc_id

## Missingness ####

society_coverage <- rowMeans(!is.na(mat))
data.frame(
  min    = min(society_coverage),
  q1     = quantile(society_coverage, 0.25),
  median = median(society_coverage),
  mean   = mean(society_coverage),
  q3     = quantile(society_coverage, 0.75),
  max    = max(society_coverage)) %>% 
  write_csv("output/02_pcoa_society_coverage_summary.csv")

# Remove rows with > 30% missing (=7 rows)
row_miss <- rowMeans(is.na(mat))
mat_filtered <- mat[row_miss <= 0.30, ]
# colnames(mat_filtered) <- label_vars(colnames(mat_filtered))

societies_removed <- mat[row_miss > 0.30, ] %>% rownames()


variable_coverage <- colMeans(!is.na(mat)) 

variable_coverage %>% 
  as.data.frame() %>% 
  rownames_to_column("var_id") %>% 
  rename(coverage = ".") %>% 
  write_csv("output/02a_variable_coverage.csv")

data.frame(
  min    = min(variable_coverage),
  q1     = quantile(variable_coverage, 0.25),
  median = median(variable_coverage),
  mean   = mean(variable_coverage),
  q3     = quantile(variable_coverage, 0.75),
  max    = max(variable_coverage)) %>% 
  write_csv("output/02_pcoa_variable_coverage_summary.csv")



## Prepare ordinal and binary variables ####

# Tell daisy which variables are ordinal and which are binary

# 1. Convert columns to ordered factors (needed for daisy to treat as ordinal)
mat_ord <- data.frame(lapply(as.data.frame(mat_filtered), function(x) {
  factor(x, levels = sort(unique(x[!is.na(x)])), ordered = TRUE)
}))
rownames(mat_ord) <- rownames(mat_filtered)

binary_cols <- c(6, 9, 13, 20, 21, 27, 33, 36, 47, 51)
ordinal_cols <- setdiff(1:ncol(mat_ord), binary_cols)

mat_ord_fixed <- mat_ord

# Recode binary columns to 0/1
mat_ord_fixed[, binary_cols] <- lapply(mat_ord[, binary_cols], function(x) {
  x <- as.numeric(as.character(x))
  vals <- sort(unique(x[!is.na(x)]))
  # maps lowest value -> 0, highest -> 1
  ifelse(x == vals[1], 0, ifelse(x == vals[2], 1, NA))
})
