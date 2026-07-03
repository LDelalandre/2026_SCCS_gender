source("R/01_import.R")
source("R/functions.R")


library(cluster)    # for daisy/Gower
library(ape)        # for pcoa
library(vegan) # for robustness


# pcoa ####
## select variables ####

whyte_vars_to_remove <- c("DV34") # (matri)locality

whyte_vars <- gender_response_recode %>% 
  filter(column_number_whyte78 %in% rep(1:52)) %>% 
  select(var_id, title,var_id_whyte, column_number_whyte78, category1, category2, domain, domain_name, title_shortened) %>%
  mutate(short_title = paste0(var_id, "_", str_trunc(title, width = 30, ellipsis = "..."))) %>% #TODO: remove (keep manual short_title)
  mutate(title = if_else(var_id == "SCCS625", paste("(No)", title), title)) %>%  # high = no high value on men being agressive
  filter(!var_id_whyte %in% whyte_vars_to_remove) # Remove locality

# Categories of variables

#TODO: simplify the input
whyte_category <- whyte_vars %>% 
  arrange(category1, category2) %>% 
  select(var_id, var_id_whyte, category1, category2, title, title_shortened) %>% 
  mutate(category = 
           case_when(category1 == "role" & category2 %in% c("rituals", "sexual") ~ "role_symbolic",
                     category1 == "role" & category2 %in% c("subsistence", "warfare") ~ "role_material",
                     category1 == "autonomy" & category2 == "sexual" ~ "sexual_autonomy",
                     TRUE ~ category1)) %>% 
  left_join(whyte_category_angarika)


whyte_codes <- variables %>% 
  filter(title %in% whyte_vars$title) %>% 
  select(var_id, category) %>% 
  merge(codes_raw %>% 
          filter(var_id %in% whyte_vars$var_id), 
        by = "var_id")

## Recode six variables ####

# For these five variables, I reverse the coding order, i.e., if variables
# is coded from 1 to 3, 1 becomes 3, 2 becomes 2, and 3 becomes 1.

vars_to_recode <- read.csv("data/gender_response_variables_recode.csv") %>%
  select(var_id, title, women_favorable) %>% 
  filter(women_favorable == -1)

whyte_codes_corrected <- whyte_codes %>% 
  filter(var_id %in% vars_to_recode$var_id) %>% 
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


## Build a matrix ####
whyte_data <- sccs %>% 
  filter(var_id %in% whyte_vars$var_id) %>% 
  # If multiple sub_cases per society/variable, take one (e.g. most recent)
  group_by(soc_id, var_id) %>%
  slice_max(year, na_rm = TRUE, n = 1) %>%
  ungroup()

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
colnames(mat_filtered) <- label_vars(colnames(mat_filtered))

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