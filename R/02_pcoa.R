source("R/01_import.R")
source("R/functions.R")

library(cluster)    # for daisy/Gower
library(ape)        # for pcoa

# Matrix on response variables ####

whyte_vars <- gender_response %>% 
  filter(column_number_whyte78 %in% rep(1:52)) %>% 
  select(var_id, title,var_id_whyte, column_number_whyte78, domain, domain_name) %>%
  mutate(short_title = paste0(var_id, "_", str_trunc(title, width = 30, ellipsis = "..."))) %>% 
  mutate(title = if_else(var_id == "SCCS625", paste("(No)", title), title)) # high = no high value on men being agressive

whyte_codes <- get_codes(whyte_vars$title)

whyte_data_wide <- sccs %>% 
  filter(var_id %in% whyte_vars$var_id) %>% 
  # If multiple sub_cases per society/variable, take one (e.g. most recent)
  group_by(soc_id, var_id) %>%
  slice_max(year, na_rm = TRUE, n = 1) %>%
  ungroup() %>%
  select(soc_id, var_id, code) %>%
  pivot_wider(names_from = var_id, values_from = code) %>% 
  filter(!if_all(-soc_id, is.na))

mat <- whyte_data_wide %>%
  select(-soc_id) %>%       # remove the id column
  as.matrix()

# Missingness ####

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

variable_coverage <- colMeans(!is.na(mat))
data.frame(
  min    = min(variable_coverage),
  q1     = quantile(variable_coverage, 0.25),
  median = median(variable_coverage),
  mean   = mean(variable_coverage),
  q3     = quantile(variable_coverage, 0.75),
  max    = max(variable_coverage)) %>% 
  write_csv("output/02_pcoa_variable_coverage_summary.csv")

col_miss <- colMeans(is.na(mat))
mat[, col_miss >= 0.30]

# Removes: "SCCS583" "SCCS620" "SCCS621" "SCCS622"
# Leadership Posts in Kinship or Extended Family Units
# Physical Punishment of the Spouse Condoned
# (No) Explicit View That Men Should and Do Dominat
# Attendance and Participation in General Community


mat[,"SCCS622"] %>% is.na() %>% mean()

# PCoA ####
mat_filtered2 <- mat_filtered[, ! colnames(mat_filtered) == "DV34"] #LOCALITY

# Tell daisy which variables are ordinal and which are binary

# 1. Convert columns to ordered factors (needed for daisy to treat as ordinal)
mat_ord <- data.frame(lapply(as.data.frame(mat_filtered2), function(x) {
  factor(x, levels = sort(unique(x[!is.na(x)])), ordered = TRUE)
}))

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

# Gower distance — handles NAs and ordinal variables
gower_ordinal <- cluster::daisy(mat_ord_fixed,
                                metric = "gower",
                                type = list(
                                  ordratio = ordinal_cols,
                                  symm = binary_cols
                                ))

pcoa_result <- pcoa(gower_ordinal)
