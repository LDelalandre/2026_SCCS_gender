
source(here::here("R/01_import.R"))

subset_agri <- F

source(here::here("R/02a_pcoa_data_prep.R"))
source(here::here("R/02b_pcoa_core.R"))
# source(here::here("R/02c_pcoa_permutation.R"))
# source(here::here("R/02d_pcoa_robustness.R"))

source(here::here("R/03a_predictors_data_prep.R"))
source(here::here("R/03b_predictors_pcoa.R"))
source(here::here("R/03b_predictors_one_pcoa.R"))
source(here::here("R/03c_predictors_technology.R"))

source(here::here("R/04a_dimorphism.R"))

# source(here::here("R/04b_predictors_models.R"))

source(here::here("R/05_descriptive_tables.R"))

source(here::here("R/06a_figures_pcoa_loadings.R"))
source(here::here("R/06b_figures_pcoa_biplot.R"))
# source(here::here("R/06c_figures_pcoa_additional_variables.R"))
source(here::here("R/06d_predictors_figures.R"))

# SessionInfo
sink("sessionInfo.txt")
sessionInfo()
sink()
