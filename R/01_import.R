library("tidyverse")

# Variables ####
sccs_raw <- read.csv(here::here("data/SCCS/data.csv"))

societies <- read.csv(here::here("data/SCCS/societies.csv")) %>% 
  rename(soc_id = id)
societies_mapping <- read.csv(here::here("data/SCCS/societies_mapping.csv")) %>% 
  rename(soc_id = id)

variables <- read.csv(here::here("data/SCCS/variables.csv")) %>% 
  rename(var_id = id)

codes_raw <- read.csv(here::here("data/SCCS/codes.csv")) %>% 
  left_join(variables %>% select(var_id, title))

gleeson <- read.csv(here::here("data/gleesonFemaleStatusFood2018.csv")) %>%  # height data
  mutate(society_name = str_replace(society_name, "\\*", ""))

gender_response <- read.csv(here::here("data/gender_response_variables.csv"))
gender_response_recode <- read.csv(here::here("data/gender_response_variables_recode.csv")) 
# info needed to recode 6 variables, so that, for each var, high value = high status of women

whyte_category_angarika <- read.csv(here::here("data/gender_response_variables_recode_angarika.csv")) %>%
  select(var_id, var_id_whyte, title, title_shortened, category_whyte, category_Angarika)

gender_predictors <- read.csv(here::here("data/gender_predictor_variables.csv"))

