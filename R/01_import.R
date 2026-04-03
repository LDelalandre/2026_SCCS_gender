library("tidyverse")

# Variables ####
sccs <- read.csv(here::here("data/SCCS/data.csv"))

societies <- read.csv(here::here("data/SCCS/societies.csv")) %>% 
  rename(soc_id = id)
societies_mapping <- read.csv(here::here("data/SCCS/societies_mapping.csv")) %>% 
  rename(soc_id = id)

variables <- read.csv(here::here("data/SCCS/variables.csv")) %>% 
  rename(var_id = id)

codes <- read.csv(here::here("data/SCCS/codes.csv")) %>% 
  left_join(variables %>% select(var_id, title))

gleeson <- read.csv(here::here("data/gleesonFemaleStatusFood2018.csv")) %>%  # height data
  mutate(society_name = str_replace(society_name, "\\*", ""))

gender_response <- read.csv(here::here("data/SCCS/gender_response_variables.csv"))


### Predictor variables ####

var_hierarchy <- c("Political Organization (Adapted from Murdock 1961, P. 207)",
                   "Crimes against Person Punished",
                   "(No) Government Full-time Bureaucrats",
                   "Community Is Part of a Kingdom",
                   "Community Part of a Kingdom in the past That No Longer Exists")

var_complexity <- c("Plow", 
                    "Irrigation", 
                    "Cereal Grains the Principal Crop",
                    "Importance of Agriculture in Subsistence, including Gardening",
                    "Political Organization (Adapted from Murdock 1961, P. 207)",
                    "Private Property",
                    "Settlement Type",
                    "Metalworking",
                    "Manufacture of Pottery",
                    "True Weaving",
                    "Social Stratification in the Larger Society",
                    "Social Stratification in the Local Community",
                    "Societal Complexity (Guttman Scale - Freeman and Winch 1957)")