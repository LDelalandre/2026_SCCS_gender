# get_codes <- function(var_focal){
#   # e.g., var_focal <- c("Plow", "Irrigation")
#   variables_focal <- variables %>% 
#     filter(title %in% var_focal)
#   
#   codes_focal <- variables_focal %>% 
#     select(var_id, category, title) %>% 
#     merge(codes_raw %>% 
#             filter(var_id %in% variables_focal$var_id), 
#           by = "var_id")
#   codes_focal
# }

# label_vars <- function(x){
#   # x is a character vector of SCCS ids
#   # whyte_vars$short_title[match(x, whyte_vars$var_id)]
#   whyte_vars$var_id_whyte[match(x, whyte_vars$var_id)]
# }