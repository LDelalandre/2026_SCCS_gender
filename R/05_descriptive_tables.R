source("R/01_import.R")
library(flextable)

# Table response variables ####

whyte_category_angarika %>%
  filter(!is.na(category_whyte)) %>%
  filter(!is.na(var_id)) %>%
  write.csv2(., "draft/tbl/appendix_gender_variables.csv", row.names = F)

library(flextable)
library(officer)

ft <- whyte_category_angarika %>%
  filter(!is.na(category_whyte)) %>%
  filter(!is.na(var_id)) %>%
  flextable() %>%
  set_header_labels(
    var_id = "SCCS ID",
    code = "Code", 
    full_name = "Full name",
    short_name = "Short name",
    category_whyte = "Whyte domain",
    our_category = "Our category"
  ) %>%
  fontsize(size = 8) %>%
  width(j = 1, width = 1.5) %>%
  width(j = 2, width = 1) %>%
  width(j = 3, width = 5) %>%
  width(j = 4, width = 3) %>%
  width(j = 5, width = 2) %>%
  width(j = 6, width = 2) %>%
  theme_vanilla()

save_as_docx(ft, path = "draft/Appendix_TableS1_variables.docx")

ft <- whyte_category_angarika %>%
  filter(!is.na(category_whyte)) %>%
  filter(!is.na(var_id)) %>%
  flextable() %>%
  set_header_labels(
    var_id = "SCCS ID",
    code = "Code", 
    full_name = "Full name",
    short_name = "Short name",
    category_whyte = "Whyte domain",
    our_category = "Our category"
  ) %>%
  fontsize(size = 7) %>%
  width(j = 1, width = 1) %>%
  width(j = 2, width = 0.8) %>%
  width(j = 3, width = 4) %>%
  width(j = 4, width = 2.5) %>%
  width(j = 5, width = 1.5) %>%
  width(j = 6, width = 1.5) %>%
  padding(padding = 2, part = "all") %>%
  theme_vanilla()

save_as_docx(ft, 
             pr_section = officer::prop_section(
               page_size = officer::page_size(orient = "landscape")
             ),
             path = "draft/tbl/Appendix_TableS1_variables.docx")

# Table societies ####
# 
# 
# # context_vars <- c(
# #   "SCCS1",   # Subsistence economy
# #   "SCCS4",   # Settlement pattern
# #   "SCCS5",   # Community size
# #   "SCCS6",   # Jurisdictional hierarchy
# #   "SCCS156", # Region
# #   "SCCS157"  # Subsistence economy (Murdock)
# # )
# 

# 
# # Check which exist in your data
# variables %>% 
#   filter(var_id %in% context_vars) %>%
#   select(var_id, title) %>%
#   print()
# 
# context_wide <- sccs %>%
#   filter(var_id %in% context_vars) %>%
#   group_by(soc_id, var_id) %>%
#   slice_max(year, na_rm = TRUE, n = 1) %>%
#   ungroup() %>%
#   select(soc_id, var_id, code) %>%
#   pivot_wider(names_from = var_id, values_from = code) %>%
#   # join readable names for context variables
#   rename_with(~ variables$title[match(.x, variables$var_id)],
#               any_of(context_vars)) 
# 
# # ── Master coords table with all metadata ────────────────────────────────────

society_profile <- societies %>%
  select(soc_id, main_focal_year,HRAF_name_ID, HRAF_link)

coord_societies <- coords_df %>%
  left_join(society_profile, by = "soc_id") %>%
  # left_join(context_wide, by = "soc_id") %>% 
  arrange(Ax1) %>% 
  select(pref_name_for_society, soc_id, Ax1, main_focal_year, region, origLat, origLong,
         HRAF_name_ID, HRAF_link) %>% 
  mutate(Ax1 = round(Ax1, digits = 3))


# Save with URL recognition

library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "Sheet1")

writeData(wb, "Sheet1", coord_societies)

url_col_index <- which(names(coord_societies) == "HRAF_link")

writeFormula(wb, "Sheet1",
             x = paste0('HYPERLINK("', coord_societies$HRAF_link, '","', coord_societies$HRAF_link, '")'),
             startCol = url_col_index,
             startRow = 2
)

saveWorkbook(wb, here::here("draft/tbl/appendix_coords_societies.xlsx"), overwrite = TRUE)






