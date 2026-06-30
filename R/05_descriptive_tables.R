source("R/01_import.R")
library(flextable)

# Table response variables ####

whyte_category_angarika %>%
  filter(!is.na(category_whyte)) %>%
  filter(!is.na(var_id)) %>%
  write.csv2(., "draft/tbl/appendix_gender_variables.csv", row.names = F)

library(flextable)
library(officer)

table_resp <- whyte_category_angarika %>%
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
  fontsize(size = 8, part = "header") %>% 
  width(j = 1, width = 1.7, "cm") %>%
  width(j = 2, width = 1.7, "cm") %>%
  width(j = 3, width = 5, "cm") %>%
  width(j = 4, width = 3, "cm") %>%
  width(j = 5, width = 1.5, "cm") %>%
  width(j = 6, width = 3, "cm") %>%
  padding(padding = 2, part = "all") %>%
  # theme_vanilla()
  theme_zebra %>% 
  set_caption("Table S1: the 52 variables approximating the status of women in 
  preindustrial societies measured by Whyte. The variable ID used in the 
  Standard Cross Cultural Sample and in Whyte's original data are given in the 
  first two columns, respectively. In addition to the full name of the variables, 
  we defined a short name to ease visualization. 
  Finally, the categorization used by Whyte and by us are given.", 
              fp_p = fp_par(text.align = "left"),
              fp_t = fp_text(bold = FALSE, italic = FALSE))

table_resp

save_as_docx(table_resp, 
             pr_section = officer::prop_section(
               page_size = officer::page_size(orient = "portrait")
             ),
             path = "draft/tbl/Appendix_TableS1_variables.docx")

# Table societies ####

society_profile <- societies %>%
  select(soc_id, main_focal_year,HRAF_name_ID, HRAF_link)

coord_societies <- coords_df %>%
  left_join(society_profile, by = "soc_id") %>%
  # left_join(context_wide, by = "soc_id") %>% 
  arrange(Ax1) %>% 
  select(pref_name_for_society, soc_id, Ax1, main_focal_year, region, origLat, origLong,
         HRAF_name_ID, HRAF_link) %>% 
  mutate(Ax1 = round(Ax1, digits = 3))%>%
  mutate(HRAF_link = utils::URLencode(HRAF_link, repeated = TRUE)) # for URLs


table_societies<- coord_societies %>%
  flextable() %>%
  set_header_labels(
    pref_name_for_society = "Society",
    soc_id = "SCCS ID",
    Ax1 = "Loading on PCoA Axis 1",
    main_focal_year = "Focal year",
    region = "Region", 
    origLat = "Latitude",
    origLong = "Longitude",
    HRAF_name_ID = "HRAF name (ID)",
    HRAF_link = "HRAF URL"
  ) %>%
  fontsize(size = 7) %>%
  fontsize(size = 8, part = "header") %>%
  width(j = 1, width = 1.4, unit = "cm") %>%
  width(j = 2, width = 1.4, unit = "cm") %>%
  width(j = 3, width = 1.4, unit = "cm") %>%
  width(j = 4, width = 1.4, unit = "cm") %>%
  width(j = 5, width = 1.4, unit = "cm") %>%
  width(j = 6, width = 1.4, unit = "cm") %>%
  width(j = 7, width = 1.4, unit = "cm") %>%
  width(j = 8, width = 1.4, unit = "cm") %>%
  width(j = 9, width = 5, unit = "cm") %>%
  padding(padding = 2, part = "all") %>%
  theme_zebra() %>% 
  compose(
    j = "HRAF_link",
    value = as_paragraph(
      hyperlink_text(x = HRAF_link, url = HRAF_link)
    )
  ) %>% 
  set_caption("Table S2: Societies analyzed in this study.")

table_societies

save_as_docx(table_societies, 
             pr_section = officer::prop_section(
               page_size = officer::page_size(orient = "portrait")
             ),
             path = "draft/tbl/appendix_table_societies.docx")

# Predictors ####

predictors <- c("complexity", "technology", "agriculture_cereals", "agriculture_importance",
                "herding_importance", "herding_milk", "hunting",
                "warfare_intensity", "warfare_location", "matri")


# 0. List of variables retained ####

predictors_detailed %>% 
  left_join(variables %>% select(var_id, source), by = "var_id") %>% 
  write.csv(., "draft/tbl/appendix_predictors.csv", row.names = F)
#TODO: add some more information (dataset of origin, etc.)

## Predictors loadings ####

# Round loadings and rename columns cleanly
table_loadings <- pred_loadings %>%
  mutate(across(Co1:Co4, ~ round(.x, 3))) %>% 
  left_join(predictors_detailed %>% select(title, var_id)) %>%
  rename(
    Category   = category,
    Variable   = title,
    ID = var_id,
    `Axis 1`   = Co1,
    `Axis 2`   = Co2,
    `Axis 3`   = Co3,
    `Axis 4`   = Co4
  ) %>% 
  select(Category, Variable, ID, everything()) 

# Build flextable
predictors_loadings <- flextable(table_loadings) %>%
  # Merge repeated category cells vertically
  merge_v(j = "Category") %>%
  valign(j = "Category", valign = "top") %>%
  # Bold header
  bold(part = "header") %>%
  # Shade alternating categories for readability
  bg(i = ~ Category %in% unique(Category)[c(TRUE, FALSE)],
     bg = "#F2F2F2", part = "body") %>%
  # Highlight strong loadings (|loading| > 0.5)
  color(i = ~ abs(`Axis 1`) > 0.5, j = "Axis 1", color = "#C00000") %>%
  color(i = ~ abs(`Axis 2`) > 0.5, j = "Axis 2", color = "#C00000") %>%
  color(i = ~ abs(`Axis 3`) > 0.5, j = "Axis 3", color = "#C00000") %>%
  color(i = ~ abs(`Axis 4`) > 0.5, j = "Axis 4", color = "#C00000") %>%
  # Column widths
  width(j = "Category", width = 1) %>%
  width(j = "Variable", width = 2.7) %>%
  width(j = "ID", width = 0.8) %>%
  width(j = c("Axis 1", "Axis 2", "Axis 3", "Axis 4"), width = 0.6) %>%
  # Font
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  # Caption
  set_caption("Table S1. PCoA variable loadings by category. Values represent correlations between each variable and the principal coordinate axes. Strong loadings (|r| > 0.5) are shown in red.")

# Save to Word
save_as_docx(predictors_loadings, path = "draft/tbl/appendix_predictors_loadings.docx")


save_as_docx(
  table_resp,
  table_societies,
  predictors_loadings,
  pr_section = officer::prop_section(
    page_size = officer::page_size(orient = "portrait")
  ),
  path = "draft/tbl/appendix.docx"
)



