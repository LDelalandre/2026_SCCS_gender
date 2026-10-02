source("R/01_import.R")
library(flextable)

category_colors <- c(
  "Gender Ideology"       = "#66C2A5",
  "Gendered violence"     = "#FC8D62",
  "Politics"              = "#8DA0CB",
  "Family & Household"    = "#E78AC3",
  "Subsistence & Economy" = "#A6D854",
  "Sex & Marriage"        = "#FFD92F",
  "Religion & Rituals"    = "#E5C494"
)

# Table response variables ####

whyte_category_angarika %>%
  filter(!is.na(category_whyte)) %>%
  filter(!is.na(var_id)) %>%
  write.csv2(., "draft/tbl/appendix_gender_variables.csv", row.names = F)

library(flextable)
library(officer)

variable_coverage <- read_csv("output/02a_variable_coverage.csv") %>% 
  mutate(coverage = round(coverage, digits = 2))

direction_table_clean <- read.csv("data/gender_response_variables_recode.csv") %>% 
  select(var_id, women_favorable) %>% 
  transmute(var_id = var_id,
            recoded = if_else(women_favorable == -1, "Yes", "No"))
  
codes_max_values <- codes %>% 
  filter(var_id %in% whyte_category_angarika$var_id) %>% 
  group_by(var_id) %>% 
  mutate(min_value = min(code, na.rm = T),
        max_value = max(code, na.rm = T)) %>% 
  filter(code == max_value | code == min_value) %>% 
  filter(code == max_value) %>% 
  select(var_id, name, max_value)


table_resp <- whyte_category_angarika %>%
  filter(!is.na(category_whyte)) %>%
  filter(!is.na(var_id)) %>%
  left_join(codes_max_values) %>% 
  left_join(direction_table_clean) %>% 
  left_join(variable_coverage) %>% 
  mutate(category_Angarika = factor(category_Angarika, levels = names(category_colors))) %>%
  arrange(category_Angarika, title_shortened) %>% 
  arrange(category_Angarika, title_shortened) %>% 
  select(var_id, var_id_whyte, title_shortened, title, name, max_value, 
         category_Angarika, category_whyte, everything()) %>% 
  flextable() %>%
  set_header_labels(
    var_id = "ID_SCCS",
    var_id_whyte = "ID_Whyte", 
    title_shortened = "Short name",
    title = "Full name",
    max_value = "Highest value",
    name = "Description of the highest value",
    category_whyte = "Whyte's category",
    category_Angarika = "Our category",
    recoded = "Coding direction changed",
    coverage = "Coverage"
  ) %>%
  fontsize(size = 7) %>%
  fontsize(size = 8, part = "header") %>% 
  width(j = 1, width = 1.7, "cm") %>%
  width(j = 2, width = 1.3, "cm") %>%
  width(j = 3, width = 2, "cm") %>%
  width(j = 4, width = 3, "cm") %>%
  
  width(j = 5, width = 2, "cm") %>%
  width(j = 6, width = 1, "cm") %>%
  
  width(j = 7, width = 2, "cm") %>%
  width(j = 8, width = 1.5, "cm") %>%
  
  width(j = 9, width = 1, "cm") %>%
  width(j = 10, width = 1, "cm") %>%
  padding(padding = 2, part = "all") %>%
  # theme_vanilla()
  theme_zebra %>% 
  set_caption("Table S1: Women's status variables, ordered as in Figure 1. 
  The variables are identified using the 
  Standard Cross-Cultural Sample's scheme (ID_SCCS), and Whyte's original coding (ID_Whyte). 
  Variable short name was coined for the present paper, whereas the full name is provided in D-PLACE. 
  Note that the full name is sometimes 
  unprecise regarding the direction of variable coding. 
  Variables were integers. We provide a description of the highest value of the variables
  (as given in D-PLACE), as well as that value. 
  Most of the time, the variables 
  were coded such as high values indicated higher status of women; when it was not the case, 
  we recoded the variables in 
  the opposite direction. This is indicated in the column `coding direction
  changed`. 
  Finally, variable coverage provides the proportion of societies for which the variable was measured.
  All variables are retrievable from D-PLACE
  (https://d-place.org/contributions/SCCS), using ID_SCCS.")

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
  select(pref_name_for_society, soc_id, Ax1, main_focal_year, region6, origLat, origLong,
         HRAF_name_ID, HRAF_link) %>% 
  mutate(Ax1 = round(Ax1, digits = 3))%>%
  mutate(HRAF_link = utils::URLencode(HRAF_link, repeated = TRUE)) %>% # for URLs
  left_join(coords_matched_86 %>% select(soc_id, phylo))

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
    HRAF_link = "HRAF URL",
    phylo = "Society name in the phylogenetic tree"
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
  width(j = 10, width = 1.2, unit = "cm") %>%
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
                "warfare", "matri")

predictors_detailed %>% 
  left_join(variables %>% select(var_id, source), by = "var_id") %>% 
  write.csv(., "draft/tbl/appendix_predictors.csv", row.names = F)
#TODO: add some more information (dataset of origin, etc.)

## Predictors loadings ####

predictors_detailed_tomerge <- predictors_detailed %>% 
  select(title, var_id, category) %>% 
  mutate(Analysis = case_when(category == "hunting" ~ "Raw variable used",
                              category == "warfare" ~ "Raw variable used",
                              var_id %in% c("SCCS708", "SCCS680", "SCCS681",
                                            "SCCS706", "SCCS707") ~ "Technological intensification scale",
                              TRUE ~ "PCoA"))

category_order <- c( "warfare", "hunting", 
                     "technology",
                     "kinship", "complexity",
                     "agriculture", "herding")
tech_order <- c("Metalworking", "Manufacture of Pottery", "True Weaving", "Irrigation", "Plow")

table_loadings <- pred_loadings %>%
  mutate(across(Co1:Co4, ~ round(.x, 3))) %>%
  # mutate(Analysis = "PCoA") %>% 
  select(-category) %>%
  select(-Co3, -Co4) %>% 
  right_join(predictors_detailed_tomerge) %>% 
  rename(
    Category   = category,
    Variable   = title,
    ID = var_id,
    ID = var_id,
    `Axis 1`   = Co1,
    `Axis 2`   = Co2
  ) %>% 
  select(Category, Variable, ID, Analysis, `Axis 1`, `Axis 2`) %>%
  mutate(
    Category = factor(Category, levels = category_order),  # preserve existing category order
    sort_key = if_else(Category == "technology",
                       match(Variable, tech_order),          # custom order for technology
                       row_number())                          # keep original order for everything else
  ) %>%
  arrange(Category, sort_key) %>%
  select(-sort_key) %>%
  mutate(Category = as.character(Category)) %>%  # back to character for flextable
  filter(!is.na(Category)) %>% 
  mutate(Category = if_else(Category == "kinship", "lineage/locality", Category))

codes_max_values_pred <- codes %>% 
  filter(var_id %in% table_loadings$ID) %>% 
  group_by(var_id) %>% 
  mutate(min_value = min(code, na.rm = T),
         max_value = max(code, na.rm = T)) %>% 
  filter(code == max_value | code == min_value) %>% 
  filter(code == max_value) %>% 
  select(var_id, name, max_value) %>% 
  mutate(name = if_else(var_id == "SCCS730", "Dominant, the principal subsistence activity", name))
  


table_loadings2 <- table_loadings %>% 
  left_join(codes_max_values_pred %>% rename(ID = var_id)) %>% 
  select(Category, Variable, ID, name, max_value, everything()) %>% 
  rename(`Description of the highest value` = name) %>% 
  rename(`Highest value` = max_value)

# Build flextable
predictors_loadings <- flextable(table_loadings2) %>%
  # Merge repeated category cells vertically
  merge_v(j = "Category") %>%
  valign(j = "Category", valign = "top") %>%
  # Bold header
  bold(part = "header") %>%
  # Shade alternating categories for readability
  bg(i = ~ Category %in% unique(Category)[c(TRUE, FALSE)],
     bg = "#F2F2F2", part = "body") %>%
  # # Highlight strong loadings (|loading| > 0.5)
  # color(i = ~ abs(`Axis 1`) > 0.5, j = "Axis 1", color = "#C00000") %>%
  # color(i = ~ abs(`Axis 2`) > 0.5, j = "Axis 2", color = "#C00000")
  # Column widths
  width(j = "Category", width = 1) %>%
  width(j = "Variable", width = 2) %>%
  width(j = "ID", width = 0.8) %>%
  width(j = "Description of the highest value", width = 1.5) %>%
  width(j = "Highest value", width = 0.5) %>%
  width(j = "Analysis", width = 1) %>%
  width(j = c("Axis 1", "Axis 2"), width = 0.6) %>%
  # Font
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  # Caption
  set_caption("Table S3: Predictor variables, and their grouping in 
  theoretically-informed categories. Variables are categorical, ordinal.
  As in Table S1, the higest value of a variable, and the description of it, are 
  provided, to provide an understanding of the coding and its direction.
  Synthetic predictors were defined for several categories.
  The method used is specified in the column `Analysis`, which can be:
  i) using the raw variable when there is one variable in the category;
  ii) building a Guttman scale (cumulative scale), for the technology category;
  iii) doing a PCoA with variables of the category and retain the main first or two first axes.
  In this case, columns Axis 1 and Axis 2 represent the
              correlations between a variable and principal coordinate axis 1 and 2, respectively.
              
  NB: variable SCCS730, approximating reliance on hunting, was recoded in the opposite direction
  than what is described on D-place, so that high value meant high reliance on hunting.
              ")

save_as_docx(predictors_loadings, path = "draft/tbl/appendix_predictors_loadings.docx")


## Correlation among predictors ####

library(corrplot)
predictors_selected <- pred_resp %>% dplyr::select(all_of(predictors))

cor_matrix <- cor(predictors_selected, use = "pairwise.complete.obs")

# Format as clean table with stars
# Fix: vectorize the function
format_cor <- function(mat) {
  out <- matrix("", nrow = nrow(mat), ncol = ncol(mat))
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      r <- mat[i, j]
      if (i == j) { out[i, j] <- "—"; next }
      stars <- if (abs(r) >= 0.5) "***" else if (abs(r) >= 0.4) "**" else
        if (abs(r) >= 0.3) "*"  else if (abs(r) >= 0.2) "†"  else ""
      out[i, j] <- paste0(sprintf("%.2f", r), stars)
    }
  }
  as.data.frame(out, row.names = rownames(mat))
}

cor_table_pred <- as.data.frame(format_cor(cor_matrix))
colnames(cor_table_pred) <- colnames(cor_matrix)
rownames(cor_table_pred) <- rownames(cor_matrix)

# Short names for readability
short_names <- c(
  "complexity"             = "Complexity",
  "technology"             = "Technology",
  "agriculture_cereals"    = "Agri. type",
  "agriculture_importance" = "Agri. importance",
  "herding_importance"     = "Herding",
  "herding_milk"           = "Herding milk",
  "hunting"                = "Hunting",
  "warfare"                = "Warfare",
  "matri"                  = "Matrilineality"
)

rownames(cor_table_pred) <- short_names[rownames(cor_table_pred)]
colnames(cor_table_pred) <- short_names[colnames(cor_table_pred)]

# Upper triangle only
cor_table_pred[lower.tri(cor_table_pred, diag = TRUE)] <- ""

table_pred_correlations <- cor_table_pred %>% 
  rownames_to_column("Variable category") %>% 
  flextable() %>% 
  set_caption("Table S4: Correlations among synthetic predictors.")


save_as_docx(table_pred_correlations, path = "draft/tbl/table_pred_correlations.docx")


## Correlations between pred and resp ####

axes <- paste0("Axis.", 1:5)

# compute spearman correlations with p-values
cor_table <- map_dfr(predictors, function(pred) {
  map_dfc(axes, function(ax) {
    if (ax %in% names(pred_resp) & pred %in% names(pred_resp)) {
      test <- cor.test(pred_resp[[pred]], pred_resp[[ax]],
                       method = "spearman",
                       use = "complete.obs")
      tibble(!!ax := sprintf("%.2f%s",
                             test$estimate,
                             case_when(test$p.value < 0.001 ~ "***",
                                       test$p.value < 0.01  ~ "**",
                                       test$p.value < 0.05  ~ "*",
                                       test$p.value < 0.10  ~ "†",
                                       TRUE                 ~ "")))
    } else {
      tibble(!!ax := NA_character_)
    }
  }) %>%
    mutate(predictor = pred, .before = 1)
})

table_cor_pred_axes <- cor_table %>% 
  flextable() %>% 
  set_caption("Table S5: Correlation between synthetic predictors and 
              the axes of the PCoA on women status variables. 
              Significance of Spearman correlation test: † p<0.10, * p<0.05, 
              ** p<0.01, *** p<0.001).")

save_as_docx(table_cor_pred_axes, path = "draft/tbl/table_cor_pred_axes.docx")

## Control variables ####

predictors_detailed %>% 
  filter(category == "method")


## Effect of technology ####

library(broom)

models <- list(
  "Agriculture type" = lm(Axis.1 ~ technology + agriculture_cereals, data = pred_resp),
  "Herding"          = lm(Axis.1 ~ technology + herding_importance,  data = pred_resp),
  "Hunting"          = lm(Axis.1 ~ technology + hunting,          data = pred_resp),
  "Complexity"       = lm(Axis.1 ~ technology + complexity,          data = pred_resp),
  "Matrilineality"   = lm(Axis.1 ~ technology + matri,               data = pred_resp)
)

fmt_p <- function(p) ifelse(p < 0.001, "<0.001", as.character(round(p, 3)))

mod_technology <- map_dfr(names(models), function(nm) {
  coefs <- tidy(models[[nm]]) %>% filter(term != "(Intercept)")
  
  tech  <- coefs %>% filter(term == "technology")
  other <- coefs %>% filter(term != "technology")
  
  data.frame(
    `Technology β`         = round(tech$estimate, 3),
    `Technology p`         = fmt_p(tech$p.value),
    `Added variable` = nm,
    `Added variable β`     = round(other$estimate, 3),
    `Added variable p`     = fmt_p(other$p.value),
    check.names = FALSE
  )
})

table_technology <- mod_technology %>% 
  flextable() %>% 
  set_caption("Table S6: Effect of technology in pair with other predictors.")

save_as_docx(table_technology, path ="draft/tbl/table_technology.docx")


## One PCOA ####

### Loadings ####

table_loading_one_pcoa <- loadings_all_predictors %>% 
  mutate_at(vars(pred1, pred2, pred3, pred4), funs(round(., 2))) %>% 
  arrange(pred1) %>% 
  flextable() %>% 
  set_header_labels(
    var_id = "Variable ID",
    title = "Variable",
    pred1 = "Axis 1",
    pred2 = "Axis 2",
    pred3 = "Axis 3",
    pred4 = "Axis 4") %>% 
  width(j = 2, width = 3, "cm") %>% 
  width(j = 2, width = 7, "cm") %>% 
  set_caption("Table S7: Covariation of the predictor variables with the first for axes
              of the PCoA made on all predictor variables. Variables arranged by their correlation
              with Axis 1, capturing nearly 40% of the variance (see Fig S9). 
              The first dimension captures variables linked to social complexity. 
              The second relates to the frequency of intercommunity armed conflicts, 
              the third to matrilocality/lineality, 
              and the fourth separates societies based on the type of agriculture they rely on
              (cereals vs. root, tubers, and tree fruits). ")


### Model ####

table_lm_one_pcoa_clean <- table_lm_one_pcoa %>% 
  flextable() %>% 
  width(j = 1, width = 4, "cm") %>% 
  width(j = 2, width = 4, "cm") %>% 
  set_header_labels(
    Variable = "Axis of PCoA on predictor variables",
    `HR [CI]` = "Coefficient [95% confidence interval]",
    p = "p-value"
  ) %>% 
  set_caption("Table S8: summary table of a frequentist model predicting Women's Status Index
  by the firt four axes of the PCoA made on all the predictor variables (pred1 to pred4).
  pred1 captures social complexity, pred2 the frequency of intercommunity armed conflict,
  pred3 matrilineality/locality, and pred4 the type of agriculture (see Table S7).
  Consistently with the main analysis presented in the article, complexity and matrilineality/locality 
  appeared to be the two significant predictors of women's status.")


# Save all ####

save_as_docx(
  table_resp,
  table_societies,
  predictors_loadings,
  
  table_pred_correlations,
  table_cor_pred_axes,
  
  table_loading_one_pcoa,
  table_lm_one_pcoa_clean,
  
  # table_technology,
  
  pr_section = officer::prop_section(
    page_size = officer::page_size(orient = "portrait")
  ),
  path = "draft/tbl/appendix_tables.docx"
)



