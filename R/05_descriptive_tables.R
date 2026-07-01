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
  Finally, the categorization used by Whyte and by us are given.")

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
  set_caption("Table S3: PCoA variable loadings by category. Values represent 
              correlations between each variable and the principal coordinate axes. 
              Strong loadings (|r| > 0.5) are shown in red.")

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
  "warfare_intensity"      = "Warfare intensity",
  "warfare_location"       = "Warfare location",
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


# Save all ####

save_as_docx(
  table_resp,
  table_societies,
  predictors_loadings,
  
  table_pred_correlations,
  table_cor_pred_axes,
  
  table_technology,
  
  pr_section = officer::prop_section(
    page_size = officer::page_size(orient = "portrait")
  ),
  path = "draft/tbl/appendix_tables.docx"
)



