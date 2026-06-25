library(tidyverse)


predictors <- c("complexity", "technology", "agriculture_cereals", "agriculture_importance",
                "herding_importance", "herding_milk", "hunting",
                "warfare_intensity", "warfare_location", "matri")


# 0. List of variables retained ####

predictors_detailed %>% 
  left_join(variables %>% select(var_id, source), by = "var_id") %>% 
  write.csv(., "draft/tbl/appendix_predictors.csv", row.names = F)
#TODO: add some more information (dataset of origin, etc.)

# 1. Correlation among predictors ####

library(corrplot)
predictors_selected <- pred_resp %>% dplyr::select(all_of(predictors))
cor(predictors_selected, use = "pairwise.complete.obs") %>%
  corrplot(method = "number")

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

write.csv(cor_table_pred, "draft/tbl/appendix_predictor_correlations.csv")



# 2. Correlations between pred and resp ####

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

print(cor_table)

write.csv(cor_table, "draft/tbl/appendix_correlation_predictors_axes.csv", row.names = F)

library(knitr)
library(kableExtra)

cor_table %>%
  kable(caption = "Spearman correlations between predictors and response PCoA axes",
        col.names = c("Predictor", paste0("Axis ", 1:5)),
        align = c("l", rep("c", 5))) %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE) %>%
  kableExtra::footnote(general = "† p<0.10, * p<0.05, ** p<0.01, *** p<0.001")



# 3. Models ####

library(broom)
library(tidyverse)

models <- list(
  "Agriculture type" = lm(Axis.1 ~ technology + agriculture_cereals, data = pred_resp),
  "Herding"          = lm(Axis.1 ~ technology + herding_importance,  data = pred_resp),
  "Hunting"          = lm(Axis.1 ~ technology + hunting,          data = pred_resp),
  "Complexity"       = lm(Axis.1 ~ technology + complexity,          data = pred_resp),
  "Matrilineality"   = lm(Axis.1 ~ technology + matri,               data = pred_resp)
)

fmt_p <- function(p) ifelse(p < 0.001, "<0.001", as.character(round(p, 3)))

table_final <- map_dfr(names(models), function(nm) {
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

print(table_final)

# Save
write.csv(table_final, 
          here::here("draft/tbl/appendix_technology_mediation_table.csv"), 
          row.names = FALSE)
