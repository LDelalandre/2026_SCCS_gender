library(tidyverse)





# Predictors PCoA ####
# write.csv(predictors_detailed, "output/predictors_detailed.csv")
# write.csv(pred_loadings, "output/pred_loadings.csv")


library(flextable)
library(dplyr)






# 2. Correlations between pred and resp ####



library(knitr)
library(kableExtra)

cor_table %>%
  kable(caption = "Spearman correlations between predictors and response PCoA axes",
        col.names = c("Predictor", paste0("Axis ", 1:5)),
        align = c("l", rep("c", 5))) %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE) %>%
  kableExtra::footnote(general = "† p<0.10, * p<0.05, ** p<0.01, *** p<0.001")




