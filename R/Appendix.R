source("01_import.R")
library(flextable)

# Variables ####

whyte_category_angarika %>%
  filter(!is.na(category_whyte)) %>%
  filter(!is.na(var_id)) %>%
  flextable() %>%
  set_header_labels(
    var_id = "SCCS ID", code = "Code", full_name = "Full name",
    short_name = "Short name", category_whyte = "Whyte domain",
    our_category = "Our category") %>%
  save_as_docx(path = "draft/Appendix_TableS1_variables.docx")

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
             path = "draft/Appendix_TableS1_variables.docx")
