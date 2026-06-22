# Runs PCoA on prepared ordinal matrix

source("R/02a_pcoa_data_prep.R")  # provides: mat_ord_fixed, ordinal_cols, binary_cols

gower_ordinal <- cluster::daisy(
  mat_ord_fixed,
  metric = "gower",
  type = list(ordratio = ordinal_cols, symm = binary_cols)
)

pcoa_result <- ape::pcoa(gower_ordinal)

saveRDS(pcoa_result, here::here("output/pcoa_result"))