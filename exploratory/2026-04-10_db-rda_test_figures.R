scores_combined <- scores_wide %>%
  filter(!is.na(category)) %>%
  left_join(var_loadings_corrected %>% 
              select(var_id_whyte, Axis.1, Axis.2),
            by = "var_id_whyte") %>%
  pivot_longer(
    cols = c(Axis.1, Axis.2, complexity, herding_importance, matri),
    names_to  = "dimension",
    values_to = "value"
  ) %>%
  mutate(
    dimension = factor(dimension,
                       levels = c("Axis.1", "Axis.2",
                                  "complexity", "herding_importance", "matri"),
                       labels = c("PCoA Axis 1", "PCoA Axis 2",
                                  "Complexity", "Herding", "Matrilineality")),
    type = ifelse(grepl("PCoA", dimension), "Structure", "Predictors")
  )

ggplot(scores_combined,
       aes(x = dimension, y = title, fill = value)) +
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", limits = c(-0.6, 0.6)) +
  facet_grid(category ~ type, scales = "free", space = "free") +
  theme_bw(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 0)
  ) +
  labs(x = NULL, y = NULL,
       fill = "Loading /\npartial r",
       title = "Structure and predictors of gender inequality")
