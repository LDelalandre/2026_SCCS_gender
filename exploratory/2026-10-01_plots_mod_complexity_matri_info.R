## 0. Requirements ####

# Run plot_complexity_matri_axis1.R first. This script reuses its objects:
#   mod_axis1_phylo, plot_data, lev, draws, draws_wide,
#   r2_marg, r2_cond, beta_q, lambda_q, contrast_results
library(tidyverse)
library(posterior)


## 1. Convergence checks (report in Methods / Supplement) ####

mod_diag <- summarise_draws(mod_axis1_phylo, "rhat", "ess_bulk", "ess_tail")
mod_diag %>% filter(!str_detect(variable, "^(r_|lp__|lprior)"))   # skip taxon-level terms

max(mod_diag$rhat, na.rm = TRUE)       # want < 1.01
min(mod_diag$ess_bulk, na.rm = TRUE)   # want > 400 (ideally > 1000)
# Also: pp_check(mod_axis1_phylo)


## 2. Population-level effects (beta) with pd ####

pd_fun <- function(x) max(mean(x > 0), mean(x < 0))

b_cols <- grep("^b_", names(draws), value = TRUE)

effects_tbl <- map_dfr(b_cols, function(p) {
  x <- draws[[p]]
  tibble(
    parameter = p,
    estimate  = mean(x),
    lower     = quantile(x, 0.025),
    upper     = quantile(x, 0.975),
    pd        = pd_fun(x)
  )
})

effects_tbl


## 3. R2 and phylogenetic signal ####

fit_tbl <- tibble(
  metric   = c("R2 marginal", "R2 conditional", "lambda"),
  estimate = c(r2_marg[1, "Estimate"], r2_cond[1, "Estimate"], lambda_q[2]),
  lower    = c(r2_marg[1, "Q2.5"],     r2_cond[1, "Q2.5"],     lambda_q[1]),
  upper    = c(r2_marg[1, "Q97.5"],    r2_cond[1, "Q97.5"],    lambda_q[3])
) %>%
  mutate(across(where(is.numeric), as.numeric))

fit_tbl


## 4. Contrasts between categories (with optional ROPE) ####

# ROPE: differences smaller than 10% of the SD of Axis 1 are treated as
# practically equivalent to zero (Kruschke's default). Adjust if you have a
# better, domain-based threshold.
rope_width <- 0.1 * sd(plot_data$Axis.1, na.rm = TRUE)

contrast_tbl <- contrast_results %>%
  rowwise() %>%
  mutate(
    p_in_rope = mean(abs(draws_wide[[group2]] - draws_wide[[group1]]) < rope_width)
  ) %>%
  ungroup() %>%
  select(contrast, estimate, lower, upper, pd, p_in_rope)

contrast_tbl


## 5. Export tables ####

write_csv(effects_tbl,  "table_effects_axis1.csv")
write_csv(fit_tbl,      "table_fit_axis1.csv")
write_csv(contrast_tbl, "table_contrasts_axis1.csv")


## 6. Auto-generated text for the Results ####

ci   <- function(est, lo, hi, d = 2) sprintf("%.*f [%.*f, %.*f]", d, est, d, lo, d, hi)
pd_t <- function(p) sprintf("pd = %.2f", p)

# Helper to pull one contrast by its levels
get_c <- function(a, b) contrast_tbl %>% filter(contrast == paste(a, "-", b))

c_int_non  <- get_c(lev[2], lev[1])
c_mat_non  <- get_c(lev[3], lev[1])
c_mat_int  <- get_c(lev[3], lev[2])

pd_beta <- pd_fun(draws$b_complexity)

text_results <- paste0(
  "Axis 1 was ", ifelse(beta_q[2] > 0, "positively", "negatively"),
  " associated with social complexity (β = ",
  ci(beta_q[2], beta_q[1], beta_q[3]), ", ", pd_t(pd_beta), "). ",
  "The model explained ",
  sprintf("%.0f%%", 100 * fit_tbl$estimate[1]), " of the variance through the fixed effects ",
  "(marginal R² = ", ci(fit_tbl$estimate[1], fit_tbl$lower[1], fit_tbl$upper[1]), ") and ",
  sprintf("%.0f%%", 100 * fit_tbl$estimate[2]), " when including the phylogenetic term ",
  "(conditional R² = ", ci(fit_tbl$estimate[2], fit_tbl$lower[2], fit_tbl$upper[2]), "). ",
  "The phylogenetic signal was λ = ",
  ci(fit_tbl$estimate[3], fit_tbl$lower[3], fit_tbl$upper[3]), ".\n\n",
  
  "Posterior contrasts showed where the differences among kinship categories lie. ",
  lev[2], " differed from ", lev[1], " (Δ = ",
  ci(c_int_non$estimate, c_int_non$lower, c_int_non$upper), ", ", pd_t(c_int_non$pd), "), ",
  lev[3], " differed from ", lev[1], " (Δ = ",
  ci(c_mat_non$estimate, c_mat_non$lower, c_mat_non$upper), ", ", pd_t(c_mat_non$pd), "), ",
  "and ", lev[3], " differed from ", lev[2], " (Δ = ",
  ci(c_mat_int$estimate, c_mat_int$lower, c_mat_int$upper), ", ", pd_t(c_mat_int$pd), "). ",
  "For the ", lev[3], "–", lev[2], " contrast, ",
  sprintf("%.0f%%", 100 * c_mat_int$p_in_rope),
  " of the posterior fell within the region of practical equivalence."
)

cat(text_results)

# NOTE: the sentence above is a template. Edit the verbs ("differed from",
# "did not clearly differ from") according to the CrIs: a contrast whose 95% CrI
# includes 0 should NOT be described as a difference.