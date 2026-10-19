suppressMessages({library(readr); library(dplyr)})
out <- "Results/draft1944_pre_draft_trajectories"
coefs <- read_csv(file.path(out, "replacement_margin_coefficients.csv"), show_col_types = FALSE)
ratios <- read_csv(file.path(out, "replacement_margin_ratios.csv"), show_col_types = FALSE)
cells <- read_csv(file.path(out, "replacement_1944_analysis_cells.csv"), show_col_types = FALSE)
for (sample_label in ratios$sample) {
  b <- filter(coefs, sample == sample_label)
  b <- setNames(b$estimate, b$variable)
  r <- filter(ratios, sample == sample_label)
  stopifnot(abs(b['n_internal_total'] - sum(b[c('n_retention', 'n_transfer_same_dept', 'n_transfer_diff_dept')])) < 1e-8,
            abs(r$internal_external_ratio - b['n_internal_total']/b['n_new_hires']) < 1e-12)
}
# Independent dummy-variable OLS in stats::lm, rather than fixest's absorption.
# Singleton FE cells have no influence on the treatment slope.
for (y in unique(coefs$variable)) {
  m <- lm(as.formula(paste(y, "~ n_drafted_male + log_cumul_male + factor(ka_id) + factor(pos_norm)")), data = cells)
  target <- filter(coefs, sample == '1944', variable == y)$estimate
  stopifnot(length(target) == 1, abs(unname(coef(m)['n_drafted_male']) - target) < 1e-8)
}
stopifnot(all(cells$year_num == 1944),
          all(cells$n_internal_total == cells$n_retention + cells$n_transfer_same_dept + cells$n_transfer_diff_dept),
          all(abs(cells$log_cumul_male - log1p(cells$cumul_n_male)) < 1e-12))
cat("PASS: independent OLS agrees for all five 1944 coefficients; ratios and decomposition agree.\n")
