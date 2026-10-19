################################################################################
# OLS: number of postwar-returning 1944 draftees on 1944 office-cell size
# Unit: origin office x kakari x position
################################################################################

suppressMessages({
  library(dplyr)
  library(fixest)
  library(readr)
})

SOURCE_SCRIPT <- file.path(
  Sys.getenv("USERPROFILE"), "Documents", "GitHub", "Tokyo_Project_Figures",
  "Regressions", "run_replacement_margins.R"
)
OUT_DIR <- file.path("Results", "returning_workers_on_office_size")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Reuse the established cell construction and baseline-employment measure.
source(SOURCE_SCRIPT)

available_postwar_years <- sort(intersect(1946:1955, unique(df$year_num)))
postwar_ids <- df %>%
  filter(year_num %in% available_postwar_years) %>%
  distinct(staff_id) %>%
  mutate(returned_postwar = 1L)

returns_by_origin_cell <- df %>%
  filter(year_num == 1944, drafted %in% TRUE, !is_female) %>%
  distinct(staff_id, office_id, kakari, pos_norm) %>%
  left_join(postwar_ids, by = "staff_id") %>%
  mutate(returned_postwar = coalesce(returned_postwar, 0L)) %>%
  group_by(office_id, kakari, pos_norm) %>%
  summarise(
    n_draftees_1944 = n_distinct(staff_id),
    n_returning_workers = sum(returned_postwar),
    .groups = "drop"
  )

analysis <- panel_ka %>%
  filter(year_num == 1944) %>%
  left_join(
    returns_by_origin_cell,
    by = c("office_id", "kakari", "pos_norm")
  ) %>%
  mutate(
    n_draftees_1944 = coalesce(n_draftees_1944, 0L),
    n_returning_workers = coalesce(n_returning_workers, 0L)
  )

m_raw <- feols(n_returning_workers ~ n_workers, analysis,
               cluster = ~office_id)
m_fe <- feols(
  n_returning_workers ~ n_workers + log(cumul_n_male + 1) |
    ka_id + pos_norm,
  analysis,
  cluster = ~office_id
)

m_iv <- feols(
  n_returning_workers ~ log(cumul_n_male + 1) |
    ka_id + pos_norm |
    n_workers ~ n_draftees_1944,
  analysis,
  cluster = ~office_id
)

# Exact design used in the three-slide summary: staffing response is internal
# workers plus external new hires, instrumented by the draft count.
analysis <- analysis %>%
  mutate(replacements = n_internal_total + n_new_hires)
m_replacement_ols <- feols(
  n_returning_workers ~ replacements + log(cumul_n_male + 1) |
    ka_id + pos_norm,
  analysis,
  cluster = ~office_id
)
m_replacement_iv <- feols(
  n_returning_workers ~ log(cumul_n_male + 1) |
    ka_id + pos_norm |
    replacements ~ n_draftees_1944,
  analysis,
  cluster = ~office_id
)
m_replacement_fs <- feols(
  replacements ~ n_draftees_1944 + log(cumul_n_male + 1) |
    ka_id + pos_norm,
  analysis,
  cluster = ~office_id
)

tidy_model <- function(model, label) {
  tibble(
    specification = label,
    dependent_variable = "Number of 1944 draftees returning in 1946-1955",
    independent_variable = "Total workers in 1944 cell, including draftees",
    estimate = unname(coef(model)["n_workers"]),
    std_error = unname(se(model)["n_workers"]),
    p_value = unname(pvalue(model)["n_workers"]),
    observations = nobs(model),
    offices = n_distinct(analysis$office_id)
  )
}

results <- bind_rows(
  tidy_model(m_raw, "Unadjusted"),
  tidy_model(m_fe, "Section and position FE + baseline employment")
)

write_csv(results, file.path(OUT_DIR, "ols_results.csv"))

iv_coef <- unname(coef(m_iv)["fit_n_workers"])
iv_se <- unname(se(m_iv)["fit_n_workers"])
iv_p <- unname(pvalue(m_iv)["fit_n_workers"])
first_stage <- fitstat(m_iv, ~ivf, simplify = TRUE)
iv_results <- tibble(
  dependent_variable = "Number of 1944 draftees returning in 1946-1955",
  endogenous_variable = "Total workers in 1944 cell, including draftees",
  instrument = "Number of male draftees in 1944",
  estimate = iv_coef,
  std_error = iv_se,
  p_value = iv_p,
  first_stage_f = unname(first_stage$stat),
  observations = nobs(m_iv),
  offices = n_distinct(analysis$office_id)
)
write_csv(iv_results, file.path(OUT_DIR, "iv_results.csv"))
replacement_fs <- fitstat(m_replacement_iv, ~ivf, simplify = TRUE)
replacement_results <- bind_rows(
  tibble(
    model = "OLS",
    estimate = unname(coef(m_replacement_ols)["replacements"]),
    std_error = unname(se(m_replacement_ols)["replacements"]),
    p_value = unname(pvalue(m_replacement_ols)["replacements"]),
    first_stage_f = NA_real_
  ),
  tibble(
    model = "IV",
    estimate = unname(coef(m_replacement_iv)["fit_replacements"]),
    std_error = unname(se(m_replacement_iv)["fit_replacements"]),
    p_value = unname(pvalue(m_replacement_iv)["fit_replacements"]),
    first_stage_f = (
      unname(coef(m_replacement_fs)["n_draftees_1944"]) /
        unname(se(m_replacement_fs)["n_draftees_1944"])
    )^2
  )
) %>%
  mutate(
    observations = nobs(m_replacement_iv),
    offices = n_distinct(analysis$office_id),
    first_stage_coefficient = unname(coef(m_replacement_fs)["n_draftees_1944"]),
    first_stage_se = unname(se(m_replacement_fs)["n_draftees_1944"])
  )
write_csv(replacement_results,
          file.path(OUT_DIR, "returnees_on_replacements_ols_iv.csv"))
write_csv(analysis, file.path(OUT_DIR, "analysis_cells.csv"))

cat("Available postwar directories:",
    paste(available_postwar_years, collapse = ", "), "\n")
cat("Returning draftees:", sum(analysis$n_returning_workers), "\n")
cat("1944 draftees represented:", sum(analysis$n_draftees_1944), "\n\n")
print(results)
cat("\nIV RESULTS\n")
print(iv_results)
cat("\nRETURNING WORKERS ON STAFFING RESPONSE: OLS AND IV\n")
print(replacement_results)
