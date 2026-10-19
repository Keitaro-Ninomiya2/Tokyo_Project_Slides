################################################################################
# Staffing-replacement first stage implied by the replacement-margin design
# Unit: destination office x kakari x occupation x year, 1938--1945
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
OUT_DIR <- file.path("Results", "drafting_net_staffing_first_stage")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Builds panel_ka and the established replacement-margin models using the same
# worker classification, fixed effects, baseline control, and sample.
source(SOURCE_SCRIPT)

first_stage_data <- panel_ka %>%
  mutate(
    replacements = n_internal_total + n_new_hires
  )

first_stage_formula <-
  replacements ~ n_drafted_male + log(cumul_n_male + 1) |
    year_num + ka_id + pos_norm

m_first_stage <- feols(
  first_stage_formula,
  data = first_stage_data,
  cluster = ~office_id
)

# Alternative first stage requested: total contemporaneous cell employment.
# Draftees remain in the office-size count; they are not subtracted from the DV.
m_office_size <- feols(
  n_workers ~ n_drafted_male + log(cumul_n_male + 1) |
    year_num + ka_id + pos_norm,
  data = first_stage_data,
  cluster = ~office_id
)

replacement_coef <- unname(coef(m_internal)["n_drafted_male"]) +
  unname(coef(m_new_hires)["n_drafted_male"])

results <- tibble(
  dependent_variable = "Internal workers plus new hires",
  independent_variable = "Number of male draftees",
  estimate = unname(coef(m_first_stage)["n_drafted_male"]),
  std_error = unname(se(m_first_stage)["n_drafted_male"]),
  p_value = unname(pvalue(m_first_stage)["n_drafted_male"]),
  first_stage_f = (estimate / std_error)^2,
  internal_replacement_margin = unname(coef(m_internal)["n_drafted_male"]),
  external_replacement_margin = unname(coef(m_new_hires)["n_drafted_male"]),
  total_replacement_margin = replacement_coef,
  observations = nobs(m_first_stage),
  offices = n_distinct(first_stage_data$office_id)
)

write_csv(results, file.path(OUT_DIR, "replacement_first_stage.csv"))
office_size_results <- tibble(
  dependent_variable = "Total workers, including draftees",
  independent_variable = "Number of male draftees",
  estimate = unname(coef(m_office_size)["n_drafted_male"]),
  std_error = unname(se(m_office_size)["n_drafted_male"]),
  p_value = unname(pvalue(m_office_size)["n_drafted_male"]),
  first_stage_f = (estimate / std_error)^2,
  observations = nobs(m_office_size),
  offices = n_distinct(first_stage_data$office_id)
)
write_csv(office_size_results, file.path(OUT_DIR, "office_size_first_stage.csv"))
write_csv(
  first_stage_data %>% select(
    office_id, kyoku, ka, kakari, pos_norm, year_num,
    n_drafted_male, n_internal_total, n_new_hires, replacements,
    cumul_n_male
  ),
  file.path(OUT_DIR, "analysis_cells.csv")
)

cat("\nREPLACEMENT FIRST STAGE\n")
print(results)
cat("\nOFFICE-SIZE FIRST STAGE (DRAFTEES NOT SUBTRACTED)\n")
print(office_size_results)
