suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(ggplot2)
  library(fixest)
})

OUT_TEX <- file.path("Results", "NaturalDepartureReplacementMain.tex")
OUT_CSV <- file.path("Results", "natural_departure_replacement_margins.csv")

source("Results/load_replacement_margins.R")

baseline_years <- 1930:1943

drafted_staff <- df_all %>%
  filter(drafted %in% TRUE) %>%
  distinct(staff_id)

baseline_cumul_male <- map_dfr(baseline_years, function(yr) {
  df %>%
    filter(year_num < yr, !is_female) %>%
    group_by(office_id, kakari, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

baseline_lag <- df %>%
  distinct(staff_id, year_num, .keep_all = TRUE) %>%
  select(
    staff_id, year_num,
    lag_kyoku_group = kyoku_group,
    lag_ka = ka,
    lag_ka_group = ka_group
  ) %>%
  mutate(year_num = year_num + 1)

baseline_transitions <- df %>%
  filter(year_num %in% baseline_years) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year, by = "staff_id") %>%
  left_join(baseline_lag, by = c("staff_id", "year_num")) %>%
  mutate(
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ year_num == first_year
    ),
    arrival_type = case_when(
      is.na(lag_kyoku_group) | is.na(kyoku_group) ~ NA_character_,
      !is.na(ka_group) & !is.na(lag_ka_group) & ka_group == lag_ka_group ~ "retention",
      (is.na(ka_group) | is.na(lag_ka_group)) &
        lag_kyoku_group == kyoku_group &
        !is.na(lag_ka) & !is.na(ka) & lag_ka == ka ~ "retention",
      lag_kyoku_group == kyoku_group ~ "transfer_same_dept",
      lag_kyoku_group != kyoku_group ~ "transfer_diff_dept",
      TRUE ~ NA_character_
    )
  )

baseline_outcomes <- baseline_transitions %>%
  anti_join(drafted_staff, by = "staff_id") %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_retention = sum(arrival_type == "retention", na.rm = TRUE),
    n_transfer_same_dept = sum(arrival_type == "transfer_same_dept", na.rm = TRUE),
    n_transfer_diff_dept = sum(arrival_type == "transfer_diff_dept", na.rm = TRUE),
    n_new_hires = sum(is_new_hire, na.rm = TRUE),
    n_workers = n(),
    .groups = "drop"
  ) %>%
  mutate(n_internal_total = n_retention + n_transfer_same_dept + n_transfer_diff_dept)

presence_t <- df %>%
  filter(year_num %in% baseline_years) %>%
  anti_join(drafted_staff, by = "staff_id") %>%
  distinct(
    staff_id,
    year_num,
    t_office_id = office_id,
    t_kyoku = kyoku,
    t_ka = ka,
    t_kakari = kakari,
    t_pos_norm = pos_norm
  )

departures <- df %>%
  filter(year_num %in% (baseline_years - 1), !is_female) %>%
  anti_join(drafted_staff, by = "staff_id") %>%
  transmute(
    staff_id,
    year_num = year_num + 1,
    office_id,
    kyoku,
    ka,
    kakari,
    pos_norm
  ) %>%
  left_join(presence_t, by = c("staff_id", "year_num")) %>%
  mutate(
    observed_in_t = !is.na(t_office_id),
    same_cell = observed_in_t &
      office_id == t_office_id &
      coalesce(kyoku, "") == coalesce(t_kyoku, "") &
      coalesce(ka, "") == coalesce(t_ka, "") &
      coalesce(kakari, "") == coalesce(t_kakari, "") &
      coalesce(pos_norm, "") == coalesce(t_pos_norm, ""),
    dep_transfer = observed_in_t & !same_cell,
    dep_exit = !observed_in_t
  ) %>%
  filter(year_num %in% baseline_years) %>%
  group_by(office_id, kyoku, ka, kakari, pos_norm, year_num) %>%
  summarise(
    n_depart_transfer = sum(dep_transfer, na.rm = TRUE),
    n_depart_exit = sum(dep_exit, na.rm = TRUE),
    .groups = "drop"
  )

panel <- baseline_outcomes %>%
  left_join(baseline_cumul_male, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  left_join(departures, by = c("office_id", "kyoku", "ka", "kakari", "pos_norm", "year_num")) %>%
  mutate(
    cumul_n_male = replace_na(cumul_n_male, 0),
    n_depart_transfer = replace_na(n_depart_transfer, 0),
    n_depart_exit = replace_na(n_depart_exit, 0),
    log_cumul_male = log(cumul_n_male + 1),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_),
    period = case_when(
      year_num <= 1936 ~ "Peace, 1930--1936",
      year_num >= 1937 & year_num <= 1943 ~ "War, 1937--1943"
    )
  ) %>%
  filter(!is.na(ka_id), !is.na(period))

outcomes <- c(
  n_internal_total = "Total internal",
  n_retention = "Retention",
  n_transfer_same_dept = "Same dept.",
  n_transfer_diff_dept = "Diff. dept.",
  n_new_hires = "New hires"
)

treatments <- c(
  n_depart_transfer = "Internal-transfer departures",
  n_depart_exit = "Exit departures"
)

estimate_one <- function(d, y, x, period_label, outcome_label, treatment_label) {
  fml <- as.formula(paste0(y, " ~ ", x, " + log_cumul_male | year_num + ka_id + pos_norm"))
  m <- feols(fml, data = d, cluster = ~office_id, warn = FALSE)
  tibble(
    period = period_label,
    outcome = outcome_label,
    variable = y,
    treatment = treatment_label,
    treatment_var = x,
    estimate = unname(coef(m)[x]),
    se = unname(se(m)[x]),
    p = unname(pvalue(m)[x]),
    n = nobs(m),
    offices = n_distinct(d$office_id),
    total_departures = sum(d[[x]], na.rm = TRUE)
  )
}

results <- imap_dfr(treatments, function(treatment_label, x) {
  imap_dfr(outcomes, function(outcome_label, y) {
    map_dfr(c("Peace, 1930--1936", "War, 1937--1943"), function(period_label) {
      estimate_one(filter(panel, period == period_label), y, x, period_label, outcome_label, treatment_label)
    })
  })
})

write_csv(results, OUT_CSV)

fmt_coef <- function(b, p) {
  stars <- case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "$^{***}$",
    p < 0.05 ~ "$^{**}$",
    p < 0.1 ~ "$^{*}$",
    TRUE ~ ""
  )
  paste0(sprintf("%.3f", b), stars)
}

fmt_se <- function(s) paste0("(", sprintf("%.3f", s), ")")

make_row <- function(d, treatment_label) {
  d <- d %>%
    filter(treatment == treatment_label) %>%
    mutate(variable = factor(variable, levels = names(outcomes))) %>%
    arrange(variable)
  c(
    paste0("        ", treatment_label, " & ", paste(fmt_coef(d$estimate, d$p), collapse = " & "), " \\\\"),
    paste0("         & ", paste(fmt_se(d$se), collapse = " & "), " \\\\")
  )
}

make_frame <- function(period_label, title, date_label) {
  d <- results %>% filter(period == period_label)
  n_obs <- max(d$n, na.rm = TRUE)
  n_transfer <- d %>% filter(treatment_var == "n_depart_transfer") %>% summarise(x = max(total_departures, na.rm = TRUE)) %>% pull(x)
  n_exit <- d %>% filter(treatment_var == "n_depart_exit") %>% summarise(x = max(total_departures, na.rm = TRUE)) %>% pull(x)
  rows <- c(make_row(d, "Internal-transfer departures"), make_row(d, "Exit departures"))

  c(
    paste0("\\begin{frame}[t]{", title, "}"),
    "    \\tiny",
    "    \\centering",
    "    \\begin{threeparttable}",
    "    \\begin{tabular}{lcccc|c}",
    "        \\toprule",
    paste0("        \\multicolumn{6}{l}{\\textbf{Natural departures, ", date_label, "}} \\\\[2pt]"),
    "        & \\multicolumn{4}{c|}{\\textit{Internal staff response}} & \\textit{External} \\\\",
    "        \\cmidrule(lr){2-5}\\cmidrule(lr){6-6}",
    "         & Total & \\multicolumn{3}{c|}{--- origin of worker ---} & \\\\",
    "         & internal & Retention & Same dept. & Diff. dept. & New hires \\\\",
    "         &  & (same section) & (cross-section) & (cross-bureau) & \\\\",
    "        \\midrule",
    rows,
    "        \\midrule",
    paste0("        Observations & \\multicolumn{5}{c}{", scales::comma(n_obs), "} \\\\"),
    paste0("        Internal-transfer departures & \\multicolumn{5}{c}{", scales::comma(n_transfer), "} \\\\"),
    paste0("        Exit departures & \\multicolumn{5}{c}{", scales::comma(n_exit), "} \\\\"),
    "        Year FE & \\multicolumn{5}{c}{Yes} \\\\",
    "        Section FE & \\multicolumn{5}{c}{Yes} \\\\",
    "        Position FE & \\multicolumn{5}{c}{Yes} \\\\",
    "        Log cumulative male baseline & \\multicolumn{5}{c}{Yes} \\\\",
    "        \\bottomrule",
    "    \\end{tabular}",
    "    \\begin{tablenotes}[flushleft]",
    "        \\tiny",
    paste0("        \\item \\textit{Notes:} OLS regressions. Unit of observation: destination office $\\times$ kyoku $\\times$ ka $\\times$ kakari $\\times$ occupation $\\times$ year. Each row is estimated separately and reports the coefficient on the listed number of male natural departures from the same full cell between $t-1$ and $t$. Internal-transfer departures are workers observed in a different cell in $t$; exit departures are workers absent from the personnel directory in $t$. Draft-coded workers are excluded. Outcomes use the same replacement-margin definitions as the draft table. Standard errors clustered by office in parentheses. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
    "    \\end{tablenotes}",
    "    \\end{threeparttable}",
    "\\end{frame}",
    ""
  )
}

tex <- c(
  make_frame("Peace, 1930--1936", "Natural departures and replacement margins: peace time", "peace time, 1930--1936"),
  make_frame("War, 1937--1943", "Natural departures and replacement margins: war time", "war time, 1937--1943")
)

writeLines(tex, OUT_TEX, useBytes = TRUE)
print(results)
cat("Wrote ", OUT_TEX, "\n", sep = "")
