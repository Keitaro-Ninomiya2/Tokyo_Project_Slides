suppressMessages({
  library(tidyverse)
  library(fixest)
})

FIG_REPO <- file.path(Sys.getenv("USERPROFILE"), "Documents", "GitHub", "Tokyo_Project_Figures")
SOURCE_SCRIPT <- file.path(FIG_REPO, "Regressions", "run_replacement_margins.R")
ENTRANT_CLASSIFICATION <- file.path(
  FIG_REPO,
  "MainResults",
  "PostMergerEntrantOrganizationalAdjustment1944",
  "tables",
  "step1_tokyoto1944_worker_entrant_classification.csv"
)
OUT_DIR <- file.path("Results", "draft1944_pre_draft_trajectories")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

source(SOURCE_SCRIPT)

pre_years <- 1930:1943
table_years <- c(1930, 1933, 1936, 1939, 1941, 1943)
cell_table_years <- 1930:1944
event_table_years <- c(1930, 1933, 1936, 1939, 1941, 1943, 1944)

period_rects <- tibble(
  period = c("Peace time", "War", "Mass drafting"),
  xmin = c(1929.5, 1936.5, 1943.5),
  xmax = c(1936.5, 1943.5, 1944.5),
  fill = c("#dcebf5", "#f5e6c8", "#f1cfcf")
  ) %>%
  mutate(period = factor(period, levels = c("Peace time", "War", "Mass drafting")))

add_period_background <- function(p) {
  p +
    geom_rect(
      data = period_rects,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period),
      inherit.aes = FALSE,
      alpha = 0.55
    ) +
    scale_fill_manual(values = setNames(period_rects$fill, period_rects$period), name = NULL)
}

future_draftees_1944 <- df %>%
  filter(year_num == 1944, drafted == TRUE, !is_female) %>%
  distinct(staff_id)

post_merger_entrant_draftees_1944 <- read_csv(
  ENTRANT_CLASSIFICATION,
  show_col_types = FALSE,
  guess_max = 100000
) %>%
  mutate(
    staff_id = as.numeric(staff_id),
    office_id = as.numeric(office_id),
    year_num = as.integer(year_num),
    drafted_pref = drafted_pref %in% TRUE,
    post_merger_entrant_preferred = post_merger_entrant_preferred %in% TRUE,
    is_female = is_female %in% TRUE
  ) %>%
  filter(
    year_num == 1944,
    drafted_pref,
    !is_female,
    post_merger_entrant_preferred
  ) %>%
  distinct(staff_id, office_id, kyoku, ka, kakari, pos_norm)

draft_cells_1943 <- df %>%
  filter(year_num == 1943) %>%
  semi_join(future_draftees_1944, by = "staff_id") %>%
  group_by(office_id, kyoku, ka, kakari, pos_norm) %>%
  summarise(n_future_drafted_1944 = n_distinct(staff_id), .groups = "drop")

cumul_male_stock_pre <- map_dfr(pre_years, function(yr) {
  df %>%
    filter(year_num < yr, !is_female) %>%
    group_by(office_id, kakari, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

staff_lag_pre <- df %>%
  distinct(staff_id, year_num, .keep_all = TRUE) %>%
  select(staff_id, year_num,
         lag_kyoku_group = kyoku_group,
         lag_ka          = ka,
         lag_ka_group    = ka_group) %>%
  mutate(year_num = year_num + 1)

staff_transitions_pre <- df %>%
  filter(year_num %in% pre_years) %>%
  left_join(office_initial_year, by = "office_id") %>%
  left_join(staff_first_year,    by = "staff_id") %>%
  left_join(staff_lag_pre,       by = c("staff_id", "year_num")) %>%
  mutate(
    is_new_hire = case_when(
      year_num == office_first_year ~ NA,
      TRUE ~ (year_num == first_year)
    ),
    arrival_type = case_when(
      is.na(lag_kyoku_group) | is.na(kyoku_group)         ~ NA_character_,
      !is.na(ka_group) & !is.na(lag_ka_group) &
        ka_group == lag_ka_group                           ~ "retention",
      (is.na(ka_group) | is.na(lag_ka_group)) &
        lag_kyoku_group == kyoku_group &
        !is.na(lag_ka) & !is.na(ka) & lag_ka == ka        ~ "retention",
      lag_kyoku_group == kyoku_group                       ~ "transfer_same_dept",
      lag_kyoku_group != kyoku_group                       ~ "transfer_diff_dept",
      TRUE ~ NA_character_
    )
  )

pre_outcomes_excl_future_draftees <- staff_transitions_pre %>%
  anti_join(future_draftees_1944, by = "staff_id") %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_retention          = sum(arrival_type == "retention",          na.rm = TRUE),
    n_transfer_same_dept = sum(arrival_type == "transfer_same_dept", na.rm = TRUE),
    n_transfer_diff_dept = sum(arrival_type == "transfer_diff_dept", na.rm = TRUE),
    n_new_hires          = sum(is_new_hire,                          na.rm = TRUE),
    n_workers            = n(),
    .groups = "drop"
  ) %>%
  mutate(n_internal_total = n_retention + n_transfer_same_dept + n_transfer_diff_dept)

pre_panel <- pre_outcomes_excl_future_draftees %>%
  left_join(cumul_male_stock_pre, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  left_join(
    draft_cells_1943,
    by = c("office_id", "kyoku", "ka", "kakari", "pos_norm")
  ) %>%
  mutate(
    cumul_n_male = replace_na(cumul_n_male, 0),
    n_future_drafted_1944 = replace_na(n_future_drafted_1944, 0),
    any_future_drafted_1944 = as.integer(n_future_drafted_1944 > 0),
    log_cumul_male = log(cumul_n_male + 1),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  ) %>%
  filter(!is.na(ka_id))

matched_draft_cells_1943 <- draft_cells_1943 %>%
  group_by(ka, kakari, pos_norm) %>%
  summarise(n_future_drafted_1944 = sum(n_future_drafted_1944), .groups = "drop")

matched_cumul_male_stock_pre <- map_dfr(pre_years, function(yr) {
  df %>%
    filter(year_num < yr, !is_female) %>%
    group_by(ka, kakari, pos_norm) %>%
    summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

matched_pre_panel <- pre_outcomes_excl_future_draftees %>%
  group_by(ka, kakari, pos_norm, year_num) %>%
  summarise(
    n_retention          = sum(n_retention,          na.rm = TRUE),
    n_transfer_same_dept = sum(n_transfer_same_dept, na.rm = TRUE),
    n_transfer_diff_dept = sum(n_transfer_diff_dept, na.rm = TRUE),
    n_new_hires          = sum(n_new_hires,          na.rm = TRUE),
    n_workers            = sum(n_workers,            na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(n_internal_total = n_retention + n_transfer_same_dept + n_transfer_diff_dept) %>%
  left_join(matched_cumul_male_stock_pre, by = c("ka", "kakari", "pos_norm", "year_num")) %>%
  left_join(matched_draft_cells_1943, by = c("ka", "kakari", "pos_norm")) %>%
  mutate(
    cumul_n_male = replace_na(cumul_n_male, 0),
    n_future_drafted_1944 = replace_na(n_future_drafted_1944, 0),
    any_future_drafted_1944 = as.integer(n_future_drafted_1944 > 0),
    log_cumul_male = log(cumul_n_male + 1),
    ka_id = if_else(!is.na(ka) & ka != "", ka, NA_character_)
  ) %>%
  filter(!is.na(ka_id))

outcomes <- c(
  n_workers = "Workers",
  n_internal_total = "Total internal",
  n_retention = "Retention",
  n_transfer_same_dept = "Same dept.",
  n_transfer_diff_dept = "Diff. dept.",
  n_new_hires = "New hires"
)

raw_trajectories <- pre_panel %>%
  group_by(year_num, any_future_drafted_1944) %>%
  summarise(
    cells = n(),
    across(all_of(names(outcomes)), \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(group = if_else(any_future_drafted_1944 == 1,
                         "Cells with 1944 drafted workers",
                         "Cells without 1944 drafted workers"))

future_drafted_offices <- draft_cells_1943 %>%
  distinct(office_id) %>%
  mutate(any_future_drafted_office = 1L)

office_cumul_male_stock_pre <- map_dfr(pre_years, function(yr) {
  df %>%
    filter(year_num < yr, !is_female) %>%
    group_by(office_id) %>%
    summarise(cumul_n_male_office = n_distinct(staff_id), .groups = "drop") %>%
    mutate(year_num = yr)
})

office_panel <- pre_outcomes_excl_future_draftees %>%
  group_by(office_id, year_num) %>%
  summarise(
    n_internal_total = sum(n_internal_total, na.rm = TRUE),
    n_retention = sum(n_retention, na.rm = TRUE),
    n_new_hires = sum(n_new_hires, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(office_cumul_male_stock_pre, by = c("office_id", "year_num")) %>%
  left_join(future_drafted_offices, by = "office_id") %>%
  mutate(
    cumul_n_male_office = replace_na(cumul_n_male_office, 0),
    any_future_drafted_office = replace_na(any_future_drafted_office, 0L),
    log_cumul_male_office = log(cumul_n_male_office + 1)
  )

office_raw_trajectories <- office_panel %>%
  group_by(year_num, any_future_drafted_office) %>%
  summarise(
    offices = n(),
    n_internal_total = mean(n_internal_total, na.rm = TRUE),
    n_retention = mean(n_retention, na.rm = TRUE),
    n_new_hires = mean(n_new_hires, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group = if_else(any_future_drafted_office == 1,
                         "Offices with 1944 drafted workers",
                         "Offices without 1944 drafted workers"))

office_adjusted_diffs <- imap_dfr(c(n_internal_total = "Total internal",
                                    n_retention = "Retention",
                                    n_new_hires = "New hires"), function(label, y) {
  map_dfr(pre_years, function(yr) {
    d <- office_panel %>% filter(year_num == yr)
    m <- feols(as.formula(paste0(y, " ~ any_future_drafted_office + log_cumul_male_office")),
               data = d, cluster = ~office_id, warn = FALSE)
    tibble(
      outcome = label,
      variable = y,
      year_num = yr,
      estimate = unname(coef(m)["any_future_drafted_office"]),
      se = unname(se(m)["any_future_drafted_office"]),
      p = unname(pvalue(m)["any_future_drafted_office"]),
      n = nobs(m)
    )
  })
})

adjusted_diffs <- imap_dfr(outcomes, function(label, y) {
  map_dfr(pre_years, function(yr) {
    d <- matched_pre_panel %>% filter(year_num == yr)
    fml <- as.formula(paste0(y, " ~ any_future_drafted_1944 + log_cumul_male | ka_id + pos_norm"))
    m <- feols(fml, data = d, cluster = ~ka_id, warn = FALSE)
    tibble(
      outcome = label,
      variable = y,
      year_num = yr,
      estimate = unname(coef(m)["any_future_drafted_1944"]),
      se = unname(se(m)["any_future_drafted_1944"]),
      p = unname(pvalue(m)["any_future_drafted_1944"]),
      n = nobs(m)
    )
  })
})

draft_cells_1944 <- df_all %>%
  filter(year_num == 1944, drafted == TRUE, !is_female) %>%
  group_by(ka, kakari, pos_norm) %>%
  summarise(n_drafted_male_1944 = n_distinct(staff_id), .groups = "drop")

outcomes_1944_excl_draftees <- staff_transitions %>%
  filter(year_num == 1944) %>%
  anti_join(future_draftees_1944, by = "staff_id") %>%
  group_by(kyoku, ka, office_id, kakari, pos_norm, year_num) %>%
  summarise(
    n_retention          = sum(arrival_type == "retention",          na.rm = TRUE),
    n_transfer_same_dept = sum(arrival_type == "transfer_same_dept", na.rm = TRUE),
    n_transfer_diff_dept = sum(arrival_type == "transfer_diff_dept", na.rm = TRUE),
    n_new_hires          = sum(is_new_hire,                          na.rm = TRUE),
    n_workers            = n(),
    .groups = "drop"
  ) %>%
  mutate(n_internal_total = n_retention + n_transfer_same_dept + n_transfer_diff_dept)

panel_1944 <- outcomes_1944_excl_draftees %>%
  group_by(ka, kakari, pos_norm, year_num) %>%
  summarise(
    n_retention          = sum(n_retention,          na.rm = TRUE),
    n_transfer_same_dept = sum(n_transfer_same_dept, na.rm = TRUE),
    n_transfer_diff_dept = sum(n_transfer_diff_dept, na.rm = TRUE),
    n_new_hires          = sum(n_new_hires,          na.rm = TRUE),
    n_workers            = sum(n_workers,            na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(n_internal_total = n_retention + n_transfer_same_dept + n_transfer_diff_dept) %>%
  left_join(matched_cumul_male_stock_pre, by = c("ka", "kakari", "pos_norm", "year_num")) %>%
  left_join(draft_cells_1944, by = c("ka", "kakari", "pos_norm")) %>%
  mutate(
    cumul_n_male = replace_na(cumul_n_male, 0),
    n_drafted_male_1944 = replace_na(n_drafted_male_1944, 0),
    any_future_drafted_1944 = as.integer(n_drafted_male_1944 > 0),
    log_cumul_male = log(cumul_n_male + 1),
    ka_id = if_else(!is.na(ka) & ka != "", ka, NA_character_)
  ) %>%
  filter(!is.na(ka_id))

adjusted_diffs_1944 <- imap_dfr(outcomes, function(label, y) {
  fml <- as.formula(paste0(y, " ~ any_future_drafted_1944 + log_cumul_male | ka_id + pos_norm"))
  m <- feols(fml, data = panel_1944, cluster = ~ka_id, warn = FALSE)
  tibble(
    outcome = label,
    variable = y,
    year_num = 1944L,
    estimate = unname(coef(m)["any_future_drafted_1944"]),
    se = unname(se(m)["any_future_drafted_1944"]),
    p = unname(pvalue(m)["any_future_drafted_1944"]),
    n = nobs(m)
  )
})

adjusted_diffs <- bind_rows(adjusted_diffs, adjusted_diffs_1944)

draft_intensity_treatment_diffs_pre <- imap_dfr(outcomes, function(label, y) {
  map_dfr(pre_years, function(yr) {
    d <- matched_pre_panel %>%
      filter(year_num == yr, any_future_drafted_1944 == 1)
    fml <- as.formula(paste0(y, " ~ n_future_drafted_1944 + log_cumul_male | ka_id + pos_norm"))
    m <- tryCatch(
      feols(fml, data = d, cluster = ~ka_id, warn = FALSE),
      error = function(e) NULL
    )
    if (is.null(m) || !("n_future_drafted_1944" %in% names(coef(m)))) {
      return(tibble(
        outcome = label,
        variable = y,
        year_num = yr,
        estimate = NA_real_,
        se = NA_real_,
        p = NA_real_,
        n = ifelse(is.null(m), nrow(d), nobs(m))
      ))
    }
    tibble(
      outcome = label,
      variable = y,
      year_num = yr,
      estimate = unname(coef(m)["n_future_drafted_1944"]),
      se = unname(se(m)["n_future_drafted_1944"]),
      p = unname(pvalue(m)["n_future_drafted_1944"]),
      n = nobs(m)
    )
  })
})

draft_intensity_treatment_diffs_1944 <- imap_dfr(outcomes, function(label, y) {
  d <- panel_1944 %>%
    filter(any_future_drafted_1944 == 1)
  fml <- as.formula(paste0(y, " ~ n_drafted_male_1944 + log_cumul_male | ka_id + pos_norm"))
  m <- tryCatch(
    feols(fml, data = d, cluster = ~ka_id, warn = FALSE),
    error = function(e) NULL
  )
  if (is.null(m) || !("n_drafted_male_1944" %in% names(coef(m)))) {
    return(tibble(
      outcome = label,
      variable = y,
      year_num = 1944L,
      estimate = NA_real_,
      se = NA_real_,
      p = NA_real_,
      n = ifelse(is.null(m), nrow(d), nobs(m))
    ))
  }
  tibble(
    outcome = label,
    variable = y,
    year_num = 1944L,
    estimate = unname(coef(m)["n_drafted_male_1944"]),
    se = unname(se(m)["n_drafted_male_1944"]),
    p = unname(pvalue(m)["n_drafted_male_1944"]),
    n = nobs(m)
  )
})

draft_intensity_treatment_diffs <- bind_rows(
  draft_intensity_treatment_diffs_pre,
  draft_intensity_treatment_diffs_1944
)

did_panel <- bind_rows(
  matched_pre_panel %>%
    transmute(
      ka, kakari, pos_norm, ka_id, year_num,
      n_workers, n_internal_total, n_retention, n_transfer_same_dept,
      n_transfer_diff_dept, n_new_hires, log_cumul_male,
      any_future_drafted_1944,
      draft_treatment_count = n_future_drafted_1944
    ),
  panel_1944 %>%
    transmute(
      ka, kakari, pos_norm, ka_id, year_num,
      n_workers, n_internal_total, n_retention, n_transfer_same_dept,
      n_transfer_diff_dept, n_new_hires, log_cumul_male,
      any_future_drafted_1944,
      draft_treatment_count = n_drafted_male_1944
    )
) %>%
  mutate(
    cell_id = paste(ka_id, kakari, pos_norm, sep = "||"),
    period = case_when(
      year_num <= 1936 ~ "peace",
      year_num <= 1943 ~ "war",
      year_num == 1944 ~ "mass",
      TRUE ~ NA_character_
    ),
    war_period = as.integer(period == "war"),
    mass_drafting_year = as.integer(period == "mass")
  ) %>%
  filter(!is.na(period))

did_panel_treated <- did_panel %>%
  filter(any_future_drafted_1944 == 1)

intensity_outcomes <- c(
  n_workers = "Workers",
  n_internal_total = "Total internal",
  n_retention = "Retention",
  n_transfer_same_dept = "Same dept.",
  n_transfer_diff_dept = "Diff. dept.",
  n_new_hires = "New hires"
)

did_term_labels <- c(
  "any_future_drafted_1944:war_period" = "Future drafted cell $\\times$ War",
  "any_future_drafted_1944:mass_drafting_year" = "Future drafted cell $\\times$ Mass drafting"
)

estimate_period_did <- function(outcome_var, outcome_label) {
  models <- list(
    "Cell + year FE" = feols(
      as.formula(paste0(outcome_var, " ~ any_future_drafted_1944:war_period + ",
                        "any_future_drafted_1944:mass_drafting_year + log_cumul_male | ",
                        "cell_id + year_num")),
      data = did_panel,
      cluster = ~ka_id,
      warn = FALSE
    ),
    "Section/position + year FE" = feols(
      as.formula(paste0(outcome_var, " ~ any_future_drafted_1944:war_period + ",
                        "any_future_drafted_1944:mass_drafting_year + log_cumul_male | ",
                        "ka_id + pos_norm + year_num")),
      data = did_panel,
      cluster = ~ka_id,
      warn = FALSE
    )
  )

  imap_dfr(models, function(model, spec) {
    broom::tidy(model) %>%
      filter(term %in% names(did_term_labels)) %>%
      transmute(
        outcome = outcome_label,
        variable = outcome_var,
        specification = spec,
        term,
        label = did_term_labels[term],
        estimate,
        se = std.error,
        p = p.value,
        n = nobs(model)
      )
  })
}

period_did_results <- imap_dfr(intensity_outcomes, function(label, variable) {
  estimate_period_did(variable, label)
})

cell_size_did_models <- list(
  "Cell + year FE" = feols(
    n_workers ~ any_future_drafted_1944:war_period +
      any_future_drafted_1944:mass_drafting_year + log_cumul_male |
      cell_id + year_num,
    data = did_panel,
    cluster = ~ka_id,
    warn = FALSE
  ),
  "Section/position + year FE" = feols(
    n_workers ~ any_future_drafted_1944:war_period +
      any_future_drafted_1944:mass_drafting_year + log_cumul_male |
      ka_id + pos_norm + year_num,
    data = did_panel,
    cluster = ~ka_id,
    warn = FALSE
  )
)

cell_size_did_results <- period_did_results %>%
  filter(variable == "n_workers")

estimate_draft_intensity_treatment_did <- function(outcome_var, outcome_label) {
  specs <- list(
    "Section + occupation + year FE" = "ka_id + pos_norm + year_num",
    "No section FE" = "pos_norm + year_num",
    "No occupation FE" = "ka_id + year_num"
  )

  term_labels <- c(
    "draft_treatment_count:war_period" = "No. future drafted $\\times$ War",
    "draft_treatment_count:mass_drafting_year" = "No. future drafted $\\times$ Mass drafting"
  )

  imap_dfr(specs, function(fe_spec, spec) {
    m <- feols(
      as.formula(paste0(outcome_var, " ~ draft_treatment_count:war_period + ",
                        "draft_treatment_count:mass_drafting_year + log_cumul_male | ",
                        fe_spec)),
      data = did_panel_treated,
      cluster = ~ka_id,
      warn = FALSE
    )
    broom::tidy(m) %>%
      filter(term %in% names(term_labels)) %>%
      transmute(
        outcome = outcome_label,
        variable = outcome_var,
        specification = spec,
        term,
        label = term_labels[term],
        estimate,
        se = std.error,
        p = p.value,
        n = nobs(m)
      )
  })
}

draft_intensity_treatment_did_results <- imap_dfr(intensity_outcomes, function(label, variable) {
  estimate_draft_intensity_treatment_did(variable, label)
})

estimate_intensity_model <- function(data, margin, year_value, label) {
  fml <- as.formula(paste0("draft_intensity ~ ", margin, " + log_cumul_male | ka_id + pos_norm"))
  m <- tryCatch(
    feols(fml, data = data, cluster = ~ka_id, warn = FALSE),
    error = function(e) NULL
  )

  if (is.null(m) || !(margin %in% names(coef(m)))) {
    return(tibble(
      outcome = label,
      variable = margin,
      year_num = year_value,
      estimate = NA_real_,
      se = NA_real_,
      p = NA_real_,
      n = ifelse(is.null(m), nrow(data), nobs(m))
    ))
  }

  tibble(
    outcome = label,
    variable = margin,
    year_num = year_value,
    estimate = unname(coef(m)[margin]),
    se = unname(se(m)[margin]),
    p = unname(pvalue(m)[margin]),
    n = nobs(m)
  )
}

intensity_diffs_pre <- imap_dfr(intensity_outcomes, function(label, margin) {
  map_dfr(pre_years, function(yr) {
    d <- matched_pre_panel %>%
      filter(year_num == yr, any_future_drafted_1944 == 1) %>%
      mutate(draft_intensity = n_future_drafted_1944)
    estimate_intensity_model(d, margin, yr, label)
  })
})

intensity_diffs_1944 <- imap_dfr(intensity_outcomes, function(label, margin) {
  d <- panel_1944 %>%
    filter(any_future_drafted_1944 == 1) %>%
    mutate(draft_intensity = n_drafted_male_1944)
  estimate_intensity_model(d, margin, 1944L, label)
})

intensity_diffs <- bind_rows(intensity_diffs_pre, intensity_diffs_1944)

intensity_period_panel <- bind_rows(
  matched_pre_panel %>%
    filter(any_future_drafted_1944 == 1) %>%
    transmute(
      ka, kakari, pos_norm, ka_id, year_num,
      n_workers, n_internal_total, n_retention, n_transfer_same_dept,
      n_transfer_diff_dept, n_new_hires, log_cumul_male,
      draft_intensity = n_future_drafted_1944
    ),
  panel_1944 %>%
    filter(any_future_drafted_1944 == 1) %>%
    transmute(
      ka, kakari, pos_norm, ka_id, year_num,
      n_workers, n_internal_total, n_retention, n_transfer_same_dept,
      n_transfer_diff_dept, n_new_hires, log_cumul_male,
      draft_intensity = n_drafted_male_1944
    )
) %>%
  mutate(
    cell_id = paste(ka_id, kakari, pos_norm, sep = "||"),
    period = case_when(
      year_num <= 1936 ~ "peace",
      year_num <= 1943 ~ "war",
      year_num == 1944 ~ "mass",
      TRUE ~ NA_character_
    ),
    war_period = as.integer(period == "war"),
    mass_drafting_year = as.integer(period == "mass")
  ) %>%
  filter(!is.na(period))

estimate_intensity_period_did <- function(margin_var, margin_label) {
  term_labels <- setNames(
    c(paste0(margin_label, " $\\times$ War"),
      paste0(margin_label, " $\\times$ Mass drafting")),
    c(paste0(margin_var, ":war_period"),
      paste0(margin_var, ":mass_drafting_year"))
  )

  models <- list(
    "Cell + year FE" = feols(
      as.formula(paste0("draft_intensity ~ ", margin_var, " + ",
                        margin_var, ":war_period + ",
                        margin_var, ":mass_drafting_year + log_cumul_male | ",
                        "cell_id + year_num")),
      data = intensity_period_panel,
      cluster = ~ka_id,
      warn = FALSE
    ),
    "Section/position + year FE" = feols(
      as.formula(paste0("draft_intensity ~ ", margin_var, " + ",
                        margin_var, ":war_period + ",
                        margin_var, ":mass_drafting_year + log_cumul_male | ",
                        "ka_id + pos_norm + year_num")),
      data = intensity_period_panel,
      cluster = ~ka_id,
      warn = FALSE
    )
  )

  imap_dfr(models, function(model, spec) {
    broom::tidy(model) %>%
      filter(term %in% names(term_labels)) %>%
      transmute(
        outcome = margin_label,
        variable = margin_var,
        specification = spec,
        term,
        label = term_labels[term],
        estimate,
        se = std.error,
        p = p.value,
        n = nobs(model)
      )
  })
}

intensity_period_did_results <- imap_dfr(intensity_outcomes, function(label, variable) {
  estimate_intensity_period_did(variable, label)
})

within_section_plot_specs <- tribble(
  ~variable, ~title, ~subtitle, ~file, ~plot_target, ~table_target,
  "n_workers", "Within-section pre-trend: cell size", "Outcome: workers in team $\\times$ occupation cell", "within_section_pretrend_workers.pdf", "app:pretrend-workers-plot", "app:pretrend-workers-table",
  "n_internal_total", "Within-section pre-trend: internal flows", "Outcome: total internal arrivals", "within_section_pretrend_internal.pdf", "app:pretrend-internal-plot", "app:pretrend-internal-table",
  "n_retention", "Within-section pre-trend: retention", "Outcome: same-section retention", "within_section_pretrend_retention.pdf", "app:pretrend-retention-plot", "app:pretrend-retention-table",
  "n_transfer_same_dept", "Within-section pre-trend: same-dept transfers", "Outcome: cross-section transfers within department", "within_section_pretrend_same_dept.pdf", "app:pretrend-same-dept-plot", "app:pretrend-same-dept-table",
  "n_transfer_diff_dept", "Within-section pre-trend: diff.-dept transfers", "Outcome: cross-department transfers", "within_section_pretrend_diff_dept.pdf", "app:pretrend-diff-dept-plot", "app:pretrend-diff-dept-table",
  "n_new_hires", "Within-section pre-trend: new hires", "Outcome: external new hires", "within_section_pretrend_new_hires.pdf", "app:pretrend-new-hires-plot", "app:pretrend-new-hires-table"
)

make_within_section_plot <- function(target_variable, title, file) {
  d <- adjusted_diffs %>%
    filter(.data$variable == target_variable, year_num %in% cell_table_years) %>%
    mutate(
      ci_low = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se
    )

  p <- ggplot(d, aes(x = year_num, y = estimate)) +
    geom_rect(
      data = period_rects,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period),
      inherit.aes = FALSE,
      alpha = 0.55
    ) +
    geom_hline(yintercept = 0, linewidth = 0.4, color = "gray45") +
    geom_vline(xintercept = 1943.5, linewidth = 0.35, linetype = "dashed", color = "gray60") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12, color = "#536878") +
    geom_line(color = "#1f5a85", linewidth = 0.8) +
    geom_point(color = "#1f5a85", size = 2) +
    scale_x_continuous(breaks = seq(1930, 1944, by = 2), limits = c(1929.5, 1944.5)) +
    scale_fill_manual(values = setNames(period_rects$fill, period_rects$period), name = NULL) +
    labs(x = NULL, y = "Adjusted difference") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0)
    )

  ggsave(file.path(OUT_DIR, file), p, width = 6.8, height = 3.8, units = "in")
}

pwalk(
  within_section_plot_specs %>% transmute(target_variable = variable, title, file),
  make_within_section_plot
)

draft_intensity_treatment_plot_specs <- tribble(
  ~variable, ~title, ~subtitle, ~file,
  "n_internal_total", "Draft intensity: internal transfers event study", "Outcome: total internal arrivals; treatment: number of future draftees", "draft_intensity_treatment_internal.pdf",
  "n_new_hires", "Draft intensity: external hires event study", "Outcome: external new hires; treatment: number of future draftees", "draft_intensity_treatment_new_hires.pdf"
)

make_draft_intensity_treatment_plot <- function(target_variable, title, file) {
  d <- draft_intensity_treatment_diffs %>%
    filter(.data$variable == target_variable, year_num %in% cell_table_years) %>%
    mutate(
      ci_low = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se
    )

  p <- ggplot(d, aes(x = year_num, y = estimate)) +
    geom_rect(
      data = period_rects,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period),
      inherit.aes = FALSE,
      alpha = 0.55
    ) +
    geom_hline(yintercept = 0, linewidth = 0.4, color = "gray45") +
    geom_vline(xintercept = 1943.5, linewidth = 0.35, linetype = "dashed", color = "gray60") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12, color = "#536878") +
    geom_line(color = "#6d3f8f", linewidth = 0.8) +
    geom_point(color = "#6d3f8f", size = 2) +
    scale_x_continuous(breaks = seq(1930, 1944, by = 2), limits = c(1929.5, 1944.5)) +
    scale_fill_manual(values = setNames(period_rects$fill, period_rects$period), name = NULL) +
    labs(x = NULL, y = "Effect per future draftee") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0)
    )

  ggsave(file.path(OUT_DIR, file), p, width = 6.8, height = 3.8, units = "in")
}

pwalk(
  draft_intensity_treatment_plot_specs %>% transmute(target_variable = variable, title, file),
  make_draft_intensity_treatment_plot
)

intensity_plot_specs <- tribble(
  ~variable, ~title, ~subtitle, ~file, ~plot_target, ~table_target,
  "n_workers", "Draft intensity orthogonality: cell size", "Predictor: workers in team $\\times$ occupation cell", "draft_intensity_orthogonality_workers.pdf", "app:orthogonality-workers-plot", "app:orthogonality-workers-table",
  "n_internal_total", "Draft intensity orthogonality: internal flows", "Predictor: total internal arrivals", "draft_intensity_orthogonality_internal.pdf", "app:orthogonality-internal-plot", "app:orthogonality-internal-table",
  "n_retention", "Draft intensity orthogonality: retention", "Predictor: same-section retention", "draft_intensity_orthogonality_retention.pdf", "app:orthogonality-retention-plot", "app:orthogonality-retention-table",
  "n_transfer_same_dept", "Draft intensity orthogonality: same-dept transfers", "Predictor: cross-section transfers within department", "draft_intensity_orthogonality_same_dept.pdf", "app:orthogonality-same-dept-plot", "app:orthogonality-same-dept-table",
  "n_transfer_diff_dept", "Draft intensity orthogonality: diff.-dept transfers", "Predictor: cross-department transfers", "draft_intensity_orthogonality_diff_dept.pdf", "app:orthogonality-diff-dept-plot", "app:orthogonality-diff-dept-table",
  "n_new_hires", "Draft intensity orthogonality: new hires", "Predictor: external new hires", "draft_intensity_orthogonality_new_hires.pdf", "app:orthogonality-new-hires-plot", "app:orthogonality-new-hires-table"
)

make_intensity_plot <- function(target_variable, title, file) {
  d <- intensity_diffs %>%
    filter(.data$variable == target_variable, year_num %in% cell_table_years) %>%
    mutate(
      ci_low = estimate - 1.96 * se,
      ci_high = estimate + 1.96 * se
    )

  p <- ggplot(d, aes(x = year_num, y = estimate)) +
    geom_rect(
      data = period_rects,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period),
      inherit.aes = FALSE,
      alpha = 0.55
    ) +
    geom_hline(yintercept = 0, linewidth = 0.4, color = "gray45") +
    geom_vline(xintercept = 1943.5, linewidth = 0.35, linetype = "dashed", color = "gray60") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12, color = "#536878") +
    geom_line(color = "#6d3f8f", linewidth = 0.8) +
    geom_point(color = "#6d3f8f", size = 2) +
    scale_x_continuous(breaks = seq(1930, 1944, by = 2), limits = c(1929.5, 1944.5)) +
    scale_fill_manual(values = setNames(period_rects$fill, period_rects$period), name = NULL) +
    labs(x = NULL, y = "Coefficient predicting draft count") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0)
    )

  ggsave(file.path(OUT_DIR, file), p, width = 6.8, height = 3.8, units = "in")
}

pwalk(
  intensity_plot_specs %>% transmute(target_variable = variable, title, file),
  make_intensity_plot
)

plot_data <- raw_trajectories %>%
  select(year_num, group, n_internal_total, n_new_hires) %>%
  pivot_longer(c(n_internal_total, n_new_hires), names_to = "outcome", values_to = "mean") %>%
  mutate(outcome = recode(outcome,
                          n_internal_total = "Total internal",
                          n_new_hires = "New hires"))

trajectory_plot <- ggplot(plot_data, aes(x = year_num, y = mean, color = group, shape = group)) +
  geom_rect(
    data = period_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period),
    inherit.aes = FALSE,
    alpha = 0.55
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  facet_wrap(~outcome, scales = "free_y") +
  scale_x_continuous(breaks = seq(1930, 1944, by = 2), limits = c(1929.5, 1944.5)) +
  scale_color_manual(values = c("Cells with 1944 drafted workers" = "#1f5a85",
                                "Cells without 1944 drafted workers" = "#9c4a3b")) +
  scale_fill_manual(values = setNames(period_rects$fill, period_rects$period), name = NULL) +
  labs(x = NULL, y = "Mean count per cell", color = NULL, shape = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0)
  )

ggsave(file.path(OUT_DIR, "raw_trajectory_total_internal_new_hires.pdf"),
       trajectory_plot, width = 7.2, height = 3.4, units = "in")

office_plot_data <- office_raw_trajectories %>%
  select(year_num, group, n_internal_total, n_new_hires) %>%
  pivot_longer(c(n_internal_total, n_new_hires), names_to = "outcome", values_to = "mean") %>%
  mutate(outcome = recode(outcome,
                          n_internal_total = "Total internal",
                          n_new_hires = "New hires"))

office_trajectory_plot <- ggplot(office_plot_data, aes(x = year_num, y = mean, color = group, shape = group)) +
  geom_rect(
    data = period_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period),
    inherit.aes = FALSE,
    alpha = 0.55
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  facet_wrap(~outcome, scales = "free_y") +
  scale_x_continuous(breaks = seq(1930, 1944, by = 2), limits = c(1929.5, 1944.5)) +
  scale_color_manual(values = c("Offices with 1944 drafted workers" = "#1f5a85",
                                "Offices without 1944 drafted workers" = "#9c4a3b")) +
  scale_fill_manual(values = setNames(period_rects$fill, period_rects$period), name = NULL) +
  labs(x = NULL, y = "Mean count per office", color = NULL, shape = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0)
  )

ggsave(file.path(OUT_DIR, "office_raw_trajectory_total_internal_new_hires.pdf"),
       office_trajectory_plot, width = 7.2, height = 3.4, units = "in")

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "$^{***}$",
    p < 0.05 ~ "$^{**}$",
    p < 0.1 ~ "$^{*}$",
    TRUE ~ ""
  )
}

fmt_coef <- function(b, p) ifelse(is.na(b), "NA", paste0(sprintf("%.3f", b), stars(p)))
fmt_se <- function(s) ifelse(is.na(s), "NA", sprintf("(%.3f)", s))

table_rows <- office_adjusted_diffs %>%
  filter(outcome %in% c("Total internal", "Retention", "New hires"),
         year_num %in% table_years) %>%
  mutate(cell = fmt_coef(estimate, p), se_cell = fmt_se(se)) %>%
  select(outcome, year_num, cell, se_cell) %>%
  pivot_wider(names_from = year_num, values_from = c(cell, se_cell)) %>%
  mutate(outcome = factor(outcome, levels = c("Total internal", "Retention", "New hires"))) %>%
  arrange(outcome)

write_csv(raw_trajectories, file.path(OUT_DIR, "raw_trajectories.csv"))
write_csv(adjusted_diffs, file.path(OUT_DIR, "adjusted_yearly_differences.csv"))
write_csv(draft_intensity_treatment_diffs, file.path(OUT_DIR, "draft_intensity_treatment_yearly_differences.csv"))
write_csv(cell_size_did_results, file.path(OUT_DIR, "within_section_cell_size_did_results.csv"))
write_csv(period_did_results, file.path(OUT_DIR, "within_section_period_did_results.csv"))
write_csv(draft_intensity_treatment_did_results, file.path(OUT_DIR, "draft_intensity_treatment_did_results.csv"))
write_csv(intensity_diffs, file.path(OUT_DIR, "draft_intensity_orthogonality_differences.csv"))
write_csv(intensity_period_did_results, file.path(OUT_DIR, "draft_intensity_period_did_results.csv"))
write_csv(office_raw_trajectories, file.path(OUT_DIR, "office_raw_trajectories.csv"))
write_csv(office_adjusted_diffs, file.path(OUT_DIR, "office_adjusted_yearly_differences.csv"))

sample_summary <- pre_panel %>%
  summarise(
    rows = n(),
    future_drafted_cells = n_distinct(paste(office_id, kyoku, ka, kakari, pos_norm)[any_future_drafted_1944 == 1]),
    comparison_cells = n_distinct(paste(office_id, kyoku, ka, kakari, pos_norm)[any_future_drafted_1944 == 0]),
    future_drafted_workers = sum(n_future_drafted_1944[year_num == 1943]),
    offices = n_distinct(office_id)
  )
write_csv(sample_summary, file.path(OUT_DIR, "sample_summary.csv"))

matched_sample_summary <- matched_pre_panel %>%
  summarise(
    rows = n(),
    future_drafted_cells = n_distinct(paste(ka, kakari, pos_norm)[any_future_drafted_1944 == 1]),
    comparison_cells = n_distinct(paste(ka, kakari, pos_norm)[any_future_drafted_1944 == 0]),
    future_drafted_workers = sum(n_future_drafted_1944[year_num == 1943]),
    sections = n_distinct(ka_id)
  )
write_csv(matched_sample_summary, file.path(OUT_DIR, "matched_section_team_occ_sample_summary.csv"))

office_sample_summary <- office_panel %>%
  summarise(
    rows = n(),
    future_drafted_offices = n_distinct(office_id[any_future_drafted_office == 1]),
    comparison_offices = n_distinct(office_id[any_future_drafted_office == 0]),
    offices = n_distinct(office_id)
  )
write_csv(office_sample_summary, file.path(OUT_DIR, "office_sample_summary.csv"))

get_row_value <- function(row, nm) {
  value <- row[[nm]]
  if (is.null(value) || length(value) == 0 || is.na(value)) "NA" else as.character(value)
}

row_lines_for <- function(rows, years) {
  pmap_chr(rows, function(...) {
  row <- list(...)
  cells <- map_chr(years, \(yr) get_row_value(row, paste0("cell_", yr)))
  ses <- map_chr(years, \(yr) get_row_value(row, paste0("se_cell_", yr)))
  paste0(
    as.character(row$outcome), " & ", paste(cells, collapse = " & "), " \\\\\n",
    " & ", paste(ses, collapse = " & "), " \\\\"
  )
  })
}

row_lines <- row_lines_for(table_rows, table_years)

cell_table_rows <- adjusted_diffs %>%
  filter(outcome %in% c("Total internal", "Retention", "New hires"),
         year_num %in% cell_table_years) %>%
  mutate(cell = fmt_coef(estimate, p), se_cell = fmt_se(se)) %>%
  select(outcome, year_num, cell, se_cell) %>%
  pivot_wider(names_from = year_num, values_from = c(cell, se_cell)) %>%
  mutate(outcome = factor(outcome, levels = c("Total internal", "Retention", "New hires"))) %>%
  arrange(outcome)

cell_row_lines <- row_lines_for(cell_table_rows, cell_table_years)

period_table_frame <- function(data, target_variable, title, note, cells = NA_character_, sections = NA_character_, target = NA_character_) {
  specs <- c("Cell + year FE", "Section/position + year FE")
  rows <- data %>%
    filter(.data$variable == target_variable) %>%
    mutate(
      specification = factor(specification, levels = specs),
      cell = fmt_coef(estimate, p),
      se_cell = fmt_se(se)
    ) %>%
    select(label, specification, cell, se_cell) %>%
    pivot_wider(names_from = specification, values_from = c(cell, se_cell))

  row_lines <- pmap_chr(rows, function(...) {
    row <- list(...)
    coefs <- map_chr(specs, \(spec) get_row_value(row, paste0("cell_", spec)))
    ses <- map_chr(specs, \(spec) get_row_value(row, paste0("se_cell_", spec)))
    paste0(
      "        ", as.character(row$label), " & ", paste(coefs, collapse = " & "), " \\\\\n",
      "        & ", paste(ses, collapse = " & "), " \\\\"
    )
  })

  nobs_row <- data %>%
    filter(.data$variable == target_variable) %>%
    distinct(specification, n) %>%
    mutate(specification = factor(specification, levels = specs)) %>%
    arrange(specification)

  nobs_cells <- map_chr(specs, function(spec) {
    n_value <- nobs_row$n[as.character(nobs_row$specification) == spec]
    if (length(n_value) == 0 || is.na(n_value[1])) "NA" else scales::comma(n_value[1])
  })

  c(
    paste0("\\begin{frame}[t]{", title, "}"),
    if (!is.na(target)) paste0("    \\hypertarget{", target, "}{}") else character(0),
    "    \\scriptsize",
    "    \\centering",
    "    \\begin{threeparttable}",
    "    \\begin{tabular}{lcc}",
    "        \\toprule",
    "        & Cell + year FE & Section/position + year FE \\\\",
    "        \\midrule",
    row_lines,
    "        \\midrule",
    paste0("        Observations & ", paste(nobs_cells, collapse = " & "), " \\\\"),
    if (!is.na(cells)) paste0("        Cells & \\multicolumn{2}{c}{", cells, "} \\\\") else character(0),
    if (!is.na(sections)) paste0("        Sections & \\multicolumn{2}{c}{", sections, "} \\\\") else character(0),
    "        Baseline male employment (log) & Yes & Yes \\\\",
    "        \\bottomrule",
    "    \\end{tabular}",
    "    \\begin{tablenotes}[flushleft]",
    "        \\tiny",
    paste0("        \\item \\textit{Notes:} ", note, " Peace time, 1930--1936, is the omitted period; War is 1937--1943; Mass drafting is 1944. Standard errors clustered by section in parentheses. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
    "    \\end{tablenotes}",
    "    \\end{threeparttable}",
    "\\end{frame}",
    ""
  )
}

three_fe_period_table_frame <- function(data, target_variable, title, note, sections = NA_character_, target = NA_character_) {
  specs <- c("Section + occupation + year FE", "No section FE", "No occupation FE")
  rows <- data %>%
    filter(.data$variable == target_variable) %>%
    mutate(
      specification = factor(specification, levels = specs),
      cell = fmt_coef(estimate, p),
      se_cell = fmt_se(se)
    ) %>%
    select(label, specification, cell, se_cell) %>%
    pivot_wider(names_from = specification, values_from = c(cell, se_cell))

  row_lines <- pmap_chr(rows, function(...) {
    row <- list(...)
    coefs <- map_chr(specs, \(spec) get_row_value(row, paste0("cell_", spec)))
    ses <- map_chr(specs, \(spec) get_row_value(row, paste0("se_cell_", spec)))
    paste0(
      "        ", as.character(row$label), " & ", paste(coefs, collapse = " & "), " \\\\\n",
      "        & ", paste(ses, collapse = " & "), " \\\\"
    )
  })

  nobs_row <- data %>%
    filter(.data$variable == target_variable) %>%
    distinct(specification, n) %>%
    mutate(specification = factor(specification, levels = specs)) %>%
    arrange(specification)

  nobs_cells <- map_chr(specs, function(spec) {
    n_value <- nobs_row$n[as.character(nobs_row$specification) == spec]
    if (length(n_value) == 0 || is.na(n_value[1])) "NA" else scales::comma(n_value[1])
  })

  c(
    paste0("\\begin{frame}[t]{", title, "}"),
    if (!is.na(target)) paste0("    \\hypertarget{", target, "}{}") else character(0),
    "    \\scriptsize",
    "    \\centering",
    "    \\begin{threeparttable}",
    "    \\begin{tabular}{lccc}",
    "        \\toprule",
    "        & Section + occ. + year FE & No section FE & No occ. FE \\\\",
    "        \\midrule",
    row_lines,
    "        \\midrule",
    paste0("        Observations & ", paste(nobs_cells, collapse = " & "), " \\\\"),
    if (!is.na(sections)) paste0("        Sections clustered & \\multicolumn{3}{c}{", sections, "} \\\\") else character(0),
    "        Baseline male employment (log) & Yes & Yes & Yes \\\\",
    "        \\bottomrule",
    "    \\end{tabular}",
    "    \\begin{tablenotes}[flushleft]",
    "        \\tiny",
    paste0("        \\item \\textit{Notes:} ", note, " Peace time, 1930--1936, is the omitted period; War is 1937--1943; Mass drafting is 1944. Standard errors clustered by section in parentheses. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
    "    \\end{tablenotes}",
    "    \\end{threeparttable}",
    "\\end{frame}",
    ""
  )
}

within_section_frames <- pmap(
  within_section_plot_specs,
  function(variable, title, subtitle, file, plot_target, table_target) {
    c(
      paste0("\\begin{frame}[t]{", title, "}"),
      paste0("    \\hypertarget{", plot_target, "}{}"),
      "    \\centering",
      paste0("    \\includegraphics[width=0.84\\textwidth]{Results/draft1944_pre_draft_trajectories/", file, "}"),
      "    \\vspace{1mm}",
      "    \\parbox{0.9\\textwidth}{\\centering\\tiny Unit: section $\\times$ team $\\times$ occupation. Conditional on section and position; controls for log cumulative male baseline. Bars show 95\\% confidence intervals.}",
      "\\end{frame}",
      "",
      period_table_frame(
        period_did_results,
        variable,
        paste0(title, " DID"),
        "Outcome is measured in the section $\\times$ team $\\times$ occupation cell, excluding workers later marked drafted in the 1944 TokyoTo directory. Treatment is whether the cell contains any 1944 male draftee. Coefficients are DID estimates relative to peace time.",
        cells = scales::comma(n_distinct(did_panel$cell_id)),
        sections = scales::comma(n_distinct(did_panel$ka_id)),
        target = table_target
      )
    )
  }
) %>%
  unlist()

intensity_frames <- pmap(
  intensity_plot_specs,
  function(variable, title, subtitle, file, plot_target, table_target) {
    c(
      paste0("\\begin{frame}[t]{", title, "}"),
      paste0("    \\hypertarget{", plot_target, "}{}"),
      "    \\centering",
      paste0("    \\includegraphics[width=0.84\\textwidth]{Results/draft1944_pre_draft_trajectories/", file, "}"),
      "    \\vspace{1mm}",
      paste0("    \\parbox{0.9\\textwidth}{\\centering\\tiny ", subtitle, ". Sample: units with at least one 1944 draftee. Dependent variable is 1944 draft count. Conditional on section and position; controls for log cumulative male baseline. Bars show 95\\% confidence intervals.}"),
      "\\end{frame}",
      "",
      period_table_frame(
        intensity_period_did_results,
        variable,
        paste0(title, " DID"),
        "Dependent variable is 1944 draft count in units with at least one 1944 draftee. Coefficients report whether the listed margin predicts draft intensity differently in war or mass-drafting years relative to peace time.",
        cells = scales::comma(n_distinct(intensity_period_panel$cell_id)),
        sections = scales::comma(n_distinct(intensity_period_panel$ka_id)),
        target = table_target
      )
    )
  }
) %>%
  unlist()

draft_intensity_treatment_frames <- pmap(
  tribble(
    ~variable, ~title, ~subtitle, ~file, ~plot_target, ~table_target,
    "n_internal_total", "Draft intensity: internal transfers event study", "Outcome: total internal arrivals; treatment: number of future draftees", "draft_intensity_treatment_internal.pdf", "app:intensity-internal-plot", "app:intensity-internal-table",
    "n_new_hires", "Draft intensity: external hires event study", "Outcome: external new hires; treatment: number of future draftees", "draft_intensity_treatment_new_hires.pdf", "app:intensity-new-hires-plot", "app:intensity-new-hires-table"
  ),
  function(variable, title, subtitle, file, plot_target, table_target) {
    c(
      paste0("\\begin{frame}[t]{", title, "}"),
      paste0("    \\hypertarget{", plot_target, "}{}"),
      "    \\centering",
      paste0("    \\includegraphics[width=0.84\\textwidth]{Results/draft1944_pre_draft_trajectories/", file, "}"),
      "    \\vspace{1mm}",
      paste0("    \\parbox{0.9\\textwidth}{\\centering\\tiny ", subtitle, ". Conditional on section and occupation; controls for log cumulative male baseline. Bars show 95\\% confidence intervals.}"),
      "\\end{frame}",
      "",
      three_fe_period_table_frame(
        draft_intensity_treatment_did_results,
        variable,
        paste0(title, " DID"),
        "Outcome is measured in the section $\\times$ team $\\times$ occupation cell. Treatment is the number of male workers drafted in 1944 from the same future cell.",
        sections = scales::comma(n_distinct(did_panel_treated$ka_id)),
        target = table_target
      )
    )
  }
) %>%
  unlist()

tex <- c(
  "\\begin{frame}[t]{Office pre-trajectories: drafted offices}",
  "    \\centering",
  "    \\includegraphics[width=0.88\\textwidth]{Results/draft1944_pre_draft_trajectories/office_raw_trajectory_total_internal_new_hires.pdf}",
  "    \\vspace{1mm}",
  "    {\\tiny Descriptive office-level 1930--1943 trajectories, grouped by whether the office contains any worker later marked drafted in the 1944 TokyoTo directory. Outcomes exclude those eventual draftees.}",
  "\\end{frame}",
  "",
  "\\begin{frame}[t]{Pre-trends: adjusted office differences}",
  "    \\tiny",
  "    \\centering",
  "    \\begin{threeparttable}",
  "    \\begin{tabular}{lcccccc}",
  "        \\toprule",
  "        & \\multicolumn{6}{c}{\\textit{Adjusted difference: offices with 1944 drafted workers}} \\\\",
  "        \\cmidrule(lr){2-7}",
  "        & 1930 & 1933 & 1936 & 1939 & 1941 & 1943 \\\\",
  "        \\midrule",
  paste0("        ", row_lines),
  "        \\midrule",
  paste0("        Future-drafted offices & \\multicolumn{6}{c}{", scales::comma(office_sample_summary$future_drafted_offices), "} \\\\"),
  paste0("        Future drafted workers & \\multicolumn{6}{c}{", scales::comma(sample_summary$future_drafted_workers), "} \\\\"),
  "        Baseline male employment (log) & \\multicolumn{6}{c}{Yes} \\\\",
  "        \\bottomrule",
  "    \\end{tabular}",
  "    \\begin{tablenotes}[flushleft]",
  "        \\tiny",
  "        \\item \\textit{Notes:} Separate year-by-year office-level OLS differences relative to offices without future draftees. Outcomes exclude workers later marked drafted in the 1944 TokyoTo directory. The table reports selected years from the 1930--1943 trajectory window. Specifications control for log cumulative male baseline employment. Standard errors clustered by office in parentheses. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
  "    \\end{tablenotes}",
  "    \\end{threeparttable}",
  "\\end{frame}",
  "",
  within_section_frames,
  draft_intensity_treatment_frames,
  intensity_frames
)

writeLines(tex, file.path("Results", "Draft1944PreDraftTrajectoriesAppendix.tex"), useBytes = TRUE)

cat("Wrote:\n")
cat(" -", file.path("Results", "Draft1944PreDraftTrajectoriesAppendix.tex"), "\n")
cat(" -", file.path(OUT_DIR, "raw_trajectory_total_internal_new_hires.pdf"), "\n")
cat(" -", file.path(OUT_DIR, "office_raw_trajectory_total_internal_new_hires.pdf"), "\n")
cat(" -", file.path(OUT_DIR, "raw_trajectories.csv"), "\n")
cat(" -", file.path(OUT_DIR, "adjusted_yearly_differences.csv"), "\n")
cat(" -", file.path(OUT_DIR, "within_section_cell_size_did_results.csv"), "\n")
cat(" -", file.path(OUT_DIR, "draft_intensity_orthogonality_differences.csv"), "\n")
