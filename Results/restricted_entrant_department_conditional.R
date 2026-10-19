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

source("Results/draft1944_pre_draft_trajectories.R")

OUT <- file.path("Results", "draft1944_pre_draft_trajectories")

restricted_cells <- post_merger_entrant_draftees_1944 %>%
  mutate(
    office_id = as.numeric(office_id),
    pos_norm = as.character(pos_norm)
  ) %>%
  filter(
    !is.na(office_id),
    !is.na(pos_norm), pos_norm != ""
  ) %>%
  group_by(office_id, pos_norm) %>%
  summarise(restricted_draft_count_1944 = n_distinct(staff_id), .groups = "drop")

restricted_offices <- post_merger_entrant_draftees_1944 %>%
  mutate(office_id = as.numeric(office_id)) %>%
  filter(!is.na(office_id)) %>%
  distinct(office_id)

baseline_panel <- pre_panel %>%
  mutate(
    dept_id = na_if(as.character(kyoku), ""),
    office_id = as.numeric(office_id),
    kakari = as.character(kakari),
    pos_norm = as.character(pos_norm),
    period2 = case_when(
      year_num <= 1936 ~ "Peace, 1930-1936",
      year_num <= 1943 ~ "War, 1937-1943"
    )
  ) %>%
  semi_join(restricted_offices, by = "office_id") %>%
  left_join(restricted_cells, by = c("office_id", "pos_norm")) %>%
  mutate(restricted_draft_count_1944 = replace_na(restricted_draft_count_1944, 0)) %>%
  filter(!is.na(dept_id), !is.na(pos_norm), !is.na(period2))

cumul_male_stock_1944 <- df %>%
  filter(year_num < 1944, !is_female) %>%
  group_by(office_id, kakari, pos_norm) %>%
  summarise(cumul_n_male = n_distinct(staff_id), .groups = "drop") %>%
  mutate(year_num = 1944L)

panel_1944_office_team <- outcomes_1944_excl_draftees %>%
  left_join(cumul_male_stock_1944, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(
    dept_id = na_if(as.character(kyoku), ""),
    office_id = as.numeric(office_id),
    kakari = as.character(kakari),
    pos_norm = as.character(pos_norm),
    cumul_n_male = replace_na(cumul_n_male, 0),
    log_cumul_male = log(cumul_n_male + 1),
    period2 = "Mass drafting, 1944"
  ) %>%
  semi_join(restricted_offices, by = "office_id") %>%
  left_join(restricted_cells, by = c("office_id", "pos_norm")) %>%
  mutate(restricted_draft_count_1944 = replace_na(restricted_draft_count_1944, 0)) %>%
  filter(!is.na(dept_id), !is.na(pos_norm))

yearly_panel <- bind_rows(
  baseline_panel,
  panel_1944_office_team %>%
    select(any_of(names(baseline_panel)))
)

outcomes <- c(
  n_internal_total = "Total internal",
  n_new_hires = "External hires",
  n_retention = "Retention",
  n_transfer_same_dept = "Same-dept transfers",
  n_transfer_diff_dept = "Diff.-dept transfers"
)

estimate_year <- function(d, y, label) {
  if (n_distinct(d$restricted_draft_count_1944) < 2 ||
      n_distinct(d$dept_id) < 2 ||
      nrow(d) < 10) {
    return(tibble(
      outcome = label, variable = y, estimate = NA_real_, se = NA_real_,
      p_value = NA_real_, n = nrow(d), departments = n_distinct(d$dept_id)
    ))
  }

  m <- tryCatch(
    feols(
      as.formula(paste0(y, " ~ restricted_draft_count_1944 + log_cumul_male | dept_id + pos_norm")),
      data = d,
      cluster = ~dept_id,
      warn = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(m) || !("restricted_draft_count_1944" %in% names(coef(m)))) {
    return(tibble(
      outcome = label, variable = y, estimate = NA_real_, se = NA_real_,
      p_value = NA_real_, n = ifelse(is.null(m), NA_integer_, nobs(m)),
      departments = n_distinct(d$dept_id)
    ))
  }

  tibble(
    outcome = label,
    variable = y,
    estimate = unname(coef(m)["restricted_draft_count_1944"]),
    se = unname(se(m)["restricted_draft_count_1944"]),
    p_value = unname(pvalue(m)["restricted_draft_count_1944"]),
    n = nobs(m),
    departments = n_distinct(d$dept_id)
  )
}

yearly <- imap_dfr(outcomes, function(label, y) {
  map_dfr(1930:1944, function(yr) {
    estimate_year(filter(yearly_panel, year_num == yr), y, label) %>%
      mutate(year_num = yr, period = "Year")
  })
})

period <- imap_dfr(outcomes, function(label, y) {
  map_dfr(c("Peace, 1930-1936", "War, 1937-1943"), function(per) {
    d <- filter(baseline_panel, period2 == per)
    if (n_distinct(d$restricted_draft_count_1944) < 2) {
      return(tibble(
        outcome = label, variable = y, period = per, estimate = NA_real_,
        se = NA_real_, p_value = NA_real_, n = nrow(d),
        departments = n_distinct(d$dept_id)
      ))
    }

    m <- feols(
      as.formula(paste0(y, " ~ restricted_draft_count_1944 + log_cumul_male | dept_id + pos_norm + year_num")),
      data = d,
      cluster = ~dept_id,
      warn = FALSE
    )

    tibble(
      outcome = label,
      variable = y,
      period = per,
      estimate = unname(coef(m)["restricted_draft_count_1944"]),
      se = unname(se(m)["restricted_draft_count_1944"]),
      p_value = unname(pvalue(m)["restricted_draft_count_1944"]),
      n = nobs(m),
      departments = n_distinct(d$dept_id)
    )
  })
})

summary <- baseline_panel %>%
  summarise(
    rows = n(),
    offices = n_distinct(office_id),
    departments = n_distinct(dept_id),
    team_occ_cells = n_distinct(paste(office_id, kakari, pos_norm)),
    exposed_team_occ_cells = n_distinct(paste(office_id, kakari, pos_norm)[restricted_draft_count_1944 > 0]),
    exposed_office_occ_cells = n_distinct(paste(office_id, pos_norm)[restricted_draft_count_1944 > 0]),
    restricted_draftees = sum(distinct(restricted_cells, office_id, pos_norm, restricted_draft_count_1944)$restricted_draft_count_1944)
  )

write_csv(yearly, file.path(OUT, "restricted_entrant_department_conditional_yearly.csv"))
write_csv(period, file.path(OUT, "restricted_entrant_department_conditional_period.csv"))
write_csv(summary, file.path(OUT, "restricted_entrant_department_conditional_summary.csv"))

plot_df <- yearly %>%
  filter(variable %in% c("n_internal_total", "n_new_hires")) %>%
  mutate(
    lo = estimate - 1.96 * se,
    hi = estimate + 1.96 * se
  )

rects <- tibble(
  xmin = c(1929.5, 1936.5, 1943.5),
  xmax = c(1936.5, 1943.5, 1944.5),
  ymin = -Inf,
  ymax = Inf,
  regime = c("Peace", "War", "Mass drafting")
)

p <- ggplot(plot_df, aes(year_num, estimate, color = outcome, group = outcome)) +
  geom_rect(
    data = rects,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = regime),
    inherit.aes = FALSE,
    alpha = 0.45
  ) +
  geom_hline(yintercept = 0, linewidth = 0.35, color = "gray35") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12, linewidth = 0.45, na.rm = TRUE) +
  geom_line(
    data = plot_df %>% filter(!is.na(estimate)),
    linewidth = 0.55
  ) +
  geom_point(size = 1.8, na.rm = TRUE) +
  scale_fill_manual(values = c(Peace = "#dcebf5", War = "#f5e6c8", "Mass drafting" = "#eadce8"), name = NULL) +
  scale_color_manual(values = c("Total internal" = "#1f6f9b", "External hires" = "#8b4fa3"), name = NULL) +
  labs(
    x = NULL,
    y = "Dept.-conditional coefficient per restricted 1944 draftee",
    title = "Restricted draft exposure and hiring patterns"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(
  file.path(OUT, "restricted_entrant_department_conditional_yearly.pdf"),
  p,
  width = 8.2,
  height = 4.4
)

print(summary)
print(
  period %>%
    filter(variable %in% c("n_internal_total", "n_new_hires")) %>%
    mutate(across(c(estimate, se, p_value), ~round(.x, 3))),
  n = Inf
)
