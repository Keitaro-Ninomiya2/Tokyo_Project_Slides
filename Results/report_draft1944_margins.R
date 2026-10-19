# Keep generated text free of trailing whitespace and blank final lines.
write_clean_lines <- function(text, con, useBytes = FALSE) {
  text <- trimws(text, which = "right")
  text <- text[seq_len(max(c(0L, which(nzchar(text)))))]
  writeLines(text, con, useBytes = useBytes)
}

# Reporting stage for run_draft1944_analysis.R. Requires the freshly built panels.
margin_vars <- c("n_internal_total", "n_retention", "n_transfer_same_dept",
                 "n_transfer_diff_dept", "n_new_hires")
margin_labels <- c("Total internal", "Retention", "Same dept.", "Diff. dept.", "External hires")

# Same office/team/occupation exposure and section/occupation FE as the pooled
# benchmark, restricted to 1944 and excluding 1944 male draftees from outcomes.
replacement_1944_panel <- outcomes_1944_excl_draftees %>%
  left_join(cumul_male_stock, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  left_join(position_drafts, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(
    across(c(cumul_n_male, n_drafted_male), ~replace_na(.x, 0)),
    log_cumul_male = log1p(cumul_n_male),
    ka_id = if_else(!is.na(ka) & !is.na(kyoku), paste(kyoku, ka, sep = "_"), NA_character_)
  ) %>%
  filter(!is.na(ka_id))
stopifnot(all(replacement_1944_panel$year_num == 1944),
          any(replacement_1944_panel$cumul_n_male > 0),
          any(panel_1944$cumul_n_male > 0))

fit_margin_set <- function(d, treatment, fixed_effects, sample_label) {
  models <- setNames(map(margin_vars, function(y) {
    feols(as.formula(paste0(y, " ~ ", treatment,
                           " + log_cumul_male | ", fixed_effects)),
          data = d, cluster = ~office_id, warn = FALSE)
  }), margin_vars)
  # OLS additivity only holds when all five models use exactly the same rows.
  stopifnot(all(map_lgl(models, ~identical(obs(.x), obs(models[[1]])))))
  results <- imap_dfr(models, function(m, y) {
    tibble(sample = sample_label, variable = y, outcome = margin_labels[match(y, margin_vars)],
           estimate = unname(coef(m)[treatment]), se = unname(se(m)[treatment]),
           p_value = unname(pvalue(m)[treatment]), n = nobs(m),
           offices = n_distinct(d$office_id[obs(m)]))
  })
  b <- setNames(results$estimate, results$variable)
  stopifnot(all(is.finite(b)), abs(b['n_internal_total'] - sum(b[margin_vars[2:4]])) < 1e-8,
            abs(b['n_new_hires']) > 1e-10)
  summary <- tibble(sample = sample_label,
                    internal = b[['n_internal_total']], external = b[['n_new_hires']],
                    internal_external_ratio = internal / external,
                    retention = b[['n_retention']], same_department = b[['n_transfer_same_dept']],
                    different_department = b[['n_transfer_diff_dept']],
                    decomposition_error = internal - retention - same_department - different_department,
                    n = nobs(models[[1]]), offices = results$offices[1])
  list(models = models, results = results, summary = summary)
}

margins_1944 <- fit_margin_set(replacement_1944_panel, "n_drafted_male", "ka_id + pos_norm", "1944")
pooled_panel <- panel_ka %>% mutate(log_cumul_male = log1p(cumul_n_male))
margins_pooled <- fit_margin_set(pooled_panel, "n_drafted_male", "year_num + ka_id + pos_norm", "1938-1945 pooled")
# Independent agreement with the upstream benchmark models.
stopifnot(abs(margins_pooled$summary$internal - unname(coef(m_internal)['n_drafted_male'])) < 1e-10,
          abs(margins_pooled$summary$external - unname(coef(m_new_hires)['n_drafted_male'])) < 1e-10)

entrant_counts <- post_merger_entrant_draftees_1944 %>%
  group_by(office_id, kakari, pos_norm) %>%
  summarise(n_entrant_drafted = n_distinct(staff_id), .groups = "drop") %>% mutate(year_num = 1944L)
entrant_panel <- pooled_panel %>%
  left_join(entrant_counts, by = c("office_id", "kakari", "pos_norm", "year_num")) %>%
  mutate(n_entrant_drafted = replace_na(n_entrant_drafted, 0L))
margins_entrant <- fit_margin_set(entrant_panel, "n_entrant_drafted", "year_num + ka_id + pos_norm", "1938-1945 pooled, entrant exposure")

write_csv(bind_rows(margins_1944$results, margins_pooled$results, margins_entrant$results),
          file.path(OUT_DIR, "replacement_margin_coefficients.csv"))
write_csv(bind_rows(margins_1944$summary, margins_pooled$summary, margins_entrant$summary),
          file.path(OUT_DIR, "replacement_margin_ratios.csv"))
write_csv(replacement_1944_panel, file.path(OUT_DIR, "replacement_1944_analysis_cells.csv"))
legacy_entrant <- margins_entrant$results %>%
  select(variable, estimate, se, p_value, n, offices) %>%
  mutate(total_entrant_drafted = sum(entrant_panel$n_entrant_drafted))
write_csv(legacy_entrant, file.path(OUT_DIR, "replacement_margins_post_merger_entrant_draftees.csv"))
write_csv(legacy_entrant, file.path(OUT_DIR, "restricted_entrant_replacement_margins.csv"))

margin_plot <- margins_1944$results %>%
  mutate(outcome = factor(outcome, levels = rev(margin_labels))) %>%
  ggplot(aes(estimate, outcome)) +
  geom_vline(xintercept = 0, color = "gray60", linewidth = 0.4) +
  geom_errorbar(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se),
                orientation = "y", width = 0.15, color = "#536878") +
  geom_point(color = "#1f5a85", size = 2) +
  labs(x = "Conditional response per additional male drafted in 1944", y = NULL,
       caption = "Internal total = retention + same-department + different-department transfers.\n95% normal confidence intervals; standard errors clustered by office.") +
  theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
ggsave(file.path(OUT_DIR, "replacement_margins_1944.pdf"), margin_plot, width = 7.5, height = 3.5)

# Regenerate the previously hand-maintained three-FE comparison table.
three_fe_specs <- c("Section + occupation + year FE" = "ka_id + pos_norm + year_num",
                    "No section FE" = "pos_norm + year_num", "No occupation FE" = "ka_id + year_num")
period_fit <- function(d, y, label, treatment, specs) {
  imap_dfr(specs, function(fes, spec) {
    m <- feols(as.formula(paste0(y, " ~ ", treatment, ":war_period + ", treatment,
                               ":mass_drafting_year + log_cumul_male | ", fes)),
               data = d, cluster = ~ka_id, warn = FALSE)
    terms <- paste0(treatment, c(":war_period", ":mass_drafting_year"))
    tibble(outcome = label, variable = y, specification = spec, term = terms,
           label = if (treatment == "draft_treatment_count")
             c("No. future drafted $\\times$ War", "No. future drafted $\\times$ Mass drafting") else
             c("Future drafted cell $\\times$ War", "Future drafted cell $\\times$ Mass drafting"),
           estimate = unname(coef(m)[terms]), se = unname(se(m)[terms]),
           p = unname(pvalue(m)[terms]), n = nobs(m), sections = n_distinct(d$ka_id[obs(m)]))
  })
}
three_fe_results <- imap_dfr(c(n_internal_total = "Total internal", n_new_hires = "External hires"),
  ~period_fit(did_panel, .y, .x, "any_future_drafted_1944", three_fe_specs))
write_csv(three_fe_results, file.path(OUT_DIR, "within_section_period_did_three_fe_results.csv"))

# Restricted-cohort sensitivity: same panels/outcomes; only draft exposure changes.
restricted_pre <- df %>% filter(year_num == 1943) %>%
  semi_join(post_merger_entrant_draftees_1944, by = "staff_id") %>%
  group_by(ka, kakari, pos_norm) %>% summarise(restricted_pre_count = n_distinct(staff_id), .groups = "drop")
restricted_post <- post_merger_entrant_draftees_1944 %>%
  group_by(ka, kakari, pos_norm) %>% summarise(restricted_post_count = n_distinct(staff_id), .groups = "drop")
restricted_did_panel <- did_panel %>%
  left_join(restricted_pre, by = c("ka", "kakari", "pos_norm")) %>%
  left_join(restricted_post, by = c("ka", "kakari", "pos_norm")) %>%
  mutate(draft_treatment_count = if_else(year_num == 1944, replace_na(restricted_post_count, 0L),
                                       replace_na(restricted_pre_count, 0L)),
         any_future_drafted_1944 = as.integer(draft_treatment_count > 0))
restricted_period <- imap_dfr(intensity_outcomes, ~period_fit(restricted_did_panel, .y, .x,
  "any_future_drafted_1944", c("Cell + year FE" = "cell_id + year_num", "Section/position + year FE" = "ka_id + pos_norm + year_num")))
restricted_intensity <- imap_dfr(intensity_outcomes, ~period_fit(filter(restricted_did_panel, draft_treatment_count > 0), .y, .x,
  "draft_treatment_count", three_fe_specs))
write_csv(restricted_period, file.path(OUT_DIR, "restricted_entrant_within_section_period_did_results.csv"))
write_csv(restricted_intensity, file.path(OUT_DIR, "restricted_entrant_draft_intensity_treatment_did_results.csv"))
restricted_yearly <- imap_dfr(c(n_internal_total = "Total internal", n_new_hires = "External hires"), function(label, y) {
  map_dfr(1930:1944, function(yr) {
    d <- filter(restricted_did_panel, year_num == yr)
    map_dfr(c("any_future_drafted_1944", "draft_treatment_count"), function(treatment) {
      if (treatment == "draft_treatment_count") d <- filter(d, draft_treatment_count > 0)
      m <- tryCatch(feols(as.formula(paste0(y, " ~ ", treatment, " + log_cumul_male | ka_id + pos_norm")),
                          data = d, cluster = ~ka_id, warn = FALSE), error = function(e) NULL)
      tibble(variable = y, outcome = label, year_num = yr, treatment = treatment,
             estimate = if (is.null(m)) NA_real_ else unname(coef(m)[treatment]),
             se = if (is.null(m)) NA_real_ else unname(se(m)[treatment]),
             p = if (is.null(m)) NA_real_ else unname(pvalue(m)[treatment]), n = if (is.null(m)) nrow(d) else nobs(m))
    })
  })
})
write_csv(restricted_yearly, file.path(OUT_DIR, "restricted_entrant_yearly_differences.csv"))
for (y in c("n_internal_total", "n_new_hires")) for (treatment in c("any_future_drafted_1944", "draft_treatment_count")) {
  d <- filter(restricted_yearly, variable == y, .data$treatment == .env$treatment)
  p <- ggplot(d, aes(year_num, estimate)) +
    geom_rect(data = period_rects, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period),
              inherit.aes = FALSE, alpha = 0.55) +
    geom_hline(yintercept = 0, color = "gray45", linewidth = 0.4) +
    geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), width = 0.12, na.rm = TRUE) +
    geom_line(color = "#1f5a85", na.rm = TRUE) + geom_point(color = "#1f5a85", na.rm = TRUE) +
    scale_fill_manual(values = setNames(period_rects$fill, period_rects$period), name = NULL) +
    scale_x_continuous(breaks = seq(1930, 1944, 2)) + labs(x = NULL, y = "Conditional coefficient") +
    theme_minimal(base_size = 11)
  suffix <- if (y == "n_internal_total") "internal" else "new_hires"
  prefix <- if (treatment == "draft_treatment_count") "restricted_entrant_draft_intensity_treatment_" else "restricted_entrant_within_section_pretrend_"
  ggsave(file.path(OUT_DIR, paste0(prefix, suffix, ".pdf")), p, width = 6.8, height = 3.8)
}

# Main-deck and appendix tables come directly from the model results.
replacement_frame <- function(result, title, target = NULL, note) {
  r <- result$results
  c(paste0("\\begin{frame}[t]{", title, "}"),
    if (!is.null(target)) paste0("\\hypertarget{", target, "}{}"),
    "\\tiny\\centering", "\\begin{threeparttable}", "\\begin{tabular}{lccccc}", "\\toprule",
    " & Total internal & Retention & Same dept. & Diff. dept. & External hires \\\\", "\\midrule",
    paste0("Male drafted workers & ", paste(fmt_coef(r$estimate, r$p_value), collapse = " & "), " \\\\"),
    paste0(" & ", paste(fmt_se(r$se), collapse = " & "), " \\\\"), "\\midrule",
    paste0("Observations & \\multicolumn{5}{c}{", scales::comma(r$n[1]), "} \\\\"),
    paste0("Offices clustered & \\multicolumn{5}{c}{", scales::comma(r$offices[1]), "} \\\\"),
    "Section and occupation FE & \\multicolumn{5}{c}{Yes} \\\\",
    "Log cumulative male baseline & \\multicolumn{5}{c}{Yes} \\\\",
    if (result$summary$sample != "1944") "Year FE & \\multicolumn{5}{c}{Yes} \\\\",
    "\\bottomrule", "\\end{tabular}", "\\begin{tablenotes}[flushleft]",
    paste0("\\tiny\\item \\textit{Notes:} ", note,
           " Internal total includes retention, same-department transfers, and different-department transfers. Standard errors clustered by office. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."),
    "\\end{tablenotes}", "\\end{threeparttable}", "\\vspace{2mm}",
    sprintf("\\par Internal / external $= %.4f / %.4f = \\mathbf{%.2f:1}$.", result$summary$internal, result$summary$external, result$summary$internal_external_ratio),
    "\\end{frame}", "")
}

note_1944 <- paste0("OLS, 1944 only. Unit: observed destination office $\\times$ team $\\times$ occupation cell, with section identifiers retained. ",
                    "Outcomes exclude male workers marked drafted in 1944. Exposure is the male draft count in the same office-team-occupation cell. ",
                    "Cells without observed non-draftee outcomes are outside the sample. These are conditional associations, not identified causal replacement effects.")
note_pooled <- "OLS benchmark over the 1938--1945 window (observed directories: 1938--1944), with year fixed effects. Exposure is the contemporaneous male draft count; this is not a 1944-only estimate."
note_entrant <- "OLS over the 1938--1945 window (observed directories: 1938--1944), with year fixed effects. Exposure counts male post-merger entrants drafted in 1944; outcomes follow the pooled benchmark definition."
primary_frames <- c(
  replacement_frame(margins_1944, "1944 draft margins: internal and external response", note = note_1944),
  "\\begin{frame}[t]{1944 draft margins: decomposition}", "\\centering",
  paste0("\\includegraphics[width=0.92\\textwidth]{", OUT_DIR, "/replacement_margins_1944.pdf}"),
  "\\end{frame}", "",
  replacement_frame(margins_pooled, "Draft margins: pooled 1938--1945 benchmark", note = note_pooled),
  replacement_frame(margins_entrant, "Post-merger entrant drafting: pooled benchmark", note = note_entrant),
  "\\begin{frame}[plain]\\centering\\vfill{\\Large\\bfseries Appendix}\\vfill\\end{frame}", ""
)
main_period_frames <- c(
  three_fe_period_table_frame(draft_intensity_treatment_did_results, "n_internal_total",
                             "Draft intensity: total internal response DID", "Outcome: total internal response, including retention. Treatment: 1944 male draft count.",
                             sections = scales::comma(n_distinct(did_panel_treated$ka_id))),
  three_fe_period_table_frame(draft_intensity_treatment_did_results, "n_new_hires",
                             "Draft intensity: external hires DID", "Outcome: external hires. Treatment: 1944 male draft count.",
                             sections = scales::comma(n_distinct(did_panel_treated$ka_id))),
  three_fe_period_table_frame(three_fe_results, "n_internal_total", "Total internal response: period DID",
                             "Outcome: total internal response, including retention. Treatment: any male 1944 draftee in the cell."),
  three_fe_period_table_frame(three_fe_results, "n_new_hires", "External hires: period DID",
                             "Outcome: external hires. Treatment: any male 1944 draftee in the cell.")
)
plot_main_frame <- function(title, file) c(
  paste0("\\begin{frame}[t]{", title, "}"), "\\centering",
  paste0("\\includegraphics[width=0.84\\textwidth]{", OUT_DIR, "/", file, "}"),
  "\\parbox{0.9\\textwidth}{\\centering\\tiny Separate year-by-year conditional associations with 1944 draft exposure; section and occupation FE and log cumulative male control. Bars show 95\\% normal confidence intervals.}",
  "\\end{frame}", "")
write_clean_lines(c(primary_frames, main_period_frames,
             plot_main_frame("Draft intensity: total internal yearly associations", "draft_intensity_treatment_internal.pdf"),
             plot_main_frame("Draft intensity: external hires yearly associations", "draft_intensity_treatment_new_hires.pdf"),
             plot_main_frame("Total internal response: yearly associations", "within_section_pretrend_internal.pdf"),
             plot_main_frame("External hires: yearly associations", "within_section_pretrend_new_hires.pdf")),
           "Results/Draft1944Main.tex", useBytes = TRUE)

# Preserve unrelated bilateral-mobility and career-result slides.
main_tex <- readLines("Results/Results.tex", warn = FALSE)
keep_from <- match("\\begin{frame}[t]{Bilateral mobility: nested organizational distance}", main_tex)
stopifnot(!is.na(keep_from))
write_clean_lines(c("% Generated draft-margin slides: run Results/run_draft1944_analysis.R.",
             "\\input{Results/Draft1944Main.tex}", "", main_tex[keep_from:length(main_tex)]), "Results/Results.tex", useBytes = TRUE)

# Keep the geographic benchmark only if its published aggregate coefficients
# and sample reproduce. The new 1944 table is a separate appendix target.
geographic <- readLines("Results/DraftingReplacementAppendix.tex", warn = FALSE)
geo_start <- match("\\begin{frame}[t]{Drafting and replacement margins: geographic benchmark}", geographic)
stopifnot(!is.na(geo_start), margins_pooled$summary$n == 7409,
          all(abs(margins_pooled$results$estimate - c(0.4079, 0.2269, 0.1001, 0.0809, 0.2057)) < 0.00005))
write_clean_lines(c(replacement_frame(margins_1944, "1944 draft margins: full decomposition", "app:replacement-1944", note_1944),
             geographic[geo_start:length(geographic)]), "Results/DraftingReplacementAppendix.tex", useBytes = TRUE)

# Refresh the 1943 placebo from the actual 1943 locations of future draftees.
placebo_panel <- pre_panel %>% filter(year_num == 1943)
placebo <- fit_margin_set(placebo_panel, "n_future_drafted_1944", "ka_id + pos_norm", "1943 placebo")
write_csv(placebo$results, file.path(OUT_DIR, "placebo_1943_replacement_margins.csv"))
placebo_frame <- replacement_frame(placebo, "Placebo: future drafting and prior replacements", note =
  "Outcomes are 1943 replacement margins, excluding 1944 male draftees. Exposure is the future male draft count at each worker's actual 1943 location. Section and occupation FE; no year FE in this single-year placebo.")
# The ratio is not an economically meaningful replacement ratio for a placebo.
placebo_frame <- placebo_frame[!grepl("Internal / external|^Year FE", placebo_frame)]
placebo_frame <- placebo_frame[seq_len(max(which(nzchar(placebo_frame))))]
write_clean_lines(placebo_frame, "Results/Draft1944PretrendAppendix.tex", useBytes = TRUE)

# Generate the restricted department-conditional appendix from its CSV objects.
eol <- paste0(" ", intToUtf8(c(92, 92)))
period_order <- c("Peace, 1930-1936", "War, 1937-1943")
restricted_rows <- unlist(map(margin_vars, function(y) {
  d <- period %>% filter(variable == y) %>% arrange(match(period, period_order))
  c(paste0(margin_labels[match(y, margin_vars)], " & ", paste(fmt_coef(d$estimate, d$p_value), collapse = " & "), eol),
    paste0(" & ", paste(fmt_se(d$se), collapse = " & "), eol))
}))
support_names <- c(rows = "Baseline cell-year rows", offices = "Offices with restricted draft exposure",
                   departments = "Departments in baseline panel", team_occ_cells = "Baseline office-team-occupation cells",
                   exposed_team_occ_cells = "Exposed baseline cells", exposed_office_occ_cells = "Exposed office-occupation cells",
                   restricted_draftees = "Male post-merger entrant draftees")
write_clean_lines(c("\\begin{frame}[t]{Restricted exposure: baseline period correlations}",
  "\\hypertarget{app:restricted-entrant-baseline-period}{}", "\\scriptsize\\centering",
  "\\begin{threeparttable}", "\\begin{tabular}{lcc}", "\\toprule",
  paste0(" & Peace, 1930--1936 & War, 1937--1943", eol), "\\midrule", restricted_rows,
  "\\midrule", paste0("Observations & ", paste(map_int(period_order, ~max(period$n[period$period == .x])), collapse = " & "), eol),
  paste0("Departments in input panel & ", paste(map_int(period_order, ~max(period$departments[period$period == .x])), collapse = " & "), eol),
  paste0("Department, occupation, year FE & Yes & Yes", eol),
  paste0("Log cumulative male baseline & Yes & Yes", eol), "\\bottomrule", "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\tiny",
  "\\item \\textit{Notes:} Baseline office-team-occupation-year cells in offices with male post-merger entrant draftees. Exposure: restricted 1944 draft count at office-occupation level. Internal total includes retention. Standard errors clustered by department. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$.",
  "\\end{tablenotes}\\end{threeparttable}\\end{frame}", "",
  "\\begin{frame}[t]{Restricted exposure: support}", "\\hypertarget{app:restricted-entrant-support}{}",
  "\\scriptsize\\centering\\begin{tabular}{lc}\\toprule",
  imap_chr(support_names, ~paste0(.x, " & ", scales::comma(summary[[.y]]), eol)),
  "\\bottomrule\\end{tabular}\\end{frame}"), "Results/RestrictedEntrantDraftAppendix.tex", useBytes = TRUE)

inputs <- c(personnel_master = DATA_PATH, replacement_source = SOURCE_SCRIPT,
            entrant_classification = ENTRANT_CLASSIFICATION, ka_group_map = KA_GROUP_PATH)
manifest <- tibble(input = names(inputs), path = unname(inputs),
                   bytes = as.numeric(file.info(inputs)$size), md5 = unname(tools::md5sum(inputs)))
write_csv(manifest, file.path(OUT_DIR, "input_manifest.csv"))
write_clean_lines(c(capture.output(sessionInfo())), file.path(OUT_DIR, "R_session_info.txt"))
write_clean_lines(c("Validated: same estimation rows for all five margins in each replacement specification.",
             "Validated: retention + same-department + different-department coefficients equal internal total (<1e-8).",
             "Validated: pooled benchmark agrees with the upstream model coefficients (<1e-10).",
             "Validated: 1944 cumulative-male baseline contains positive historical counts.",
             capture.output(print(bind_rows(margins_1944$summary, margins_pooled$summary, margins_entrant$summary), width = Inf))),
           file.path(OUT_DIR, "validation.txt"))
print(bind_rows(margins_1944$summary, margins_pooled$summary, margins_entrant$summary), width = Inf)

# Office-cell robustness with department FE, using the office-level cumulative
# baseline (including 1944) rather than the section-matched baseline.
department_drafts_1944 <- df_all %>% filter(year_num == 1944, drafted %in% TRUE, !is_female) %>%
  group_by(office_id, kyoku, ka, kakari, pos_norm, year_num) %>%
  summarise(full_cell_drafted = n_distinct(staff_id), .groups = "drop")
department_panel_1944 <- replacement_1944_panel %>%
  left_join(department_drafts_1944, by = c("office_id", "kyoku", "ka", "kakari", "pos_norm", "year_num"))
department_did_panel <- bind_rows(
  pre_panel %>% transmute(office_id, kyoku, ka_id, kakari, pos_norm, year_num,
                         across(all_of(names(intensity_outcomes))), log_cumul_male,
                         draft_treatment_count = n_future_drafted_1944),
  department_panel_1944 %>% transmute(office_id, kyoku, ka_id, kakari, pos_norm, year_num,
                                     across(all_of(names(intensity_outcomes))), log_cumul_male,
                                     draft_treatment_count = replace_na(full_cell_drafted, 0L))
) %>% mutate(team_id = paste(ka_id, kakari, sep = "||"),
             dept_id = na_if(as.character(kyoku), ""),
             war_period = as.integer(year_num >= 1937 & year_num <= 1943),
             mass_drafting_year = as.integer(year_num == 1944)) %>%
  filter(draft_treatment_count > 0, !is.na(dept_id))
department_specs <- c("Department + occupation + year FE" = "dept_id + pos_norm + year_num",
                      "Department + team + occupation + year FE" = "dept_id + team_id + pos_norm + year_num",
                      "Section + occupation + year FE" = "ka_id + pos_norm + year_num")
department_results <- imap_dfr(intensity_outcomes, ~period_fit(department_did_panel, .y, .x,
  "draft_treatment_count", department_specs)) %>%
  rename(p_value = p) %>% mutate(departments = n_distinct(department_did_panel$dept_id))
write_csv(department_results, file.path(OUT_DIR, "draft_intensity_within_department_did_results.csv"))
department_plot <- department_results %>% filter(variable %in% c("n_internal_total", "n_new_hires")) %>%
  mutate(term = if_else(grepl("war_period", term), "War vs. peace", "1944 vs. peace"),
         outcome = if_else(variable == "n_internal_total", "Total internal (including retention)", "External hires")) %>%
  ggplot(aes(estimate, specification, color = outcome)) +
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_errorbar(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se),
                position = position_dodge(width = 0.5), orientation = "y", width = 0.15) +
  geom_point(position = position_dodge(width = 0.5)) + facet_wrap(~term, scales = "free_x") +
  labs(x = "Coefficient per future male draftee", y = NULL, color = NULL) +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave(file.path(OUT_DIR, "draft_intensity_within_department_main_margins.png"), department_plot,
       width = 11, height = 4, dpi = 160, device = "png")
write_csv(panel_1944 %>% select(ka, kakari, pos_norm, cumul_n_male, log_cumul_male),
          file.path(OUT_DIR, "matched_1944_baseline_audit.csv"))
