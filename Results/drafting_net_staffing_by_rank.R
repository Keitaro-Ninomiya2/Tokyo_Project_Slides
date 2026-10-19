################################################################################
# Heterogeneity in the net staffing first stage by position rank
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

source(SOURCE_SCRIPT)

d_rank <- panel_ka %>%
  mutate(
    replacements = n_internal_total + n_new_hires,
    net_staffing_change = replacements - n_drafted_male,
    rank_group = factor(
      pos_rank,
      levels = c(1, 2, 3),
      labels = c("Lower rank", "Middle/other rank", "Higher rank")
    )
  ) %>%
  filter(!is.na(rank_group))

# Rank-specific draft slopes in a pooled regression. Position-title fixed
# effects control flexibly for level differences across occupations/ranks.
m_rank <- feols(
  net_staffing_change ~ n_drafted_male:rank_group +
    log(cumul_n_male + 1) |
    year_num + ka_id + pos_norm,
  data = d_rank,
  cluster = ~office_id
)

m_lower <- feols(
  net_staffing_change ~ n_drafted_male + log(cumul_n_male + 1) |
    year_num + ka_id + pos_norm,
  data = filter(d_rank, rank_group == "Lower rank"),
  cluster = ~office_id
)

m_higher <- feols(
  net_staffing_change ~ n_drafted_male + log(cumul_n_male + 1) |
    year_num + ka_id + pos_norm,
  data = filter(d_rank, rank_group == "Higher rank"),
  cluster = ~office_id
)

m_middle <- feols(
  net_staffing_change ~ n_drafted_male + log(cumul_n_male + 1) |
    year_num + ka_id + pos_norm,
  data = filter(d_rank, rank_group == "Middle/other rank"),
  cluster = ~office_id
)

terms <- names(coef(m_rank))[grepl("n_drafted_male:rank_group", names(coef(m_rank)), fixed = TRUE)]
results <- tibble(
  term = terms,
  rank = sub("n_drafted_male:rank_group", "", terms, fixed = TRUE),
  estimate = unname(coef(m_rank)[terms]),
  std_error = unname(se(m_rank)[terms]),
  p_value = unname(pvalue(m_rank)[terms]),
  observations = nobs(m_rank),
  offices = n_distinct(d_rank$office_id)
)

# Pairwise Wald tests of equality between rank-specific slopes.
wald_tests <- bind_rows(lapply(combn(terms, 2, simplify = FALSE), function(pair) {
  difference <- unname(coef(m_rank)[pair[1]] - coef(m_rank)[pair[2]])
  variance <- vcov(m_rank)[pair[1], pair[1]] + vcov(m_rank)[pair[2], pair[2]] -
    2 * vcov(m_rank)[pair[1], pair[2]]
  z <- difference / sqrt(variance)
  tibble(
    comparison = paste(
      sub("n_drafted_male:rank_group", "", pair, fixed = TRUE),
      collapse = " vs. "
    ),
    difference = difference,
    statistic = z^2,
    p_value = pchisq(z^2, df = 1, lower.tail = FALSE)
  )
}))

write_csv(results, file.path(OUT_DIR, "net_staffing_by_rank.csv"))
write_csv(wald_tests, file.path(OUT_DIR, "rank_slope_equality_tests.csv"))
lower_result <- tibble(
  sample = "Lower rank only",
  estimate = unname(coef(m_lower)["n_drafted_male"]),
  std_error = unname(se(m_lower)["n_drafted_male"]),
  p_value = unname(pvalue(m_lower)["n_drafted_male"]),
  first_stage_f = (estimate / std_error)^2,
  observations = nobs(m_lower),
  offices = n_distinct(d_rank$office_id[d_rank$rank_group == "Lower rank"])
)
write_csv(lower_result, file.path(OUT_DIR, "lower_rank_first_stage.csv"))
higher_result <- tibble(
  sample = "Higher rank only",
  estimate = unname(coef(m_higher)["n_drafted_male"]),
  std_error = unname(se(m_higher)["n_drafted_male"]),
  p_value = unname(pvalue(m_higher)["n_drafted_male"]),
  first_stage_f = (estimate / std_error)^2,
  observations = nobs(m_higher),
  offices = n_distinct(d_rank$office_id[d_rank$rank_group == "Higher rank"])
)
write_csv(higher_result, file.path(OUT_DIR, "higher_rank_first_stage.csv"))
middle_result <- tibble(
  sample = "Middle/other rank only",
  estimate = unname(coef(m_middle)["n_drafted_male"]),
  std_error = unname(se(m_middle)["n_drafted_male"]),
  p_value = unname(pvalue(m_middle)["n_drafted_male"]),
  first_stage_f = (estimate / std_error)^2,
  observations = nobs(m_middle),
  offices = n_distinct(d_rank$office_id[d_rank$rank_group == "Middle/other rank"])
)
write_csv(middle_result, file.path(OUT_DIR, "middle_rank_first_stage.csv"))

cat("\nRANK DISTRIBUTION\n")
print(d_rank %>% count(rank_group) %>% mutate(draftees = sapply(rank_group, function(g) sum(d_rank$n_drafted_male[d_rank$rank_group == g]))))
cat("\nNET STAFFING EFFECT BY RANK\n")
print(results)
cat("\nPAIRWISE TESTS\n")
print(wald_tests)
cat("\nLOWER-RANK-ONLY FIRST STAGE\n")
print(lower_result)
cat("\nHIGHER-RANK-ONLY FIRST STAGE\n")
print(higher_result)
cat("\nMIDDLE-RANK-ONLY FIRST STAGE\n")
print(middle_result)
