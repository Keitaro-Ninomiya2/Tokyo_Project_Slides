################################################################################
# Did offices with more 1944 draftees receive more of them back, 1945--1950?
#
# A return to the origin office means that a male worker flagged as drafted in
# 1944 is observed again with the same office_id.  The personnel directories
# available in the requested window are 1946, 1948, and 1950; missing directory
# years are not coded as zero.
################################################################################

suppressMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
})

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv"
)
OUT_DIR <- file.path("Results", "drafted_office_return_1945_1950")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

d <- fread(file = DATA_PATH, select = c(
  "staff_id", "year", "is_name", "gender_modern", "drafted", "office_id",
  "kyoku"
))
d <- unique(d[is_name == TRUE][, year := as.integer(year)], by = c("staff_id", "year"))

available_years <- sort(intersect(1945:1950, unique(d$year)))
if (!identical(available_years, c(1946L, 1948L, 1950L))) {
  warning("Unexpected directory coverage in 1945--1950: ",
          paste(available_years, collapse = ", "))
}

base <- d[year == 1944 & gender_modern != "female" & !is.na(office_id)]
base[, drafted_flag := drafted %in% TRUE]
office <- base[, .(
  office_size_1944 = uniqueN(staff_id),
  drafted_1944 = uniqueN(staff_id[drafted_flag]),
  kyoku_1944 = first(na.omit(kyoku))
), by = office_id]
office[!is.finite(office_size_1944), office_size_1944 := 0L]

cohort <- unique(base[drafted_flag == TRUE, .(
  staff_id, origin_office = office_id
)])

# Office-year outcomes.  Reappearance elsewhere is retained as a useful
# benchmark; same-office return is the main outcome.
panel <- CJ(office_id = office$office_id, year = available_years)
panel <- merge(panel, office, by = "office_id", all.x = TRUE)

for (yr in available_years) {
  observed <- unique(d[year == yr, .(staff_id, return_office = office_id)])
  ret <- merge(cohort, observed, by = "staff_id", all = FALSE)
  counts <- ret[, .(
    returned_any = uniqueN(staff_id),
    returned_same = uniqueN(staff_id[return_office == origin_office])
  ), by = .(office_id = origin_office)]
  # Match the computed counts back to the relevant panel rows.  Referring to
  # i.returned_* here would select the panel's prefilled values rather than the
  # values in `counts`, silently replacing every return count with zero.
  year_rows <- which(panel$year == yr)
  count_rows <- match(panel$office_id[year_rows], counts$office_id)
  panel[year_rows, `:=`(
    returned_any = counts$returned_any[count_rows],
    returned_same = counts$returned_same[count_rows]
  )]
}
panel[is.na(returned_any), returned_any := 0L]
panel[is.na(returned_same), returned_same := 0L]
panel[, `:=`(
  same_return_rate = fifelse(drafted_1944 > 0, returned_same / drafted_1944, NA_real_),
  any_return_rate = fifelse(drafted_1944 > 0, returned_any / drafted_1944, NA_real_)
)]
fwrite(panel, file.path(OUT_DIR, "office_year_returns.csv"))

# Main count specifications.  The unadjusted coefficient answers the literal
# counts question.  Controlling for 1944 office size separates draft volume
# from general office scale.  SEs are clustered by origin office.
m_raw <- feols(returned_same ~ drafted_1944 | year, panel, cluster = ~office_id)
m_size <- feols(returned_same ~ drafted_1944 + office_size_1944 | year,
                panel, cluster = ~office_id)
m_any_size <- feols(returned_any ~ drafted_1944 + office_size_1944 | year,
                    panel, cluster = ~office_id)

# Among offices with at least one draftee, test whether larger drafted cohorts
# had a larger fraction return to the same office (weighted by cohort size).
rate_panel <- panel[drafted_1944 > 0]
m_rate <- feols(same_return_rate ~ drafted_1944 + office_size_1944 | year,
                rate_panel, weights = ~drafted_1944, cluster = ~office_id)

tidy_one <- function(model, term, label, offices) {
  ct <- coeftable(model)
  data.table(
    specification = label,
    outcome = as.character(model$fml[[2]]),
    term = term,
    estimate = unname(coef(model)[term]),
    std_error = unname(se(model)[term]),
    p_value = unname(pvalue(model)[term]),
    n = nobs(model),
    n_offices = uniqueN(offices)
  )
}

results <- rbindlist(list(
  tidy_one(m_raw, "drafted_1944", "Return count: year FE only", panel$office_id),
  tidy_one(m_size, "drafted_1944", "Return count: + 1944 office size", panel$office_id),
  tidy_one(m_any_size, "drafted_1944", "Any-government return count: + office size", panel$office_id),
  tidy_one(m_rate, "drafted_1944", "Same-office return rate: + office size", rate_panel$office_id)
), fill = TRUE)
fwrite(results, file.path(OUT_DIR, "regression_results.csv"))

yearly <- panel[, .(
  offices = .N,
  offices_with_draftees = sum(drafted_1944 > 0),
  draftees = sum(drafted_1944),
  returned_same = sum(returned_same),
  returned_any = sum(returned_any),
  same_return_rate = sum(returned_same) / sum(drafted_1944),
  any_return_rate = sum(returned_any) / sum(drafted_1944)
), by = year]
fwrite(yearly, file.path(OUT_DIR, "yearly_summary.csv"))

# Cumulative unique returns through 1950 (a worker counts once).
post <- unique(d[year %in% available_years, .(staff_id, return_office = office_id)])
cum <- merge(cohort, post, by = "staff_id", all = FALSE)[, .(
  ever_returned_any = uniqueN(staff_id),
  ever_returned_same = uniqueN(staff_id[return_office == origin_office])
), by = .(office_id = origin_office)]
cum <- merge(office, cum, by = "office_id", all.x = TRUE)
cum[is.na(ever_returned_any), ever_returned_any := 0L]
cum[is.na(ever_returned_same), ever_returned_same := 0L]
fwrite(cum, file.path(OUT_DIR, "office_cumulative_returns.csv"))

p <- ggplot(panel, aes(drafted_1944, returned_same)) +
  geom_point(alpha = .35, size = 1.2, colour = "#1f5a85") +
  geom_smooth(method = "lm", se = TRUE, linewidth = .6, colour = "#9b2f2f") +
  facet_wrap(~year) +
  labs(x = "Male workers drafted in 1944 (origin office)",
       y = "Draftees back in the same office",
       caption = "Only years with an available personnel directory are shown.") +
  theme_minimal(base_size = 10)
ggsave(file.path(OUT_DIR, "drafting_and_same_office_returns.pdf"), p,
       width = 7, height = 3.2, units = "in")

cat("Available years:", paste(available_years, collapse = ", "), "\n")
print(yearly)
print(results)
