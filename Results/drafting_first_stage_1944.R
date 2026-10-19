################################################################################
# First stage: 1944 staffing net of draftees on the number drafted
# Unit: office_id x position title
################################################################################

suppressMessages({
  library(data.table)
  library(fixest)
})

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"), "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data", "Tokyo_Personnel_Master_All_Years.csv"
)
OUT_DIR <- file.path("Results", "drafting_first_stage_1944")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
BASELINE_STOCK_PATH <- file.path(
  Sys.getenv("USERPROFILE"), "Documents", "GitHub", "Tokyo_Project_Figures",
  "MainResults", "TransferPatternsBy1944Stock",
  "tables_full_office_crosswalk_corrected_fu_shi", "stock_by_1944_office.csv"
)

d <- fread(DATA_PATH, select = c(
  "staff_id", "year", "is_name", "gender_modern", "drafted", "office_id",
  "position", "kyoku", "ka"
))
d[, year := as.integer(year)]
d[, position_norm := gsub("\\s+", "", position)]
d <- unique(d[
  is_name == TRUE & gender_modern != "female" & !is.na(office_id) &
    !is.na(position_norm) & position_norm != ""
], by = c("staff_id", "year"))

# Construct the 1944 office x position cross-section.  The outcome is the
# number of named male workers left in the cell after removing draftees.
cells44 <- d[year == 1944, .(
  total_staff_1944 = uniqueN(staff_id),
  draftees_1944 = uniqueN(staff_id[drafted %in% TRUE]),
  staff_net_draftees_1944 = uniqueN(staff_id[!(drafted %in% TRUE)]),
  kyoku_1944 = first(na.omit(kyoku)),
  ka_1944 = first(na.omit(ka))
), by = .(office_id, position_norm)]

# Same-cell staffing immediately before mass drafting.  Missing matches are
# recorded as zero and a match indicator is included in the controlled model.
cells43 <- d[year == 1943, .(
  staff_1943 = uniqueN(staff_id)
), by = .(office_id, position_norm)]
cells44 <- merge(cells44, cells43,
                 by = c("office_id", "position_norm"), all.x = TRUE)
cells44[, matched_1943 := as.integer(!is.na(staff_1943))]
cells44[is.na(staff_1943), staff_1943 := 0L]

# Historical office distribution, mapped through the corrected Fu/Shi-to-1944
# crosswalk.  This is the mean male roster stock of all mapped predecessor
# offices over 1938--1943.  It varies at the 1944 kyoku x ka level and is then
# attached to each position cell within that office.
stock <- fread(BASELINE_STOCK_PATH, select = c(
  "1944_bureau", "1944_office", "mean_supply_stock_all_links",
  "mapped_years_all_links"
))
normalize_name <- function(x) gsub("\\s+", "", x)
normalize_bureau <- function(x) {
  x <- normalize_name(x)
  x <- sub("局$", "", x)
  x[x == "計晝"] <- "計画"
  x
}
normalize_office <- function(x, bureau) {
  x <- normalize_name(x)
  # Some roster rows repeat the bureau name inside the ka label (for example,
  # 交通局経理課).  Remove that prefix before matching the crosswalk.
  raw_bureau <- normalize_name(bureau)
  plain_bureau <- normalize_bureau(bureau)
  for (i in seq_along(x)) {
    prefixes <- unique(c(raw_bureau[i], plain_bureau[i]))
    prefixes <- prefixes[!is.na(prefixes) & prefixes != ""]
    for (prefix in prefixes) {
      x[i] <- sub(paste0("^", prefix), "", x[i])
    }
  }
  x
}
stock[, `:=`(
  kyoku_key = normalize_bureau(`1944_bureau`),
  ka_key = normalize_name(`1944_office`),
  baseline_office_stock = as.numeric(mean_supply_stock_all_links),
  baseline_years_observed = as.integer(mapped_years_all_links)
)]
stock <- unique(stock[, .(
  kyoku_key, ka_key, baseline_office_stock, baseline_years_observed
)])
cells44[, `:=`(
  kyoku_key = normalize_bureau(kyoku_1944),
  ka_key = normalize_office(ka_1944, kyoku_1944)
)]
cells44 <- merge(cells44, stock,
                 by = c("kyoku_key", "ka_key"), all.x = TRUE)
baseline_sample <- cells44[
  !is.na(baseline_office_stock) & baseline_years_observed > 0
]

# The raw model is the requested first stage.  Position fixed effects compare
# offices within titles.  A direct 1943 office_id control is not estimated:
# merger-related identifier changes leave only a handful of matched cells.
m_raw <- feols(staff_net_draftees_1944 ~ draftees_1944, cells44,
               cluster = ~office_id)
m_pos <- feols(staff_net_draftees_1944 ~ draftees_1944 | position_norm,
               cells44, cluster = ~office_id)
m_baseline_sample <- feols(
  staff_net_draftees_1944 ~ draftees_1944 | position_norm,
  baseline_sample, cluster = ~office_id
)
m_baseline <- feols(
  staff_net_draftees_1944 ~ draftees_1944 + log1p(baseline_office_stock) |
    position_norm,
  baseline_sample, cluster = ~office_id
)

tidy_model <- function(model, label, model_data) {
  data.table(
    specification = label,
    dependent_variable = "1944 male staff excluding draftees",
    independent_variable = "Number of male draftees in 1944",
    estimate = unname(coef(model)["draftees_1944"]),
    std_error = unname(se(model)["draftees_1944"]),
    p_value = unname(pvalue(model)["draftees_1944"]),
    observations = nobs(model),
    offices = uniqueN(model_data$office_id)
  )
}

results <- rbindlist(list(
  tidy_model(m_raw, "Unadjusted", cells44),
  tidy_model(m_pos, "Position fixed effects", cells44),
  tidy_model(m_baseline_sample, "Position FE, baseline-matched sample",
             baseline_sample),
  tidy_model(m_baseline, "Position FE + mean 1938-1943 office stock",
             baseline_sample)
))

fwrite(cells44, file.path(OUT_DIR, "office_position_cells.csv"))
fwrite(results, file.path(OUT_DIR, "first_stage_results.csv"))

cat("1944 office-position cells:", nrow(cells44), "\n")
cat("Offices:", uniqueN(cells44$office_id), "\n")
cat("Cells with draftees:", sum(cells44$draftees_1944 > 0), "\n")
cat("Total draftees:", sum(cells44$draftees_1944), "\n\n")
cat("Cells directly matched to 1943 by office_id and position:",
    sum(cells44$matched_1943), "\n\n")
cat("Cells matched to the historical office distribution:",
    nrow(baseline_sample), "\n")
cat("Offices matched to the historical office distribution:",
    uniqueN(baseline_sample$office_id), "\n\n")
print(results)
