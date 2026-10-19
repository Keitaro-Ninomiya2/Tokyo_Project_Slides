################################################################################
# Postwar Return of Drafted Workers
#
# Question: Did workers who were drafted in 1944 return to government
# employment in the postwar years (1947+)?
#
# Approach:
#  1. Identify workers drafted in 1944 (drafted == TRUE, male, is_name == TRUE)
#  2. Check whether they reappear in postwar directories (1947-1955)
#  3. Compare return rate to a control group: non-drafted male workers who
#     also exit in 1944 (left the dataset between 1943 and 1944 or in 1944)
#  4. OLS regression: postwar_return ~ drafted + controls
#
# Output: CSV of results + LaTeX snippet for a beamer slide
################################################################################

suppressMessages({
  library(tidyverse)
  library(fixest)
})

DATA_PATH <- file.path(
  Sys.getenv("USERPROFILE"),
  "Box", "Research Notes (keitaro2@illinois.edu)",
  "Tokyo_Gender", "Processed_Data",
  "Tokyo_Personnel_Master_All_Years.csv"
)

OUT_DIR <- file.path("Results", "drafted_postwar_return")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("Loading data...\n")
df_raw <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"),
                   show_col_types = FALSE)

df <- df_raw %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm = str_replace_all(position, "\\s+", ""),
    drafted  = drafted %in% TRUE
  ) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

cat("Total worker-years:", nrow(df), "\n")
cat("Year range:", min(df$year_num), "-", max(df$year_num), "\n\n")

# ── 1. Drafted workers in 1944 ───────────────────────────────────────────────

drafted_1944 <- df %>%
  filter(year_num == 1944, drafted, !is_female) %>%
  distinct(staff_id, office_id, kyoku, ka, pos_norm)

cat("Male draftees in 1944:", nrow(drafted_1944), "\n")

# ── 2. Control group: non-drafted males present in 1944 who left ─────────────
# Workers observed in 1944 who are NOT drafted. We track everyone present
# in 1944, whether they stay or go.

males_1944 <- df %>%
  filter(year_num == 1944, !is_female) %>%
  distinct(staff_id, office_id, kyoku, ka, pos_norm, drafted)

cat("All male workers in 1944:", nrow(males_1944), "\n")
cat("  Drafted:", sum(males_1944$drafted), "\n")
cat("  Not drafted:", sum(!males_1944$drafted), "\n\n")

# ── 3. Postwar presence ───────────────────────────────────────────────────────

postwar_years <- 1947:1955

postwar_ids <- df %>%
  filter(year_num %in% postwar_years) %>%
  distinct(staff_id) %>%
  mutate(returned_postwar = 1L)

# ── 4. Build analysis dataset ─────────────────────────────────────────────────

# Assign rank (proxy for seniority)
assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "^主事$|^技師$") ~ 3L,
    str_detect(pos, "^雇$|^囑託$")   ~ 1L,
    TRUE                              ~ 2L
  )
}

# First and last year each worker appears in the dataset
first_year_df <- df %>%
  filter(!is_female) %>%
  group_by(staff_id) %>%
  summarise(first_year = min(year_num),
            last_year  = max(year_num),
            .groups = "drop")

analysis <- males_1944 %>%
  left_join(postwar_ids, by = "staff_id") %>%
  left_join(first_year_df, by = "staff_id") %>%
  mutate(
    returned_postwar = replace_na(returned_postwar, 0L),
    pos_rank = assign_rank(pos_norm),
    tenure_1944 = 1944 - first_year,
    ka_id = if_else(!is.na(ka) & !is.na(kyoku),
                    paste(kyoku, ka, sep = "_"), NA_character_),
    # "Exited in 1944": last seen in 1944 (so not continuously employed postwar)
    exited_1944 = as.integer(last_year == 1944)
  )

# Clean comparison: among workers whose last year is 1944, did they return?
# Drafted: forced out by conscription
# Non-drafted who also exited: left for other (voluntary/natural) reasons
analysis_exiters <- analysis %>%
  filter(exited_1944 == 1 | drafted)

cat("Workers whose last year is 1944 (exiters):\n")
cat("  Drafted:", sum(analysis_exiters$drafted), "\n")
cat("  Non-drafted exiters:", sum(!analysis_exiters$drafted), "\n\n")

# ── 5. Descriptive summary ────────────────────────────────────────────────────

# All 1944 workers
summary_stats_all <- analysis %>%
  group_by(drafted) %>%
  summarise(
    n = n(),
    n_returned = sum(returned_postwar),
    return_rate = mean(returned_postwar),
    mean_tenure = mean(tenure_1944, na.rm = TRUE),
    mean_rank = mean(pos_rank, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group = if_else(drafted, "Drafted (1944)", "Not drafted (1944)"),
         sample = "All 1944 males")

# Exiters only (cleaner comparison)
summary_stats_exit <- analysis_exiters %>%
  group_by(drafted) %>%
  summarise(
    n = n(),
    n_returned = sum(returned_postwar),
    return_rate = mean(returned_postwar),
    mean_tenure = mean(tenure_1944, na.rm = TRUE),
    mean_rank = mean(pos_rank, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group = if_else(drafted, "Drafted", "Non-drafted exiters"),
         sample = "1944 exiters only")

summary_stats <- bind_rows(summary_stats_all, summary_stats_exit)

cat("Return rates by group (all 1944 males):\n")
print(summary_stats_all %>% select(group, n, n_returned, return_rate,
                                    mean_tenure, mean_rank))
cat("\nReturn rates by group (exiters only):\n")
print(summary_stats_exit %>% select(group, n, n_returned, return_rate,
                                     mean_tenure, mean_rank))
cat("\n")

write_csv(summary_stats, file.path(OUT_DIR, "return_rate_summary.csv"))

# ── 6. Regression: postwar return ~ drafted + controls ───────────────────────

# Using exiters-only sample (cleaner comparison)
# Basic OLS
m_basic <- feols(
  returned_postwar ~ drafted + tenure_1944 + pos_rank,
  data = analysis_exiters,
  cluster = ~office_id
)

# With kyoku FE
m_fe <- feols(
  returned_postwar ~ drafted + tenure_1944 + pos_rank | kyoku,
  data = analysis_exiters %>% filter(!is.na(kyoku)),
  cluster = ~office_id
)

# With ka FE (section FE)
m_ka_fe <- feols(
  returned_postwar ~ drafted + tenure_1944 + pos_rank | ka_id,
  data = analysis_exiters %>% filter(!is.na(ka_id)),
  cluster = ~office_id
)

cat("=== OLS: Postwar return ~ drafted (exiters sample) ===\n")
etable(m_basic, m_fe, m_ka_fe,
       dict = c(drafted = "Drafted in 1944",
                tenure_1944 = "Tenure in 1944",
                pos_rank = "Position rank"),
       headers = c("Basic OLS", "Kyoku FE", "Section FE"),
       se.below = TRUE, fitstat = ~n + r2)

# ── 7. Event study: year-by-year reappearance ─────────────────────────────────

drafted_ids    <- drafted_1944 %>% pull(staff_id)
nondraft_ids   <- males_1944 %>% filter(!drafted) %>% pull(staff_id)

yearly_return <- map_dfr(c(1946:1955), function(yr) {
  yr_ids <- df %>% filter(year_num == yr) %>% pull(staff_id)
  tibble(
    year_num = yr,
    drafted_return_rate    = mean(drafted_ids %in% yr_ids),
    nondraft_return_rate   = mean(nondraft_ids %in% yr_ids),
    n_drafted_returned     = sum(drafted_ids %in% yr_ids),
    n_nondraft_returned    = sum(nondraft_ids %in% yr_ids)
  )
})

cat("\nYear-by-year return rates:\n")
print(yearly_return)
cat("\n")

write_csv(yearly_return, file.path(OUT_DIR, "yearly_return_rates.csv"))

# ── 7b. Plot: year-by-year return rate (drafted only) ────────────────────────

library(ggplot2)

plot_yearly <- yearly_return %>%
  select(year_num, drafted_return_rate) %>%
  mutate(pct = drafted_return_rate * 100)

p_yearly <- ggplot(plot_yearly, aes(x = year_num, y = pct)) +
  geom_col(fill = "#1f5a85", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = ifelse(pct > 0, sprintf("%.1f%%", pct), "")),
            vjust = -0.4, size = 2.8) +
  scale_x_continuous(breaks = 1946:1955) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Share of 1944 draftees reappearing",
       caption = "Share of 1944 drafted males observed in each postwar directory.") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 6, color = "gray50")
  )

ggsave(file.path(OUT_DIR, "drafted_yearly_return_rate.pdf"),
       p_yearly, width = 6.0, height = 3.2, units = "in")

# ── 8. Stars helper ───────────────────────────────────────────────────────────

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "$^{***}$",
    p < 0.05 ~ "$^{**}$",
    p < 0.1  ~ "$^{*}$",
    TRUE ~ ""
  )
}

fmt_coef <- function(b, p) paste0(sprintf("%.4f", b), stars(p))
fmt_se   <- function(s) sprintf("(%.4f)", s)

get_stat <- function(m, v) {
  list(
    b = unname(coef(m)[v]),
    s = unname(se(m)[v]),
    p = unname(pvalue(m)[v])
  )
}

drafted_basic <- get_stat(m_basic, "draftedTRUE")
drafted_fe    <- get_stat(m_fe,    "draftedTRUE")
drafted_ka    <- get_stat(m_ka_fe, "draftedTRUE")

# ── 9. Descriptive return rates for the slide ─────────────────────────────────

dr_row <- summary_stats %>% filter(drafted)
nd_row <- summary_stats %>% filter(!drafted)

dr_row  <- summary_stats_exit %>% filter(drafted)
nd_row  <- summary_stats_exit %>% filter(!drafted)

desc_drafted    <- sprintf("%.1f\\%%", 100 * dr_row$return_rate)
desc_nondraft   <- sprintf("%.1f\\%%", 100 * nd_row$return_rate)
desc_n_drafted  <- scales::comma(dr_row$n)
desc_n_nondraft <- scales::comma(nd_row$n)

# ── 10. Write LaTeX slide ─────────────────────────────────────────────────────

tex <- c(
  "\\begin{frame}[t]{Did drafted workers return after the war?}",
  "    \\hypertarget{app:postwar-return}{}",
  "    \\scriptsize",
  "    \\centering",
  "    \\begin{threeparttable}",
  "    \\begin{tabular}{lccc}",
  "        \\toprule",
  "        & Basic OLS & Kyoku FE & Section FE \\\\",
  "        \\midrule",
  paste0("        Drafted in 1944 & ",
         fmt_coef(drafted_basic$b, drafted_basic$p), " & ",
         fmt_coef(drafted_fe$b, drafted_fe$p), " & ",
         fmt_coef(drafted_ka$b, drafted_ka$p), " \\\\"),
  paste0("         & ",
         fmt_se(drafted_basic$s), " & ",
         fmt_se(drafted_fe$s), " & ",
         fmt_se(drafted_ka$s), " \\\\"),
  "        \\addlinespace",
  paste0("        Drafted return rate & \\multicolumn{3}{c}{", desc_drafted, "} \\\\"),
  paste0("        Non-drafted exiter return rate & \\multicolumn{3}{c}{", desc_nondraft, "} \\\\"),
  "        \\midrule",
  paste0("        Drafted workers & \\multicolumn{3}{c}{", desc_n_drafted, "} \\\\"),
  paste0("        Non-drafted exiters (1944) & \\multicolumn{3}{c}{", desc_n_nondraft, "} \\\\"),
  "        Kyoku FE & No & Yes & --- \\\\",
  "        Section FE & No & No & Yes \\\\",
  "        Controls (tenure, rank) & Yes & Yes & Yes \\\\",
  "        \\bottomrule",
  "    \\end{tabular}",
  "    \\begin{tablenotes}[flushleft]",
  "        \\tiny",
  paste0(
    "        \\item \\textit{Notes:} OLS regression. Outcome is an indicator equal to 1 if ",
    "the worker is observed in any postwar directory (1947--1955). Sample is male workers ",
    "whose last observed year is 1944 (``exiters''), so both groups left in 1944. ",
    "``Drafted'' workers are those flagged as drafted in the 1944 TokyoTo directory; ",
    "the comparison group left for other reasons. Controls include years of tenure in 1944 and position rank. ",
    "Standard errors clustered by office. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  ),
  "    \\end{tablenotes}",
  "    \\end{threeparttable}",
  "\\end{frame}",
  "",
  "\\begin{frame}[t]{When did drafted workers return?}",
  "    \\centering",
  "    \\includegraphics[width=0.84\\textwidth]{Results/drafted_postwar_return/drafted_yearly_return_rate.pdf}",
  "    \\vspace{1mm}",
  "    \\parbox{0.9\\textwidth}{\\centering\\tiny Share of 1944 drafted males (N\\,=\\,1,849) observed in each postwar Tokyo metropolitan government directory. No 1947 or 1949 directories available. The large spike in 1950 likely reflects the completion of demobilization and occupation-era restructuring.}",
  "\\end{frame}",
  ""
)

writeLines(tex, file.path("Results", "DraftedPostwarReturnAppendix.tex"),
           useBytes = TRUE)

cat("Wrote:\n")
cat(" -", file.path("Results", "DraftedPostwarReturnAppendix.tex"), "\n")
cat(" -", file.path(OUT_DIR, "return_rate_summary.csv"), "\n")
cat(" -", file.path(OUT_DIR, "yearly_return_rates.csv"), "\n")
