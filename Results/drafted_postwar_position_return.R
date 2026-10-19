################################################################################
# Postwar Position Return of Drafted Workers
#
# Question: Among drafted workers who returned to government employment
# after the war, did they return to the SAME POSITION they left in 1944?
#
# Approach:
#  1. Take the 782 draftees who returned postwar (from prior analysis)
#  2. Record their 1944 position: office_id, kyoku, ka, kakari, pos_norm
#  3. Record their FIRST postwar position (earliest year in 1947-1955)
#  4. Test match at each level of the hierarchy:
#       - Same office (office_id)
#       - Same section (kyoku x ka)
#       - Same bureau (kyoku)
#       - Same position title (pos_norm)
#       - Same kakari (team)
#  5. Regression: does returning to same position predict postwar tenure/rank?
#
# Output: CSV summaries + LaTeX slides
################################################################################

suppressMessages({
  library(tidyverse)
  library(fixest)
  library(ggplot2)
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
df <- read_csv(DATA_PATH, locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE) %>%
  filter(is_name == TRUE) %>%
  mutate(
    year_num  = as.numeric(year),
    is_female = gender_modern == "female",
    pos_norm  = str_replace_all(position, "\\s+", ""),
    drafted   = drafted %in% TRUE
  ) %>%
  distinct(staff_id, year_num, .keep_all = TRUE)

cat("Year range:", min(df$year_num), "-", max(df$year_num), "\n\n")

postwar_years <- 1947:1955

assign_rank <- function(pos) {
  case_when(
    str_detect(pos, "\u4e3b\u4e8b|\u6280\u5e2b") ~ 3L,  # 主事 / 技師
    str_detect(pos, "\u96c7|\u56d1\u8a17")        ~ 1L,  # 雇 / 囑託
    TRUE                                           ~ 2L
  )
}

df <- df %>%
  mutate(pos_rank = assign_rank(pos_norm))

# ── 1. Pre-draft position (1944) for male draftees ───────────────────────────

pre_draft <- df %>%
  filter(year_num == 1944, drafted, !is_female) %>%
  select(
    staff_id,
    pre_office_id = office_id,
    pre_kyoku     = kyoku,
    pre_ka        = ka,
    pre_kakari    = kakari,
    pre_pos       = pos_norm,
    pre_pos_rank  = pos_rank,
    pre_salary    = salary
  )

cat("Male draftees in 1944:", nrow(pre_draft), "\n")

# ── 2. First postwar position for returnees ───────────────────────────────────

first_postwar <- df %>%
  filter(year_num %in% postwar_years, !is_female) %>%
  semi_join(pre_draft, by = "staff_id") %>%       # only draftees who returned
  group_by(staff_id) %>%
  slice_min(year_num, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    staff_id,
    post_year      = year_num,
    post_office_id = office_id,
    post_kyoku     = kyoku,
    post_ka        = ka,
    post_kakari    = kakari,
    post_pos       = pos_norm,
    post_pos_rank  = pos_rank,
    post_salary    = salary
  )

cat("Draftees with a postwar observation:", nrow(first_postwar), "\n\n")

# ── 3. Merge and compute positional match ─────────────────────────────────────

returnees <- pre_draft %>%
  inner_join(first_postwar, by = "staff_id") %>%
  mutate(
    # Hierarchical match indicators (NA if either side is NA)
    same_office = case_when(
      is.na(pre_office_id) | is.na(post_office_id) ~ NA,
      TRUE ~ (pre_office_id == post_office_id)
    ),
    same_kyoku = case_when(
      is.na(pre_kyoku) | is.na(post_kyoku) ~ NA,
      TRUE ~ (pre_kyoku == post_kyoku)
    ),
    same_ka = case_when(
      is.na(pre_ka) | is.na(post_ka) ~ NA,
      TRUE ~ (pre_ka == post_ka & pre_kyoku == post_kyoku)
    ),
    same_kakari = case_when(
      is.na(pre_kakari) | is.na(post_kakari) ~ NA,
      TRUE ~ (pre_kakari == post_kakari & pre_kyoku == post_kyoku)
    ),
    same_pos = case_when(
      is.na(pre_pos) | is.na(post_pos) ~ NA,
      TRUE ~ (pre_pos == post_pos)
    ),
    same_office_and_pos = case_when(
      is.na(same_office) | is.na(same_pos) ~ NA,
      TRUE ~ (same_office & same_pos)
    ),
    years_until_return = post_year - 1944,
    rank_change = post_pos_rank - pre_pos_rank
  )

cat("Returnees matched to pre-draft position:", nrow(returnees), "\n\n")

# ── 4. Descriptive summary by hierarchy level ─────────────────────────────────

match_vars <- c(
  "Same bureau (kyoku)"          = "same_kyoku",
  "Same section (kyoku × ka)"    = "same_ka",
  "Same team (kyoku × kakari)"   = "same_kakari",
  "Same position title"          = "same_pos",
  "Same office (office_id)"      = "same_office",
  "Same office & position title" = "same_office_and_pos"
)

match_summary <- imap_dfr(match_vars, function(var, label) {
  vals <- returnees[[var]]
  tibble(
    level      = label,
    n_nonmiss  = sum(!is.na(vals)),
    n_match    = sum(vals, na.rm = TRUE),
    match_rate = mean(vals, na.rm = TRUE)
  )
})

cat("Positional match rates among returnees:\n")
print(match_summary)
cat("\n")

write_csv(match_summary, file.path(OUT_DIR, "positional_match_summary.csv"))
write_csv(returnees,     file.path(OUT_DIR, "returnees_with_positions.csv"))

# ── 5. Return timing by match type ───────────────────────────────────────────

timing_summary <- returnees %>%
  group_by(post_year) %>%
  summarise(
    n = n(),
    pct_same_kyoku  = mean(same_kyoku,  na.rm = TRUE),
    pct_same_ka     = mean(same_ka,     na.rm = TRUE),
    pct_same_office = mean(same_office, na.rm = TRUE),
    pct_same_pos    = mean(same_pos,    na.rm = TRUE),
    .groups = "drop"
  )

cat("Match rates by return year:\n")
print(timing_summary)
cat("\n")

write_csv(timing_summary, file.path(OUT_DIR, "positional_match_by_year.csv"))

# ── 6. Plot: match rates by hierarchy level ───────────────────────────────────

plot_data <- match_summary %>%
  mutate(
    level = factor(level, levels = rev(c(
      "Same bureau (kyoku)",
      "Same section (kyoku × ka)",
      "Same team (kyoku × kakari)",
      "Same office (office_id)",
      "Same position title",
      "Same office & position title"
    ))),
    pct = match_rate * 100
  )

p_match <- ggplot(plot_data, aes(x = level, y = pct)) +
  geom_col(fill = "#1f5a85", alpha = 0.85, width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Share of returnees matching pre-draft position",
       caption = paste0("Among ", scales::comma(nrow(returnees)),
                        " drafted workers who returned to government employment (1947-1955).")) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(size = 6, color = "gray50")
  )

ggsave(file.path(OUT_DIR, "positional_match_rates.pdf"),
       p_match, width = 6.5, height = 3.2, units = "in")

# ── 7. Regression: does returning to same bureau predict postwar outcomes? ─────
# Given near-zero match rates at office/ka/pos level, same_kyoku is the only
# match variable with enough variation to run regressions on.

# Postwar tenure = number of years observed in postwar
postwar_tenure <- df %>%
  filter(year_num %in% postwar_years, !is_female) %>%
  semi_join(pre_draft, by = "staff_id") %>%
  group_by(staff_id) %>%
  summarise(postwar_yrs = n_distinct(year_num), .groups = "drop")

returnees2 <- returnees %>%
  left_join(postwar_tenure, by = "staff_id") %>%
  mutate(
    pre_ka_id = if_else(!is.na(pre_kyoku) & !is.na(pre_ka),
                        paste(pre_kyoku, pre_ka, sep = "_"), NA_character_)
  )

reg_data <- returnees2 %>%
  filter(!is.na(same_kyoku), !is.na(pre_pos_rank), !is.na(pre_kyoku))

# --- Postwar tenure ----
m_tenure1 <- feols(
  postwar_yrs ~ same_kyoku + years_until_return + pre_pos_rank,
  data = reg_data,
  cluster = ~pre_office_id
)

m_tenure2 <- feols(
  postwar_yrs ~ same_kyoku + years_until_return + pre_pos_rank | pre_kyoku,
  data = reg_data,
  cluster = ~pre_office_id
)

cat("=== Postwar tenure ~ same bureau ===\n")
etable(m_tenure1, m_tenure2,
       dict = c(same_kyokuTRUE = "Same bureau (kyoku)",
                years_until_return = "Years until return",
                pre_pos_rank = "Pre-draft rank"),
       headers = c("No FE", "Bureau FE"),
       se.below = TRUE, fitstat = ~n + r2)

# (Rank change regression omitted: assign_rank yields rank 2 for most
#  postwar positions due to broad default, making rank_change mechanically
#  determined by pre_pos_rank.)

# ── 8. Stars / formatting helpers ─────────────────────────────────────────────

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "$^{***}$",
    p < 0.05 ~ "$^{**}$",
    p < 0.1  ~ "$^{*}$",
    TRUE ~ ""
  )
}

fmt_coef <- function(b, p) paste0(sprintf("%.3f", b), stars(p))
fmt_se   <- function(s)    sprintf("(%.3f)", s)
fmt_pct  <- function(x)    sprintf("%.1f\\%%", 100 * x)

get_stat <- function(m, v) {
  list(b = unname(coef(m)[v]),
       s = unname(se(m)[v]),
       p = unname(pvalue(m)[v]))
}

# ── 9. Write LaTeX slides ─────────────────────────────────────────────────────

n_ret <- nrow(returnees)

# Match rates for annotation
mr_bureau  <- match_summary$match_rate[match_summary$level == "Same bureau (kyoku)"]
mr_section <- match_summary$match_rate[match_summary$level == "Same section (kyoku \u00d7 ka)"]
mr_office  <- match_summary$match_rate[match_summary$level == "Same office (office_id)"]
mr_pos     <- match_summary$match_rate[match_summary$level == "Same position title"]

# --- Slide 1: match rate bar chart ---
tex_plot <- c(
  "\\begin{frame}[t]{Returnees: did they come back to the same position?}",
  "    \\hypertarget{app:postwar-position-return}{}",
  "    \\centering",
  "    \\includegraphics[width=0.84\\textwidth]{Results/drafted_postwar_return/positional_match_rates.pdf}",
  "    \\vspace{1mm}",
  paste0("    \\parbox{0.9\\textwidth}{\\centering\\tiny ",
         "Among the ", scales::comma(n_ret), " drafted workers who returned to Tokyo metropolitan ",
         "government employment (1947--1955), the figure shows what share returned to the same ",
         "organizational unit at each level of the hierarchy. ",
         "Only ", fmt_pct(mr_bureau), " returned to the same bureau; ",
         "virtually none (", fmt_pct(mr_office), ") returned to the same office. ",
         "The 1943 Tokyo Fu/Shi merger and postwar reorganization largely dissolved the original structure.}"),
  "\\end{frame}",
  ""
)

# --- Slide 2: regression: same-bureau return and postwar outcomes ---
st1 <- get_stat(m_tenure1, "same_kyokuTRUE")
st2 <- get_stat(m_tenure2, "same_kyokuTRUE")
n_t1 <- nobs(m_tenure1); n_t2 <- nobs(m_tenure2)

tex_reg <- c(
  "\\begin{frame}[t]{Same-bureau return and postwar tenure}",
  "    \\scriptsize",
  "    \\centering",
  "    \\begin{threeparttable}",
  "    \\begin{tabular}{lcc}",
  "        \\toprule",
  "        & \\multicolumn{2}{c}{Postwar years observed} \\\\",
  "        \\cmidrule(lr){2-3}",
  "        & (1) No FE & (2) Bureau FE \\\\",
  "        \\midrule",
  paste0("        Same bureau (kyoku) & ",
         fmt_coef(st1$b, st1$p), " & ",
         fmt_coef(st2$b, st2$p), " \\\\"),
  paste0("         & ",
         fmt_se(st1$s), " & ",
         fmt_se(st2$s), " \\\\"),
  "        \\addlinespace",
  paste0("        Bureau match rate & \\multicolumn{2}{c}{",
         fmt_pct(mr_bureau),
         " (N\\,=\\,",
         scales::comma(match_summary$n_nonmiss[match_summary$level == "Same bureau (kyoku)"]),
         " with non-missing kyoku)} \\\\"),
  "        \\midrule",
  paste0("        Observations & ",
         scales::comma(n_t1), " & ", scales::comma(n_t2), " \\\\"),
  "        Bureau FE (pre-draft kyoku) & No & Yes \\\\",
  "        Pre-draft rank + years until return & Yes & Yes \\\\",
  "        \\bottomrule",
  "    \\end{tabular}",
  "    \\begin{tablenotes}[flushleft]",
  "        \\tiny",
  paste0(
    "        \\item \\textit{Notes:} OLS regressions. Sample: male 1944 draftees who returned ",
    "to Tokyo metropolitan government employment in 1947--1955 (N\\,=\\,", scales::comma(n_ret), "). ",
    "``Same bureau'' is an indicator equal to 1 if the worker's first postwar kyoku matches their 1944 kyoku. ",
    "Match rates at finer levels (section, office, position title) are near zero (0.5--1.9\\%) due to the ",
    "1943 Fu/Shi merger and postwar reorganization. ",
    "``Postwar yrs'' counts postwar directories (1947--1955) in which the worker appears. ",
    "Controls include years between 1944 and first postwar appearance and pre-draft position rank. ",
    "Standard errors clustered by pre-draft office. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.1$."
  ),
  "    \\end{tablenotes}",
  "    \\end{threeparttable}",
  "\\end{frame}",
  ""
)

tex <- c(tex_plot, tex_reg)

writeLines(tex, file.path("Results", "DraftedPostwarPositionReturnAppendix.tex"),
           useBytes = TRUE)

cat("\nWrote:\n")
cat(" -", file.path("Results", "DraftedPostwarPositionReturnAppendix.tex"), "\n")
cat(" -", file.path(OUT_DIR, "positional_match_summary.csv"), "\n")
cat(" -", file.path(OUT_DIR, "positional_match_by_year.csv"), "\n")
cat(" -", file.path(OUT_DIR, "positional_match_rates.pdf"), "\n")
