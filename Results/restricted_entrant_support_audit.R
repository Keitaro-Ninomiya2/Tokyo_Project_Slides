suppressMessages({
  library(tidyverse)
})

source("Results/draft1944_pre_draft_trajectories.R")

rc <- post_merger_entrant_draftees_1944 %>%
  mutate(
    office_id = as.numeric(office_id),
    dept_id = na_if(as.character(kyoku), ""),
    section_id = na_if(as.character(ka), ""),
    kakari = as.character(kakari),
    pos_norm = as.character(pos_norm)
  ) %>%
  filter(!is.na(kakari), kakari != "", kakari != "NA", !is.na(pos_norm), pos_norm != "")

offices <- rc %>%
  filter(!is.na(office_id)) %>%
  distinct(office_id)

cat("restricted rows", nrow(rc), "workers", n_distinct(rc$staff_id),
    "offices", n_distinct(rc$office_id), "\n")

print(
  rc %>%
    summarise(
      nonmissing_dept = sum(!is.na(dept_id)),
      nonmissing_section = sum(!is.na(section_id)),
      complete_dept_team_occ = sum(!is.na(dept_id) & !is.na(kakari) & !is.na(pos_norm)),
      complete_section_team_occ = sum(!is.na(section_id) & !is.na(kakari) & !is.na(pos_norm))
    )
)

keys <- list(
  office_team_occ = c("office_id", "kakari", "pos_norm"),
  section_team_occ = c("ka", "kakari", "pos_norm"),
  dept_team_occ = c("kyoku", "kakari", "pos_norm"),
  team_occ = c("kakari", "pos_norm")
)

for (nm in names(keys)) {
  k <- keys[[nm]]
  exp <- post_merger_entrant_draftees_1944 %>%
    mutate(office_id = as.numeric(office_id)) %>%
    filter(if_all(all_of(k), ~!is.na(.) & as.character(.) != "" & as.character(.) != "NA")) %>%
    distinct(across(all_of(k)))

  bp <- pre_panel %>%
    mutate(office_id = as.numeric(office_id)) %>%
    semi_join(offices, by = "office_id")

  matched <- bp %>%
    semi_join(exp, by = k)

  cat(
    nm,
    "exposure keys", nrow(exp),
    "matched baseline rows", nrow(matched),
    "years", paste(range(matched$year_num, na.rm = TRUE), collapse = "-"),
    "matched cells", n_distinct(paste(matched$office_id, matched$kakari, matched$pos_norm)),
    "\n"
  )
}
