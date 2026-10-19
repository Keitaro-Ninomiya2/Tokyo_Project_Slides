# Configurable upstream inputs; run from the Slides repository root.
repo_override <- Sys.getenv("TOKYO_FIG_REPO")
repo_candidates <- unique(c(
  repo_override,
  file.path("..", "Tokyo_Project_Figures"),
  "/u/keitaro2/Tokyo_Project/Tokyo_Project_Figures",
  "/u/keitaro2/Tokyo_Project_Figures",
  file.path(path.expand("~"), "Documents", "GitHub", "Tokyo_Project_Figures")
))
repo_candidates <- repo_candidates[nzchar(repo_candidates)]
if (nzchar(repo_override) && !dir.exists(repo_override)) {
  stop("TOKYO_FIG_REPO does not exist: ", repo_override)
}
existing_repos <- repo_candidates[dir.exists(repo_candidates)]
if (!length(existing_repos)) {
  stop("Cannot locate Tokyo_Project_Figures; set TOKYO_FIG_REPO.")
}
FIG_REPO <- normalizePath(existing_repos[[1]], mustWork = TRUE)
resolve_input <- function(env_var, relative_path) {
  override <- Sys.getenv(env_var)
  input <- if (nzchar(override)) override else file.path(FIG_REPO, relative_path)
  if (!file.exists(input)) {
    stop("Missing required input: ", input, "\nSet ", env_var,
         " to a verified equivalent input, or transfer the missing source/data directory.")
  }
  normalizePath(input, mustWork = TRUE)
}
SOURCE_SCRIPT <- resolve_input(
  "TOKYO_REPLACEMENT_SOURCE", file.path("Regressions", "run_replacement_margins.R")
)
ENTRANT_CLASSIFICATION <- resolve_input(
  "TOKYO_ENTRANT_CLASSIFICATION",
  file.path("MainResults", "PostMergerEntrantOrganizationalAdjustment1944", "tables",
            "step1_tokyoto1944_worker_entrant_classification.csv")
)

DATA_PATH <- Sys.getenv("TOKYO_PERSONNEL_MASTER")
if (!nzchar(DATA_PATH) || !file.exists(DATA_PATH)) {
  stop("Set TOKYO_PERSONNEL_MASTER to the verified personnel master CSV. ",
       "Expected Box location: Research Notes (keitaro2@illinois.edu)/",
       "Tokyo_Gender/Processed_Data/Tokyo_Personnel_Master_All_Years.csv")
}
DATA_PATH <- normalizePath(DATA_PATH, mustWork = TRUE)
KA_GROUP_PATH <- resolve_input(
  "TOKYO_KA_GROUP_MAP", file.path("Regressions", "ka_group_map.csv")
)

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
required_columns <- c("year", "staff_id", "office_id", "is_name", "drafted",
                      "gender_modern", "position", "kyoku", "ka", "kakari")
master_columns <- names(readr::read_csv(DATA_PATH, n_max = 0, show_col_types = FALSE))
missing_columns <- setdiff(required_columns, master_columns)
if (length(missing_columns)) {
  stop("Personnel master lacks required columns: ", paste(missing_columns, collapse = ", "))
}

# The upstream script hardcodes these two Windows paths. Replace only its path
# assignments and use the explicit package imports above. Data preparation
# and estimator expressions are preserved verbatim.
upstream_expressions <- parse(SOURCE_SCRIPT, keep.source = FALSE)
expected_imports <- quote(suppressMessages({
  library(tidyverse)
  library(fixest)
  library(here)
}))
if (!identical(upstream_expressions[[1]], expected_imports)) {
  stop("Upstream imports changed; review the explicit-package adapter.")
}
upstream_expressions[[1]] <- quote(invisible(NULL))
for (path_variable in c("DATA_PATH", "KA_GROUP_PATH")) {
  matches <- which(vapply(upstream_expressions, function(expr) {
    is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name(path_variable))
  }, logical(1)))
  if (length(matches) != 1L) {
    stop("Expected exactly one upstream assignment for ", path_variable)
  }
  upstream_expressions[[matches]][[3]] <- get(path_variable)
}
message("Personnel master: ", DATA_PATH)
message("Replacement source: ", SOURCE_SCRIPT)
eval(upstream_expressions, envir = environment())
