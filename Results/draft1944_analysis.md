Run from the Tokyo_Project_Slides repository root with a verified personnel master:

```bash
Rscript -e 'install.packages(c("dplyr", "tidyr", "readr", "purrr", "stringr", "tibble", "ggplot2", "fixest", "broom"), repos="https://cloud.r-project.org")'
export TOKYO_FIG_REPO=/u/keitaro2/Tokyo_Project/Tokyo_Project_Figures
export TOKYO_PERSONNEL_MASTER=/absolute/path/to/Tokyo_Personnel_Master_All_Years.csv
Rscript Results/run_draft1944_analysis.R
Rscript Results/validate_draft1944_analysis.R
python Results/validate_draft1944_baseline.py
./build_main.sh
```

The personnel master is expected in Illinois Box under `Research Notes
(keitaro2@illinois.edu)/Tokyo_Gender/Processed_Data/`. The trajectory analysis
requires every year from 1930 through 1943; older local masters beginning in
1937 cannot reproduce it. Do not substitute those files for the complete master.

`Results/load_replacement_margins.R` discovers the figures repository or honors
`TOKYO_FIG_REPO`. It requires `Regressions/run_replacement_margins.R`,
`Regressions/ka_group_map.csv`, and
`MainResults/PostMergerEntrantOrganizationalAdjustment1944/tables/step1_tokyoto1944_worker_entrant_classification.csv`.
Explicit overrides are `TOKYO_REPLACEMENT_SOURCE`, `TOKYO_KA_GROUP_MAP`, and
`TOKYO_ENTRANT_CLASSIFICATION`. The loader evaluates the upstream source with
its two Windows path assignments replaced by the verified local paths and its
package bundle replaced by explicit imports; its preparation and regression
expressions are otherwise unchanged.

The cumulative-male table used by the matched-cell analysis includes 1944,
computed from observations strictly before 1944. Previously this table stopped
in 1943, so its join to 1944 outcomes incorrectly replaced every baseline with
zero. Regenerate results before interpreting any affected 1944 coefficients.

The deck now distinguishes a 1944-only specification from the reproduced
1938–1945 benchmark. The benchmark pools 1938–1945. Its coefficient
ratio must not be described as a standalone 1944 estimate. Total internal
response includes retention plus same-department and different-department
transfers; it is not a measure of transfers alone.

The independent Python baseline check uses only the standard library. The build
requires pdflatex and the packages in main.tex; biber is needed if citations are
added. The currently uncited bibliography requires no biber pass.
