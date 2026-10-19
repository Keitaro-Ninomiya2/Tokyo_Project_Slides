The 1944-only specification estimates conditional responses per additional male draftee,
using observed non-draftee outcome cells, section and occupation fixed effects,
the log cumulative-male control, and standard errors clustered by office.

| Outcome | 1944 coefficient | Clustered SE |
|---|---:|---:|
| Total internal | 0.193178 | 0.055903 |
| Retention | 0.071471 | 0.035612 |
| Same dept. | 0.058506 | 0.025746 |
| Diff. dept. | 0.063201 | 0.012070 |
| External hires | 0.096683 | 0.033697 |

Internal/external ratio: 1.998060:1. The internal components sum to the total within 1e-8. The common regression sample has 623 cell observations and 161 offices.

The reproduced pooled benchmark is 0.4079 internal and 0.2057 external
(ratio 1.983:1); the post-merger-entrant exposure benchmark is 0.4651 and
0.3354 (ratio about 1.39:1). These specifications use the 1938–1945 window,
with actual directory observations in 1938–1944. They are distinct from the
1944-only result.

These are associations. Pre-draft differences and changing observed cell
support prevent a causal interpretation as workers hired because of the draft.
Retention is part of total internal response and is not a transfer. The two
transfer components alone sum to 0.121706 workers per draftee. Cells
without non-draftee outcome records are outside the 1944 sample.

The matched-cell 1944 cumulative baseline now uses historical men observed
strictly before 1944. The old join made the control zero in every 1944 row.
Both matched and office baselines are independently checked against the raw
master in validate_draft1944_baseline.py. The five 1944 coefficients are also
checked using independent dummy-variable OLS in stats::lm.

All main draft-margin tables are generated from model objects. Restricted
plots retain every estimable confidence interval. The regenerated department
robustness uses full office/department/section/team/occupation draft cells and
section-specific team identifiers (including missing-team groups), with section
clusters; it is separate from the main office-team-occupation exposure measure.

Input provenance: Illinois Box, `Research Notes (keitaro2@illinois.edu)/Tokyo_Gender/Processed_Data/Tokyo_Personnel_Master_All_Years.csv`,
downloaded 2026-10-19. Box file size: 117652703 bytes; SHA-1:
`47f1f755383115ae7353e20ae08eedbdaa15991e` (verified against the download).
Figures repository commit: `6ebfbd4`. Input MD5 hashes and package versions
are recorded in `input_manifest.csv` and `R_session_info.txt` in the output folder.
