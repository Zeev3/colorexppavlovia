# EPoC analysis, October 2026

The current analysis of Experiments 1–4 (colour arrays, slider reports), meant to be close to the analysis that will be reported. It replaces the exploratory runs in `../analysis/` for the main results, and re-uses that pipeline's cleaning functions.

**Main rule: every main analysis uses the experience instruction only.** No pooling with the average instruction, no between-subjects factor. The average instruction on its own, and every comparison between the instructions, are in `R/appendix/`.

## How to run

```
cd analysis_october_26
Rscript run_all.R          # everything, from the raw data (about 13 minutes)
Rscript run_all.R 10       # from step 10 on, re-using the cleaned data already in output/
```

Each step writes its console output to `output/logs/<script>.log`. Results go to `output/`, figures to `output/figures/`.

For one model by hand (linked slopes vs maximal, any experiment / instruction / direction), open `R/checks/compare_linked_vs_maximal_interactive.R` in RStudio, set the three lines at the top, and run it (needs `output/data/` from steps 01–03).

## The data, and the two corrections

Raw data and cleaning functions come from `../analysis/` (the pipeline). Step 01 applies two corrections before anything else:

1. **True colour = the average of the squares actually shown** (read from the raw logs), not the logged target. In Exps 1–3 the arrays were fixed templates whose offsets did not average to zero.
2. **Report + 1.** The slider showed shade i + 1 but stored i.

Cleaning (unchanged from the pipeline): second sessions, unmoved-slider clicks, RT < 0.2 s or > 8 s, RT beyond ±2.5 SD of the person's own RTs, reports ≥ 30 shades off; then people with < 85% of trials left, or Spearman ρ ≤ .30 between report and true colour.

Clean data: `output/data/<exp>_clean_conservative.csv` with `participant_id, condition, noise_level, trial, meanVal (true colour), indexSelected (report)`. Noise level = number of squares (Exps 1–3) or SD of the shades (Exp 4).

## Steps

| Step | Script | What it does | Main outputs |
|---|---|---|---|
| 01 | `R/01_clean_conservative.R` | Cleaning with both corrections; model ladder (5 random-effects structures) per experiment × instruction × direction | `exp*_conservative.rds`, `exp*_models.csv` |
| 02 | `R/02_per_person_cells.R` | Per-person numbers (both instructions) and a text summary of the pipeline tests | `cells_all.rds`, `extract.txt` |
| 03 | `R/03_export_clean_data.R` | Clean trials to CSV | `data/*.csv` |
| 04 | `R/04_linked_models.R` | Linked-slopes model, forward and reverse, every experiment × instruction; zero-variance terms dropped | `linked_models.csv`, `linked_varcomp.csv` |
| **10** | `R/main/10_experience_only.R` | **Main analyses, experience only**: per-person DVs (forward and reverse slope, r, SD of reports, SD of true colours, bias, share of extreme reports, previous-trial weight), repeated-measures ANOVAs with Holm post hocs and linear trends; lowest level vs the rest; linked models with pairwise and trend tests; report windows | `experience_only_results.csv`, `experience_only_cells.csv` |
| **11** | `R/main/11_slope_vs_one.R` | Is the slope different from 1 at each level? (per person and model, Holm) | `slope_vs_one.csv` |
| **12** | `R/main/12_same_report_by_instruction.R` | Same-report test (the main exaggeration test), within each instruction | `same_report_tests_by_instruction.csv`, `same_report_values_by_instruction.csv` |
| **13** | `R/main/13_levels_and_serial_dependence.R` | Exp 4: direct level comparisons, linear and quadratic trends, serial dependence by level, where the regression lines cross | `logs/13_levels_and_serial_dependence.log` |
| 14 | `R/main/14_r_vs_one.R` | Tracking r against 1 at each level (per person, Holm); near-guaranteed, see caveat in the script | `r_vs_one.csv` |
| 20 | `R/checks/20_maximal_model_diagnostics.R` | Maximal model: singularity, convergence, SDs, correlations | `max_diag.csv`, `max_sds.csv` |
| 21 | `R/checks/21_slope_correlation_lrt.R` | Are a person's slopes linked across levels? Correlations and linked-vs-unlinked likelihood-ratio tests | `slope_cor_pairs.csv`, `slope_cor_lrt.csv` |
| 22–23 | `R/checks/22_value0_arrays.R`, `23_value0_sensitivity.R` | Exp 4 arrays with a value-0 square (drawn as shade 50): list, and main tests with them dropped / corrected | `exp4_zero_arrays.csv`, `exp4_zero_sensitivity.csv` |
| 30–34 | `R/appendix/` | Average instruction and comparisons: pooled same-report test, post hocs for both instructions, lowest vs rest for both, Exp 4 mixed ANOVA with instruction as a factor, all numbers for both instructions | `same_report_tests.csv`, `posthocs_all.csv`, `contrast_low_vs_rest.csv`, `logs/33_*.log`, `full_numbers.json` |
| 40 | `R/figures/40_figures_experience.R` | Experience-only figures for the slides (editable charts) | `figures/figures_experience.pptx` |
| 41 | `R/figures/41_tracking_figures.R` | Tracking as regression lines, every experiment: z-score version (slope = r) and both regression lines in the original shades | `figures/trackz_<exp>.png`, `figures/twolines_<exp>.png` |
| 44 | `R/figures/44_tracking_ellipse_band_drafts.R` | Drafts, Exp 4: tracking as data ellipses, and as regression lines with a ±1 residual SD band | `figures/draft_tracking_*_exp4.png` |
| 45 | `R/figures/45_galton_ellipses.R` | Galton-style figure per level: trial cloud, 68% data ellipse, forward and reverse regression lines and their tangent points | `figures/galton_ellipse_<exp>.png`, one PNG per level |
| 46 | `R/figures/46_slopes_four_ways.R` | Forward and reverse slope per level computed four ways: linked model, mean of per-person slopes, pooled trials with people centred, pooled trials raw | `figures/slopes_four_ways.csv`, `figures/slopes_four_ways_<exp>.png` |
| 43 | `R/figures/43_tracking_lines.R` | Tracking as fitted lines, styled like the model line graphs: forward and backward, all levels in one plot, z units (slope = r), linked model on the z data | `figures/tracking_lines.pptx` (editable), `figures/tracking_<direction>_<exp>.png`, `figures/tracking_lines_slopes.csv` |
| 42 | `R/figures/42_raw_data_views.R` | Raw data, every experiment: A = trial density + average report per true colour; B = error per true colour by level | `figures/rawA_<exp>.png`, `figures/rawB_<exp>.png` |

## Notes

- `notes/analysis_guide.md`: every analysis in plain words: what the DV is and how it is calculated, the IV, post hocs, how to read the model, and the results.
- `notes/analysis_decisions_and_checks.md`: the reasoning behind each decision (templates, the linked model and its sources, coding problems, the invalid "shift removed" test, maximal vs linked, slope correlations, the same-report test).
- Slides built from these results: `2facolorexp/experiments_summary/meeting_ran/` (deck build scripts in `analysis_conservative/deck_build/`).

## Known open items

- Trial position as a covariate (biased shuffle in all task repos).
- Analysis without the ρ ≤ .30 exclusion.
- Residual-variance check (varIdent) as a script, and for Exps 1–3.
- Sensitivity of the same-report test to its minimum of 5 trials per report value.
