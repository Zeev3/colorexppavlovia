# EPoC analysis set

One pipeline for the whole project: the exploratory re-analysis of the
Goldenberg et al. (2021) open data, plus the four Ensemble Phenomenology of
Colour (EPoC) experiments described in *results round 3*.

Every experiment goes through exactly the same steps — the same cleaning
criteria, the same analyses in both directions, the same eighteen figures
(including the Galton squeeze diagrams) — so the experiments are directly comparable and nothing has
to be re-derived by hand.

```bash
Rscript analysis/run_all.R            # the four reported experiments + exploratory
Rscript analysis/run_all.R exp4       # one experiment
Rscript analysis/run_all.R all        # also the unreported extra batch and pilot
Rscript analysis/run_all.R all --shown-mean   # alternative pipeline, see below
Rscript analysis/run_all.R all --full-random  # alternative random effects, see below
Rscript analysis/run_all.R --cleaned  # use the existing data_cleaned.csv instead
                                      # of re-cleaning from the raw session logs
```

Requires R with `dplyr`, `tidyr`, `readr`, `ggplot2`, `lme4`, `lmerTest`,
`emmeans`, `afex`, `ggeffects`, `patchwork` (all already installed on this machine).
A full run takes a few minutes, most of it Experiments 2 and 4.

## Which folder is which experiment

| Write-up | id | Manipulation | Data folders (`data/`) | N analysed |
|---|---|---|---|---|
| Exploratory (Goldenberg et al., 2021, Exp. 1) | `exp0` | number of faces (1–12) | `amit_exp1.csv` | 50 |
| Experiment 1 | `exp1` | array size 2/6/8/12 | `avg_datapilot1`, `exp_datapilot1` | 23 avg / 21 exp |
| Experiment 2 | `exp2` | array size 2/10/20 | `greaternoise/colorblocksavggraternoiselevels`, `greaternoise/colorsquersexpgreaternoise` | 86 avg / 77 exp |
| Experiment 3 | `exp3` | array size 2/10/20 + Mondrian frame | `mondrian/mondrian_avg`, `mondrian/mondrian_exp` | 46 avg / 38 exp |
| Experiment 4 | `exp4` | stimulus variance SD 3/5/7 | `variancecontrol/variance_control_avg`, `variancecontrol/variance_control_exp` | 51 avg / 50 exp |
| not reported | `exp2_batch2` | **subset of Experiment 2, not independent** | `greaternoise2/*` | 57 avg / 47 exp |
| not reported | `short_exposure` | short-exposure pilot, experience only | `greater_variance` | 37 exp |

Three things worth knowing about the raw files:

- **`greaternoise2` is not a separate collection.** Every raw session file in
  it also sits in the `greaternoise` folders, all of its participant ids are
  Experiment 2 participants, and the cleaned trial rows are identical — it is a
  partial early snapshot taken mid-collection. The registry marks it with
  `subset_of = "exp2"`, and anything that pools across experiments must exclude
  it. Experiments 1, 2, 3, 4 and the short-exposure pilot share no participants
  with each other.


- **Experiment 4 stores the variance level in the `array_length` column**
  (2 → SD 3, 6 → SD 5, 8 → SD 7); the real array length is always 10 squares.
  The registry undoes this with `recode`, so the analysis variable is
  `variance_level`.
- **Four people took part in both conditions** of the same experiment, average
  first and experience later (two in Experiment 2, one each in Experiments 3
  and 4). Only their first session counts: the registry lists the later one in
  `later_session`, and cleaning drops it before anything else.
- With that and the missing-response rule below, the analysed Ns differ from
  the write-up in four places: Experiment 2 experience 77 (write-up 80),
  Experiment 3 experience 38 (39), Experiment 4 average 51 (50) and
  Experiment 4 experience 50 (51). `output/<exp>/participants.csv` lists every
  exclusion with its reason.
- **A report of 0 is a missing response, not an answer.** The response scale
  starts at index 0, and a click without ever crossing the scale saves that
  default (`sad_sequential_task_functions.js`). Zeros come from mid-range
  stimuli (mean objective value 22-27 in every experiment), are far more common
  than reports of 1, and cluster in a few participants, so cleaning drops them.
  Because every objective mean is below 37, a default 0 is never 30 units off
  and the large-error rule alone did not catch them.

- **The logged true mean is not always the mean of the squares shown.**
  Main-task arrays in Experiments 1-3 are the logged `meanVal` plus a fixed
  offset template, and three templates do not average to zero: 12 squares in
  Experiment 1 (+0.33), 10 squares (+0.20) and 20 squares (-1.50) in
  Experiments 2 and 3 (and the pilot and snapshot). Every trial at those levels
  is off by exactly that constant; the other levels match exactly. In
  Experiment 4 the logged value is the target integer and the displayed mean
  differs by at most 0.3 at random. Goldenberg's `meanGroup` matches the faces
  shown. Slopes and correlations are unaffected (a constant offset), but
  anything comparing a report with the true value at those levels is: signed
  error, exaggeration counts, the fixed-cutoff and report-window tail tests.
  `run_all.R --shown-mean` reruns everything with the displayed mean as the
  true value, into `analysis/output_shown_mean/`; the main output is untouched.
  The task code in the repository (`sad_arrays.html`) does not contain these
  templates, so it is not the version participants ran.

- **Random effects in the default pipeline are often minimal.** The registry
  starts the random-effects ladder at `mean_slope` (Experiments 1 and 3) or
  `noise_slope` (Experiment 2), so the maximal structure is never tried there,
  and in Experiment 2 no model has a by-participant slope on the objective mean
  (the reverse models of Experiments 1 and 3 lose it too). No default model
  lets each participant's slope differ across noise levels, which is the random
  effect behind the key test, so those interaction tests use trial-level
  degrees of freedom. The two-stage ANOVAs (`15_anova.R`) are the check with
  the right error term. `run_all.R --full-random` refits every mixed model
  starting from the maximal structure, through per-participant slopes at each
  noise level (correlated, then uncorrelated), never dropping below a
  by-participant slope, into `analysis/output_full_random/` (combinable with
  `--shown-mean`). Its reverse models (objective ~ report) drop the
  by-participant intercept and level shifts: the outcome is the objective value,
  whose participant means are identical by design (SD ~0.1), so those variances
  are zero and made every structure singular.

## What the pipeline does

**Cleaning** (`R/02_clean.R`), identical for every experiment: drop the later session of anyone who
took part in both conditions, then incomplete sessions, then trials with a report of 0 (missing responses), then trials with RT < 200 ms or > 8000 ms, then trials beyond ±2.5 SD
of the participant's own RT, then trials where |report − objective mean| ≥ 30
scale units; then drop participants who kept < 85% of their trials or whose
Spearman correlation with the stimulus was ≤ .30. `output/<exp>/exclusions.csv`
gives the participant flow, `participants.csv` the per-participant detail.

**Analysis 1 — tracking accuracy** (`R/04_correlations.R`): within-participant
correlation between objective mean and report at each noise level,
Fisher-z transformed, mixed ANOVA (noise within, condition between) with
Tukey-adjusted follow-ups.

**Analysis 2 — regression to the mean** (`R/05_lmm.R`):

```
report ~ objective_mean_c * noise * condition + (random | participant)
```

with the objective mean centred within participant. The slope of the objective
mean is the effect of interest: 1 means the report tracks the stimulus
one-to-one, below 1 means the reports are compressed towards the participant's
own mean. Slopes and all contrasts come from `emmeans::emtrends`, so they do
not depend on the contrast coding. Random effects follow a Barr-style ladder —
start at the rung named in the registry, walk down until a model converges
without a singular fit (`rand_force` pins a rung where the write-up
deliberately keeps a singular but theoretically motivated structure, as in
Experiment 4).

**Analysis 2b — the within-condition models** (`R/11_condition_models.R`), the
models the write-up leads with:

```
report ~ objective_mean_c * noise + (random | participant)
```

fitted **separately within each condition**, experience first. This is what
answers "within the experience condition on its own, does the slope shrink as
noise increases?" — the combined model above answers the different question of
whether the two conditions differ. Same random-effects ladder, slopes and
simple-slope comparisons per noise level, plus the overall slope collapsing
noise levels (the "responses tracked the objective mean, b = ..." sentence).

**Analysis 2c — the reverse direction** (`R/12_reverse_models.R`):

```
objective_mean ~ report_c * noise (* condition) + (random | participant)
```

Galton's own direction — given what someone reported, what was actually out
there — fitted both combined and separately per condition. This is the
direction the Goldenberg exploratory model uses (`meanGroup ~ rating *
numberFaces`), so the colour experiments are now on the same footing, and it is
the model behind the reverse squeeze diagrams. Regression to the mean shows up
as a slope below 1 in *both* directions; reporting the pair is what separates
genuine compression from a scaling artefact in either variable. In practice the
reverse slopes sit near 0.6 while the forward slopes sit near 0.9 — their
product is roughly r², exactly the double squeeze.

**Galton squeeze diagrams** (`R/10_galton.R`), ported from
`funcs/galtonsqueezandgraphvariance.R` and `funcs/galtonsqueezeforarraylength.R`
(the same script twice with a different noise variable) into one set of
functions driven by the registry. The forward squeeze puts the objective mean
on the left axis and the mean response on the right: lines converging towards
the middle are regression to the mean. The reverse squeeze runs the other
direction — given a response, what was really out there — which is the version
Galton drew. Both come with a bucketed scatter version (bins of 3 scale units)
that is easier to read when a panel gets crowded.

**Analysis 3 — exaggeration** and the tails (`R/06_exaggeration.R`,
`R/13_tails.R`). Exaggeration is the proportion of responses more extreme than
the stimulus (< 14 when the objective mean is < 20, > 36 when it is > 30), per
participant and noise level, in the same mixed ANOVA. `13_tails.R` then asks
what lies *behind* those extreme reports, two ways:

- **fixed cutoff** — for trials whose report crosses the exaggeration
  thresholds, how extreme was the stimulus? This conditions on an absolute
  response value, so it moves with any overall compression: as reports
  contract, a fixed cutoff sits further out in the tail and picks up less
  extreme stimuli. `tails_fixed_window.csv` reports the spread between the two
  tails, the range of reality an extreme report brackets.
- **matched tails** — the most extreme 10% of responses per side *within each
  cell*, so the selection rate is identical across noise levels. The ratio
  |objective deviation| / |response deviation| says how much real signal backs
  a unit of reported extremity.
- **symmetric report windows** — trials where the report landed within ±2 of 10
  or of 40, and the objective mean behind each. This is the question the reverse
  slope asks, stated directly and tested where the effect actually lives: when
  someone reported 40, what was out there, and does that change with noise? The
  global reverse slope dilutes it by averaging over the middle of the range.
  `tails_windows_width.csv` reports how much reality a 30-unit span of reports
  brackets at each noise level. Windows with fewer than 20 trials in any cell are
  skipped and flagged in `results.md` and `tails_windows_coverage.csv`.

The two can disagree, and that is the point: a drop in the fixed version with a
flat matched ratio means the tails moved because the response distribution
compressed, not because extreme reports became less informative.

**Analysis 4 — two-stage ANOVAs on the data itself** (`R/15_anova.R`). Every
other test in the set gets its F-values from inside a mixed model, so they
inherit whatever random-effects structure the ladder settled on. This module
does the classic two-stage alternative: reduce each participant x noise-level
cell to one number computed straight from the trials, then run a mixed ANOVA on
those numbers with no model involved. The measures are the per-cell OLS slope in
each direction, the Fisher-z correlation, mean signed error, mean absolute
error, SD of the reports, SD of the objective means presented, and mean RT —
each with generalised eta squared.

Two of them earn their place beyond the cross-check. **SD of reports** is the
most direct read on compression there is: no model, just how spread out the
answers were. **SD of objective means presented** is a control — the stimulus
generator sets it, so it should not vary across noise levels, and if it does,
the noise manipulation moved the stimulus distribution too (the confound the
write-up suspects at array size 10 in Experiments 2 and 3).

`anova_vs_lmm_slopes.csv` and `fig17` put the per-participant OLS slopes next to
the model's estimates so the agreement can be checked at a glance.

**Stimulus audit** (`R/16_stimulus_audit.R`). Every other module takes the
design labels at face value. This one goes back to the raw session logs, reads
the `array_values` field that the cleaned files drop, and measures the
dispersion of the squares actually displayed on each trial.

It matters because array size and stimulus variance are not independent in
Experiments 1-3: the generator used a fixed deviation template per level, so the
10-square arrays are *more* dispersed than the 20-square arrays (5.93 against
5.50), and in Experiment 1 sizes 8 and 12 are identical (5.47). Ordering each
experiment's levels by measured dispersion rather than by array size makes the
slopes monotonic in 9 of 11 experiment x condition datasets — including the
"non-monotonic" dip at array size 10 that the write-up flags as an anomaly.
The audit joins those measurements to the model slopes, checks the ordering,
runs a meta-regression of cell slopes on item SD with an intercept per
experiment, and writes `supplementary.md` ready for the paper.

Parsing the raw logs takes a minute or two, so trial-level values are cached in
`analysis/output/stimulus_audit/item_sd_trials_<experiment>.csv`. Delete the
cache or call `epoc_stimulus_audit(force = TRUE)` to re-read.

**Cross-experiment comparisons** (`R/14_compare.R`). Two experiments that share
a noise factor get pooled so the design difference between them enters the model
as a factor, rather than being compared by eye across two write-ups:

```
report ~ objective_mean_c * noise * frame (* condition) + (random | participant)
```

Registered in `COMPARISONS` in `R/01_experiments.R`; each one runs when both of
its experiments are in the run, and lands in `analysis/output/<comparison>/`.
Currently one is registered: **Experiment 2 vs Experiment 3**, testing whether
the Mondrian frame adds regression to the mean (both used array sizes 2/10/20,
so the frame is the only design difference). The terms that answer it are
`objective mean × frame` and `objective mean × noise × frame`, reported for the
pooled model and within each condition, alongside the same comparison on
tracking correlations.

Note that in this comparison the frame is between-subjects **and**
between-batches — the two experiments ran on different samples at different
times — so a difference means the group that saw frames differed, not that
adding a frame to a given participant does that. The same caveat is printed at
the top of the comparison's `results.md`.

**Exploratory: what lies behind each report value**
(`R/18_exploratory_report_to_true.R`). **Not part of the write-up.** For every
report value it maps the mean objective value behind it, separately per noise
level, and at the tails (reports <= 14 and >= 36) tests whether the same
report came from a less extreme stimulus at higher noise, using
`objective ~ factor(report) + level + (1 | participant)`. It runs twice: on the
reports as given, and with each participant's constant error at each level
subtracted first. The second version matters in Experiments 1-3, where
2-square arrays are reported 2.4-4 units low across the whole range, a shift
that moves both tails the same way. Output goes to `output/<exp>/exploratory/`
(`exploratory.md`, tables, four figures) and
`output/exploratory/all_report_to_true_tests.csv`; nothing from it enters
`results.md` or the main cross-experiment tables. The same module runs the
question on the Goldenberg data (faces grouped 1-4 / 5-8 / 9-12, ratings <= 110
and >= 140): a rating-to-objective map and the mean exaggeration on each side
per cluster, in `output/exp0_goldenberg/exploratory/`.

The module also splits the change in slope across noise levels into its two
parts: slope = correlation x (SD of reports / SD of objective values), so on a
log scale the change from the lowest to the highest level is exactly a change
in correlation (a weaker relationship) plus a change in the SD ratio (reports
spreading less relative to the stimuli), each tested within participant.
Each table also gives the mean SD of the items inside the arrays (the
perceptual noise actually shown, from the stimulus audit's cache) next to the
SD of the true array means across trials. Output: `slope_decomposition.csv` /
`.md` in each `exploratory/` folder (Goldenberg: per set size and per cluster),
and `output/exploratory/all_slope_decomposition.csv` / `.md` with every
experiment in one table.

`R/20_exploratory_goldenberg_directions.R` runs the Goldenberg et al. (2021)
data in both directions: forward (rating on true mean) and reverse (true mean
on rating), each as a figure of binned means per cluster plus the mixed-model
slope at every set size (per valence), and r, the SDs and the SD ratio by
number of faces. Noise here is the number of faces, with each crowd's spread
constant by design; as a by-product the spread of true means across trials is
wider for small set sizes, so every descriptive is also given on the range of
true means that all set sizes cover, reweighted to the same distribution
("matched"), as in Experiments 1-3 where that spread is flat. Also tests, at the same
extreme true value, whether ratings are pulled toward the middle more with more
faces. Output: `directions_*` and `directions.md` in
`output/exp0_goldenberg/exploratory/`.

`R/19_exploratory_narrowing.R` asks why reports spread less at higher noise
(noise alone would widen them). Between the lowest and highest noise level,
within participant: the change in mean (report - true) in the bottom and top
band of true means (narrowing = bottom up, top down; a compressed stimulus
scale at one end would pull only at that end), the same with a shift common
to both ends removed, and whether the change in SD ratio grows over the
session (first vs second half of each colour block, first vs second block).
Output: `narrowing.md` / `narrowing_*.csv` in each `exploratory/` folder and
`output/exploratory/all_narrowing_tests.csv`.

`regression_panels.png` / `.csv` in each `exploratory/` folder show the
regression lines per noise level in four panels: forward and reverse, in
z-scores (where both directions have slope r and look identical) and in raw
units (where the forward slope is r x SD ratio and the reverse r / SD ratio,
so a narrowing of the reports lowers one and raises the other). Needs the
`patchwork` package.

`array_configurations.md` (with `array_config_*.csv`) checks whether the make-up
of the arrays, rather than the observer, could produce the regression: whether
the recorded true mean is the mean actually shown, whether spread, skew,
extremes or squares at the scale ends depend on the true mean, whether the
make-up predicts the report at a fixed true mean (and changes the slope when
controlled), and whether extreme arrays are skewed toward the centre. Displayed
arrays are read from the raw logs once and cached as
`output/stimulus_audit/array_config_trials_<exp>.csv`.

`piecewise_reverse.md` / `.png` / `.csv` fit the reverse direction (objective ~
report) as three joined straight segments: low reports (<= 14.5), middle, and
high reports (>= 35.5), each with its own slope per noise level, with each
participant's own middle slope per level. One straight reverse line mixes the
nearly flat end segments (objective means only span 14-36) with the middle, and
levels with more spread-out reports put more trials in the flat ends, which can
hide a slope difference in the middle. Reports as given and with each
participant's constant shift removed, plus a participant-level check;
all experiments in `output/exploratory/all_piecewise_reverse_tests.csv`.

`report_bands.md` / `.csv` group extreme reports into four bands (1-9, 10-14,
36-40, 41-50) and give, per band and noise level, the trials, participants,
mean and SD of the reports, mean and SD of the objective values behind them,
and the gap between the two; the lowest and highest noise levels are compared
within each band at the participant level, on the raw gap and on trials with the
same report value. `report_to_true_values.csv` now also carries the SD, minimum
and maximum of the objective values behind every report value.

`true_to_report.md` / `.csv` is the forward counterpart: for every true value
and noise level, the mean report, SD of the reports and bias (report - true),
a summary for low (<= 17), middle and high (>= 32) true values, and a
participant-level test of whether reports of the same extreme true value were
pulled further toward the middle at the noisiest level than at the least noisy.

## Output

`analysis/output/<experiment>/`

| File | Contents |
|---|---|
| `results.md` | readable results section with every number filled in from the tables below |
| `descriptives.csv`, `exclusions.csv`, `participants.csv` | sample, participant flow, per-participant retention and tracking ρ |
| `correlations_*.csv` | per-participant r, cell means, ANOVA, post hoc |
| `lmm_anova.csv`, `lmm_coefficients.csv`, `lmm_slopes.csv`, `lmm_contrasts.csv`, `lmm_model.txt` | Type-III tests, fixed effects, slopes per cell with CIs, all slope contrasts, full model printout |
| `lmm_bycondition_slopes.csv`, `_overall_slope.csv`, `_anova.csv`, `_coefficients.csv`, `_contrasts.csv`, `_structures.csv`, `_models.txt` | the within-condition models: slopes per noise level, overall slope, Type-III tests, simple-slope comparisons, and which random structure each condition ended on |
| `revlmm_slopes.csv`, `_overall_slope.csv`, `_anova.csv`, `_coefficients.csv`, `_contrasts.csv`, `_predictions.csv`, `_structures.csv`, `_models.txt` | the reverse-direction models (objective ~ subjective), combined and per condition |
| `exaggeration_*.csv` | per-participant proportions, ANOVA, post hoc |
| `anova_cell_measures.csv`, `anova_tests.csv`, `anova_posthoc.csv`, `anova_cell_means.csv`, `anova_vs_lmm_slopes.csv` | two-stage ANOVAs: the participant-level measures, the tests with generalised eta squared, post hoc comparisons, and the model-free vs model-based slope check |
| `tails_windows_*.csv` | symmetric report windows: cell means, models, contrasts, bracketed width, and which windows had enough trials |
| `tails_fixed_*.csv`, `tails_matched_*.csv` | objective means behind extreme reports, the bracketed window, and the matched-tail diagnosticity ratio |
| `fig1_tracking.png` | mean r per noise level and condition |
| `fig2_slopes.png` | slope per cell with 95% CI, 1 = perfect tracking marked |
| `fig3_predictions.png` | model-implied objective→subjective mapping against the identity line |
| `fig4_exaggeration.png` | proportion of exaggerated responses |
| `fig5_galton.png` | Galton squeeze: objective mean (left axis) to mean response (right axis) |
| `fig6_galton_reverse.png` (+ one per condition) | reverse squeeze: response given (left) to the mean objective value behind it (right) |
| `fig7_bucket_reverse.png` | bucketed version of the reverse direction, responses binned in 3s |
| `fig8_bucket_forward.png` | bucketed version of the forward direction |
| `galton_forward.csv`, `galton_reverse.csv`, `bucket_scatter.csv` | the numbers behind those four figures |
| `fig9_slopes_by_condition_model.png` | slopes from the separate within-condition models |
| `fig10_predictions_by_condition_model.png` | objective→subjective mapping, each condition modelled on its own |
| `fig11_reverse_slopes.png` | slopes from the reverse (objective ~ subjective) models |
| `fig12_reverse_predictions.png` | reverse mapping: report on x, predicted objective mean on y |
| `fig13_both_directions.png` | forward and reverse slopes side by side, both against 1 |
| `fig14_tails_fixed.png` | objective mean behind reports past each fixed cutoff |
| `fig16_anova_measures.png` | participant-level measures per noise level, no model involved |
| `fig17_anova_vs_lmm.png` | per-participant OLS slopes against the model's estimates |
| `fig18_report_windows.png` | objective mean behind reports of ~10 and ~40, per noise level |
| `fig15_tails_matched.png` | diagnosticity ratio for the matched extreme responses |

The exploratory Goldenberg analysis lands in `analysis/output/exp0_goldenberg/`
(`goldenberg_coefficients.csv`, `goldenberg_slopes.csv`,
`goldenberg_model_diagnostics.csv`, `fig1_goldenberg_predictions.png`,
`results.md`), and the Experiment 2 vs 3 comparison in
`analysis/output/compare_exp2_exp3/`.

`analysis/output/stimulus_audit/` holds the audit: `item_sd_by_level.csv`,
`item_sd_vs_slopes.csv`, `slope_ordering_by_sd.csv`, `meta_regression.csv`,
`fig1_item_sd_by_level.png`, `fig2_slope_vs_item_sd.png` and `supplementary.md`.

`analysis/output/` also holds the cross-experiment tables (`all_slopes.csv`,
`all_correlations.csv`, `all_exaggeration.csv`, `all_descriptives.csv`,
`all_slopes_by_condition_model.csv`, `all_slopes_reverse.csv`,
`all_tails_fixed.csv`, `all_anova_tests.csv`) and
`all_slopes.png`, which is the single figure that carries the argument: array
size and Mondrian noise move the slope around non-monotonically, while
variance drives it down in a straight, graded line in both conditions.

## Notes on the numbers

- The mixed ANOVAs use `afex::aov_ez` with Greenhouse-Geisser correction, which
  scales **both** df, so they come out fractional and smaller than the
  uncorrected df quoted in the write-up (e.g. F(1.91, 193.04) here against
  F(2, 202) there). The F and p values are the same. `results.md` prints them
  fractional, as APA asks for a corrected test; the exact values are in the
  `*_anova.csv` files.
- Experiment 1's write-up quotes df that imply by-participant slopes over array
  size (e.g. F(3, 55.58) for the three-way interaction). That structure is
  singular on this data, so the ladder settles on `(1 + meanVal_c | participant)`
  and the same interaction comes out F(3, 3950.12) = 4.10, p = .006 — same
  conclusion, different denominator df. Set `rand_force = "max"` in the registry
  entry to reproduce the write-up's version instead.
- The exploratory Goldenberg analysis reports the model from the write-up
  (group mean predicted from the rating) *and* the same data in the EPoC
  direction (rating predicted from the objective mean), so the exploratory
  slopes can be read on the same scale as Experiments 1–4.
- Everything is recomputed from `summary_all_participants.csv` on each run.
  The older per-folder scripts (`funcs/`, `data/rcode snippets/`) are left
  untouched; this folder does not overwrite anything unless
  `epoc_prepare(..., write_clean = TRUE)` is called explicitly.

## Adding an experiment

Add one entry to `EXPERIMENTS` in `R/01_experiments.R` — folders, noise
variable, levels, minimum trial count — and it flows through cleaning, both
analyses, the figures and the report with no other changes.
