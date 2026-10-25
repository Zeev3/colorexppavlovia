# Notes for the full deck (running file)

Started 19 Oct 2026. Everything here has been checked against code or data; the source is given for each point.

---

## 1. How the arrays were built in Experiments 1–3 (the "templates")

**What happened.** The arrays in Exps 1–3 were not drawn at random. For each array size, the task code holds one fixed list of offsets (a "template"). On every main-task trial the code adds that list to the trial's target shade (`meanVal`) and then shuffles the squares' positions on screen. So whenever a participant saw, say, a 20-square array with target 30, they saw exactly the same 20 shades as every other 20-square array with target 30, in a different arrangement (plus a small random vertical jitter per square).

**The templates** (task code, `getFaceSample` in `sad_sequential_task_functions.js`; Exp 2: `colorblocksavggraternoiselevels`, Exp 1: `colorblocksavg`; Exp 3 identical to Exp 2, checked with `diff`):

| Size | Offsets | Mean of offsets | Spread (SD, ÷n) |
|---|---|---|---|
| 2 | −4, 4 | 0.00 | 4.00 |
| 6 (Exp 1) | −6, −4, −2, 2, 4, 6 | 0.00 | 4.32 |
| 8 (Exp 1) | −8, −6, −4, −2, 2, 4, 6, 8 | 0.00 | 5.48 |
| 10 (Exps 2–3) | −9, −5, −8, −2, −1, 2, 3, 6, 7, 9 | +0.20 | 5.95 |
| 12 (Exp 1) | −9, −5, −8, −2, 0, −1, 2, 2, 3, 6, 7, 9 | +0.33 | 5.45 |
| 20 (Exps 2–3) | −10, −8, −7, −7, −7, −7, −7, −3, −2, −1, −1, −1, 0, 0, 1, 1, 5, 6, 9, 9 | −1.50 | 5.50 |

**Checked in the data.** In the stimulus audit's record of every displayed array (`colorexppavlovia/analysis/output/stimulus_audit/array_config_trials_exp*.csv`), every main-task trial has exactly its template's spread (Exp 2 at 20 squares: 5,856 of 5,856 trials; at 10 and 2 squares all non-matching rows are learning trials, trial numbers 1–20). The learning trials used random arrays.

**Consequences.**
- *The noise levels were set by the templates, not by the number of squares.* The spread does not follow size: 10 squares (5.95) are more spread out than 20 (5.50); in Exp 1, 8 and 12 squares have the same spread (5.48 vs 5.45). This is the V-shape in Exps 2–3 and the 12-square "recovery" in Exp 1.
- *Three templates do not average to zero.* Every 20-square array was 1.50 shades paler than the logged target; every 10-square array 0.20 more vivid; every 12-square array 0.33 more vivid. Slopes and correlations are unaffected (a constant shift within a level). Everything that compares a report with the true value is affected unless the true value is the displayed average (what we now use).
- *The shape of the array is fixed per size.* The 20-square template is lopsided (skew +0.40 in every trial): many pale squares (five at −7) and a few very vivid ones (+9, +9). When the true value is the displayed average, the tails at 20 squares move by about 1.5 shades in both directions together. Reports at 20 squares seem to follow the template's centre (the logged value) rather than its arithmetic average. Hypothesis: vivid squares weigh more in the perceived average. Related to the brightness preference in Exp 5? To test.
- *Participants saw identical sets many times.* Each target shade appears several times per size, so the same set of shades repeated. Worth a sentence in the methods; possibly a learning/familiarity issue.
- Exp 4 did not use templates: `generateArray` draws random offsets and rescales them to the target SD (n − 1), so every Exp 4 array is different.

---

## 2. The model for "does the slope change with noise?"

**The model we now use (the "linked-slopes" model)**, fitted separately for each instruction:

```r
lmer(report ~ true_c * level
            + (1 + true_c | participant)                        # own intercept + own overall slope (correlated)
            + (0 + I(true_c * (level == "3")) | participant)    # own extra slope at level 3
            + (0 + I(true_c * (level == "5")) | participant)    # ... at level 5
            + (0 + I(true_c * (level == "7")) | participant),   # ... at level 7
     data = d)
```

- `report` = recorded slider index + 1 (the response-scale fix); `true_c` = the displayed average, centred within participant; `level` = factor, sum-coded.
- **In words:** every person has their own overall slope, plus their own deviation from it at each level. A person's slope at a level = overall slope + that level's deviation. Because all levels share the overall slope, a person's slopes at different levels are positively correlated, which matches the data (good trackers are good at every level).
- **Reverse model:** the same with true colour and report swapped, but without a per-person intercept. Every participant saw the same range of true colours by design, so that variance is zero.
- Scripts and results: `meeting_ran/analysis_conservative/` (`conservative.R` = cleaning + corrections + model ladder; `linked.R` = this model; results `linked_models.csv`).

**"A deviation estimated at exactly zero" and "dropped".** Each per-level deviation term is a variance: how much people differ, at that level, beyond their overall slope. Sometimes the data contain no such differences at one level, and the best estimate of that variance is exactly 0. lme4 then reports a **singular (boundary) fit**. "Dropped" means we removed that term from the formula and refitted. Because its variance was 0, the term contributed nothing. Checked for Exp 4 average: log-likelihood −32252.01 with and without it, fixed effects identical to 0.00000006. After dropping, every one of the 16 fits converges without any warning. Which terms were dropped: in `linked_models.csv`, column `dropped` (e.g., Exp 4 forward: SD 5; Exp 2 average forward: 10 squares).

**Should we care about the warnings?** Two different warnings:
- *Singular fit* ("boundary (singular) fit"): a variance estimated at 0 or a correlation at ±1. Not an error; the fixed effects are still valid. But it says the random-effects part is more complex than the data can support, so the standard advice is to simplify (lme4's `?isSingular` help page; Bates et al., 2015). That is what dropping the zero terms does.
- *Convergence warnings* (the optimizer did not reach a clear maximum): more serious, because the estimates may be wrong. None in the final models.

**Assumptions, and whether this model breaks any.**
- *The slope structure.* It assumes the per-level deviations are uncorrelated with each other and with the overall slope, so the correlation between a person's slopes at two levels comes only from the shared overall slope. Close to the classic "compound symmetry" assumption of a repeated-measures ANOVA, but more flexible, because each level has its own deviation variance. The fully free version (every level's slope correlated with every other, `(1 | p) + (0 + true_c:level | p)`) is less restrictive, but it is singular in most cells.
- *Equal residual noise across levels.* The model assumes the leftover noise in reports is the same at every level. It is not quite: per-person residual SD 4.70 / 4.82 / 5.13 at SD 3 / 5 / 7 in Exp 4 (+9%). Checked with `nlme::lme` allowing a separate residual variance per level: the slope × level test barely moves (average F 121 → 118, experience 70 → 70), and AIC prefers the unequal version (64,551 → 64,522; 64,732 → 64,705). To do for the paper: use the per-level residual variance in the final model, or report it as a robustness check.
- *Normal random effects and residuals; independent trials within a person.* Standard; not checked yet. Report values are bounded (1–50), so residuals at the ends may be squeezed.
- *The per-person ANOVA does not need these assumptions about the random effects.* It agrees with the linked model in every condition, which is the main reassurance.

**Sources.**
- Barr, D. J., Levy, R., Scheepers, C., & Tily, H. J. (2013). Random effects structure for confirmatory hypothesis testing: Keep it maximal. *Journal of Memory and Language, 68*(3), 255–278.
- Bates, D., Kliegl, R., Vasishth, S., & Baayen, H. (2015). Parsimonious mixed models. arXiv:1506.04967.
- Matuschek, H., Kliegl, R., Vasishth, S., Baayen, H., & Bates, D. (2017). Balancing Type I error and power in linear mixed models. *Journal of Memory and Language, 94*, 305–315.
- Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting linear mixed-effects models using lme4. *Journal of Statistical Software, 67*(1), 1–48.
- Lorch, R. F., & Myers, J. L. (1990). Regression analyses of repeated measures data in cognitive research. *Journal of Experimental Psychology: Learning, Memory, and Cognition, 16*(1), 149–157. (The two-stage, per-person approach.)

**Why the earlier "full random" model disagreed.** The pipeline's `--full-random` ladder fell back to `slope_by_level_nocorr`: a separate slope per level with no shared part and no correlation. That assumes a person's slopes at different levels are unrelated, which throws away the within-person information the test needs. With it, Exp 1 (p = .10, .31), Exp 2 experience (.14) and Exp 3 (.07, .24) were not significant. Every structure that keeps the link agrees with the per-person ANOVA.

---

## 3. Coding problems found, with what they affect

- **Response scale one shade off (Exps 1–4).** The slider has 49 slices; slice i shows image i + 1 but stores i (`morphedScaleRed/Blue`, `nScale` built from images 1–49, `indexSelected = sliceSelected`). Identical in every slider repo; the array sprite and response images are the same files (md5-checked). Corrected by + 1. Shade 50 could never be chosen. Affects signed error, extreme-report counts, windows and cutoffs; not slopes or tracking.
- **Exp 4 generator allows value 0** (`generateArray(..., min = 0)` in `colorvariance`), drawn by the sprite as the most vivid shade. 16 trials, all at SD 7, arrays averaging about 15.
- **Exp 4 trial order not random** (`.sort(() => Math.random() - 0.5)`): SD 3 trials early in each block, SD 7 late; SD 7 means 0.58 higher (p = .005). Mean 36 never shown (list cut to 108 before shuffling).
- **Templates in Exps 1–3** (section 1).

---

## 4. The "same report, shift removed" test (exploratory, `R/18`) is not valid with a level-specific offset in the true value

How it works: for each person and level, the average error (report − true) is subtracted from that person's reports; then, at the same corrected report value (≤ 14 or ≥ 36), the true colours behind it are compared across levels with `true ~ factor(report) + level + (1 | participant)`.

What went wrong: the correction is applied to the report, but the test compares the true colour. With the displayed average, the true colour at 20 squares is 1.50 lower on every trial. At the extremes the true colour behind a report barely depends on the report (the reverse relation is nearly flat there), so subtracting a shift from the reports does not remove the offset in the true colour; it comes through whole. Exp 2: high tail 33.7 / 33.8 / 32.1, low tail 16.4 / 16.5 / 15.1: both drop by about 1.5 at 20 squares. Exaggeration would move the tails toward each other. Also, the model counts trials, so the p-values are tiny (10⁻⁸–10⁻¹²). Before the correction (logged true value) the same test gave p = .60 / .078 (Exp 2) and .48 / .76 (Exp 3).

Conclusion: not evidence of exaggeration; a limitation of this test. For the limitations slide.

---

## 5. Exaggeration analyses, Exp 4, before and after the corrections

| Analysis | Before (logged true, raw reports) | After (displayed true, reports + 1) |
|---|---|---|
| Report windows, near 40 | 31.96 / 31.45 / 31.03, p = .009 | 31.80 / 31.59 / 31.00, p = .009 |
| Report windows, near 10 | 17.08 / 16.88 / 17.57, p = .011 (not in order) | 16.80 / 16.57 / 17.26, p = .026 (not in order) |
| Fixed cutoff, > 36 | 32.15 / 31.72 / 31.13, p < .001 | 32.16 / 31.56 / 31.18, p < .001 |
| Fixed cutoff, < 14 | 17.10 / 17.03 / 17.45, p = .065 | 16.90 / 16.75 / 17.45, p = .003 (not in order) |
| Same report, shift removed | both tails, p < .001 / .008 | both tails, p < .001 / .008 |
| Share of extreme reports | falls with spread, p < .001 | falls, .15 / .11 / .10 (avg), p < .001 |
| Matched tails (top 10%) | p = .072 | p = .095 |
| Reverse slope | flat | flat |

Exps 1–3 after the corrections: extreme-report rate Exp 1 p = .19, Exp 2 p < .001 and Exp 3 p = .021 (both dip at 10 squares); matched tails n.s. (p = .62, .22, .43); windows and cutoffs move at 20 squares in both tails together (the template offset).

---

## 6. The maximal model: what it is, why it fails, what it gives, what the linked model loses

Checked 20 Oct 2026 (`analysis_conservative/max_diag.R`, results `max_diag.csv`), conservative data.

**Maximal forward model:** `report ~ true_c * level + (1 + true_c * level | participant)`. Per person: intercept, a shift at each level, overall slope, and a slope change at each level, all freely correlated. That is 6 per-person effects and 21 variance/correlation parameters (Exps 2–4), or 8 effects and 36 parameters (Exp 1). **Maximal reverse model:** `true ~ report * level + (0 + report + report:level | participant)` (no per-person intercept: that variance is zero by design). 4 or 5 effects, 10 or 15 parameters.

**Result of fitting it:** in all 16 fits it is singular *and* gives a convergence warning. In every fit the estimated per-person covariance matrix is degenerate: its smallest eigenvalue is 0 (to machine precision), so at least one combination of the per-person effects is estimated to have no variance at all. In the forward models the correlations reach .64–.97; in the reverse models they are pinned at exactly ±1.

**Why it fails (our reading):**
1. *Too many parameters for the number of people.* 21–36 covariance parameters, estimated from 21–86 people per instruction.
2. *The quantity it most needs is smaller than the noise.* The person-to-person differences in how the slope changes between levels are small: the per-level slope deviations have SD of about 0.03–0.12. But each person's slope at one level is itself measured with error, roughly residual SD ÷ (SD of true colours × √trials). For Exp 4 that is about 5 / (6.4 × √69) ≈ 0.09; for Exp 1 about 5 / (6.5 × √23) ≈ 0.16. When the true differences are smaller than the measurement error, the model cannot tell them apart from noise and pushes the estimate to the boundary.
3. *Reverse models:* in addition, every person saw the same range of true colours, so effects tied to the intercept have essentially no variance to estimate.

**Maximal vs linked: the results** (slopes per level; p for "does the slope change with noise?")

| | Maximal (singular + warning) | Linked (clean) |
|---|---|---|
| Exp 1 avg, forward | .905 / .809 / .745 / .706, p = .019 | .901 / .809 / .751 / .705, p = .025 |
| Exp 1 exp, forward | .887 / .829 / .756 / .934, p = .008 | .885 / .826 / .760 / .922, p = .020 |
| Exp 2 avg, forward | .815 / .677 / .738, p < .001 | .811 / .677 / .738, p < .001 |
| Exp 2 exp, forward | .922 / .829 / .878, p = .002 | .919 / .829 / .875, p = .004 |
| Exp 3 avg, forward | .829 / .703 / .741, p < .001 | .825 / .701 / .741, p < .001 |
| Exp 3 exp, forward | .880 / .798 / .882, p = .032 | .881 / .800 / .886, p = .025 |
| Exp 4 avg, forward | 1.075 / .913 / .777, p < .001 | 1.074 / .914 / .775, p < .001 |
| Exp 4 exp, forward | 1.096 / .965 / .859, p < .001 | 1.095 / .965 / .856, p < .001 |
| Exp 1 avg, reverse | p = .56 | p = .65 |
| Exp 1 exp, reverse | p = .17 | p = .15 |
| Exp 2 avg, reverse | p = .50 | p = .68 |
| Exp 2 exp, reverse | .659 / .697 / .661, p = .047 | .667 / .692 / .658, p = .13 |
| Exp 3 avg, reverse | p = .70 | p = .71 |
| Exp 3 exp, reverse | .688 / .761 / .690, p = .020 | .693 / .751 / .694, p = .049 |
| Exp 4 avg, reverse | .621 / .663 / .634, p = .006 | .626 / .661 / .628, p = .022 |
| Exp 4 exp, reverse | p = .20 | p = .17 |

Forward: same slopes (differences ≤ 0.012) and the same conclusions everywhere. Reverse: the maximal model gives smaller p in three cells (Exp 2 exp, Exp 3 exp, Exp 4 avg). In all three the reverse slope peaks at the middle level, so it is not an ordered effect of noise, and those maximal fits have correlations at ±1. Note also: in three reverse cells (Exp 1 both, Exp 2 avg) all per-level deviations were zero and dropped, so the linked reverse model there is a single slope per person, and its test uses trial-level df. All three are far from significant either way.

**What the linked model gives up, compared with the maximal one:**
- *Per-person shifts at each level.* The linked model gives each person one intercept, not a separate bias at each level. A person who reports too vivid only at SD 7 has that put into the residual. Small effect on the slope test, because the true colour is centred within person and the levels show nearly the same range of colours.
- *Free correlations.* It does not estimate the correlations between a person's per-level deviations, or between those deviations and their intercept or overall slope. It assumes the link between levels comes only from the shared overall slope, so every pair of levels is about equally correlated (with level-specific variances). It cannot capture, for example, SD 3 and SD 5 being more alike than SD 3 and SD 7.
- *What it keeps:* each person's intercept, their overall slope, the correlation between the two, and their own deviation at each level. That is the part the "does the slope change with noise" test depends on.

---

## 7. Do a person's slopes at different levels really correlate? (tested 20 Oct 2026)

Script `analysis_conservative/slope_cor_test.R`; results `slope_cor_pairs.csv`, `slope_cor_lrt.csv`. Conservative data.

**(1) Raw correlations of per-person OLS slopes between levels** (Pearson, n = participants):
- Exp 4: average .82 / .72 / .81 (SD 3–5, 3–7, 5–7); experience .79 / .63 / .74. All p < .001.
- Exp 3: .54–.74, all p < .001. Exp 2: .40–.69, all p < .001.
- Exp 1 experience: .61–.82, all p ≤ .004. Exp 1 average: .19–.69; three of six pairs not significant (2 vs 6, 2 vs 8, 8 vs 12; n = 23).

**(2) Corrected for measurement error.** Each per-level slope comes from 23–70 trials, so it is noisy, and noise lowers correlations. Reliability of the slopes at a level = (variance of the slopes − mean squared standard error) / variance of the slopes, which is about .51–.86. Corrected r = raw r / √(reliability_a × reliability_b). Exp 4: .76–1.00; Exps 2–3: .54–.99. Several corrected values come out above 1 (Exp 1 experience), which happens when reliabilities are estimated from few people; read them as "close to 1".

**(3) Direct model test: linked vs unlinked.** The unlinked model is the linked one with the shared-slope variance fixed at 0. Likelihood-ratio test, with the boundary correction (a 50:50 mixture of χ²(1) and χ²(2)):

| | χ² | p | AIC unlinked → linked | Implied correlation between levels |
|---|---|---|---|---|
| Exp 1 avg | 19.4 | < .001 (≈ 4e-5) | 13237 → 13221 | .57–.74 |
| Exp 1 exp | 60.4 | < .001 | 11960 → 11904 | .92–1.00 |
| Exp 2 avg | 78.8 | < .001 | 46508 → 46433 | .70–.92 |
| Exp 2 exp | 108.9 | < .001 | 43617 → 43512 | .87–.97 |
| Exp 3 avg | 69.1 | < .001 | 24688 → 24623 | .81–.89 |
| Exp 3 exp | 56.8 | < .001 | 20997 → 20945 | .88–.93 |
| Exp 4 avg | 113.7 | < .001 | 64640 → 64530 | .92–.97 |
| Exp 4 exp | 101.1 | < .001 | 64802 → 64705 | .86–.93 |

Conclusion: in every condition, adding the link improves the fit decisively. The unlinked model's assumption (zero correlation) is rejected everywhere. Weakest case: Exp 1 average (small sample, lower correlations), still p < .001.

---

## 8. "Same extreme report, less extreme array": a test that the number of extreme reports cannot drive (20 Oct 2026)

**The concern (Zeev):** "more noise = more extreme reports" is not the claim. The claim is that under noisier conditions the *same* extreme report comes from a less extreme array. The share of extreme reports does change with spread (Exp 4: reports ≥ 36 are 11.3 / 10.0 / 10.6% of trials; reports ≤ 14 are 13.5 / 9.4 / 8.1%). So fixed cutoffs and windows can move just because the mix of report values inside the tail differs between levels (at SD 3 more reports sit far out).

**The test** (`analysis_conservative/same_report.R`):
- For each report value in a tail (≤ 14 or ≥ 36) and each level, the mean true colour behind it.
- The difference between two levels at the same report value, averaged over report values with equal weight: each value counts once, however many trials it has.
- Only values with ≥ 5 trials at both levels.
- 95% CI by resampling participants (2,000 samples).
- Positive = less extreme at the noisier level.

**Exp 4:**
- Vivid end (36–49, 12 values): SD 7 vs 3 +0.95 [0.56, 1.36], p < .001; SD 5 vs 3 +0.46 (p = .004); SD 7 vs 5 +0.50 (p = .025). In order.
- Pale end (2–14, 12 values): SD 7 vs 3 +0.59 [−0.13, 1.18], p = .14; others n.s.

**Exps 1–3:** at 2 vs 20 squares (Exps 2–3) and 2 vs 12 (Exp 1) the two tails move in opposite directions. That is a constant shift: the template offset, and the 2-square arrays reported too pale. At 10 squares (the noisiest by spread): nothing (p = .20–.98).

**Consequence:** the defensible statement is "at the vivid end, the same report meant an array about 1 shade less vivid at the highest spread (Exp 4)". The earlier pale-end p-values (fixed cutoff p = .003, window p = .026) depended on the mix of report values. NOTE: the summary sent to Ran last night quotes the pale-end cutoff result (16.9 → 16.7 → 17.5, p = .003, "not in order"); the deck now corrects this.

## 9. Things noticed while building the deck

- After the +1 correction, reports in Exp 4 are on average slightly too vivid, increasingly with spread: mean signed error +0.03 / +0.56 / +0.62 (average) and +0.35 / +0.98 / +1.42 (experience); spread p < .001. Possibly linked to the Exp 5 brightness preference and the 20-square template observation. Not yet analysed.
- Exp 5 pale/vivid outward pull: unregularised per-person logistic fits give 0.115 / 0.156 (difference p = .34); a slightly regularised version gave 0.110 / 0.158 (p = .24). The deck uses the first (it matches its figure); the summary to Ran used the second. Same conclusion.
- Amit's per-set-size slopes are noisy (two per-size fits singular / non-converging), so quote the linear trend: −0.017 (happy) and −0.015 (angry) per face, both p < .001.
