# Analysis guide: what each test does, and what it found

**Rule for every main result: experience instruction only.** All main analyses (models, ANOVAs, post hocs, Galton diagrams, extremes) use only the participants who got the experience instruction. Nothing is pooled with the average instruction, and there is no between-subjects factor. The average instruction, and any comparison between the two instructions, is kept separately in the appendix (section 9).

Data: conservative clean data (true colour = displayed average of the squares, report + 1, all exclusions), `analysis_conservative/data/`. Experience participants: Exp 1 21, Exp 2 77, Exp 3 38, Exp 4 50.
Script for every number in sections 1–8: `analysis_conservative/experience_only.R` → `experience_only_results.csv` (tests) and `experience_only_cells.csv` (each person's stage-1 numbers). Same-report test: `same_report_by_instruction.R`. Figures in `guide_figures/` (to be redrawn experience-only).

---

## 0. Vocabulary

| Word | Meaning |
|---|---|
| **True colour** | The average shade of the 10 squares actually shown (scale 1–50, pale → vivid). |
| **Report** | The shade the participant chose on the slider (+1 correction). |
| **Variance level** | How much the shades within an array vary around their average, set as their SD: SD 3, 5, 7 (Exp 4). Within participants. In Exps 1–3 the noise factor is array size. |
| **Forward slope** | Change in the report per 1 shade of true colour. 1 = follows exactly; < 1 = pulled toward the middle (regression to the mean). |
| **Reverse slope** | Change in the true colour per 1 shade of report: "given a report, how extreme was the array?" Galton's direction. |
| **Tracking r** | Correlation between report and true colour. |
| **Design (main analyses)** | One within-participants factor: variance level (3 levels). Experience instruction only. |

---

## 1. How each per-person DV is calculated (stage 1)

For **each participant, at each variance level separately** (Exp 4: about 70 trials per level), we take that person's trials at that level and compute one number:

| DV | How it is calculated from the person's trials at that level |
|---|---|
| **Forward slope** | Regress the reports on the true colours (ordinary least squares, `lm(report ~ true)`); the slope. |
| **Reverse slope** | Regress the true colours on the reports (`lm(true ~ report)`); the slope. |
| **Tracking r** | Pearson correlation between the reports and the true colours. For the test it is Fisher z-transformed (z = atanh r), because r is bounded; means are shown back as r. |
| **SD of reports** | Standard deviation of the person's reports. |
| **SD of true colours (control)** | Standard deviation of the true colours the person was shown. Should not differ between levels. |
| **Bias** | Mean of (report − true colour). Positive = reports more vivid than the arrays. |
| **Share of extreme reports** | Proportion of trials where the report was below 14 while the true colour was below 20, or above 36 while the true colour was above 30. |
| **Weight on the previous trial** | Regress the reports on the current true colour and the previous trial's true colour (`lm(report ~ true + previous true)`); the coefficient of the previous trial. |

Result: one number per person per level (Exp 4 experience: 50 people × 3 levels = 150 numbers per DV).

## 2. How each DV is tested (stage 2)

- **Repeated-measures ANOVA** on the stage-1 numbers. DV: the per-person number. One factor: variance level (within participants). No instruction factor. Greenhouse–Geisser correction (that is why the df are not whole numbers). Effect size: generalised η² (ges).
- **Post hocs:** every pair of levels (3 vs 5, 3 vs 7, 5 vs 7), paired t-tests, Holm-corrected.
- **Linear trend:** contrast −1, 0, +1 over SD 3, 5, 7 (a step-by-step change). In Exps 1–3 the levels are taken in order with equal spacing.

This is a two-stage regression (summary-statistics approach): stage 1 fits one regression per person per level; stage 2 asks whether the people's numbers differ between levels. No modelling choices; each person counts once per level. Its cost: each slope is estimated from ~70 trials, so its measurement noise enters the error term (slightly conservative).

**The model (linked slopes)** answers the same forward and reverse questions from all trials at once, fitted on experience participants only:
```
report ~ true_c * level + (1 + true_c | participant) + (0 + I(true_c * (level == "3")) | participant) + (0 + I(true_c * (level == "7")) | participant)
```
true_c = true colour centred within each person. Each person has their own intercept and overall slope plus their own extra slope at each level (a term estimated at exactly 0 is dropped; here SD 5).

---

## 3. Experiment 4, experience instruction: results

### 3.1 Forward slope (the regression-to-the-mean test)
- **Per-person ANOVA:** 1.10 → 0.97 → 0.85; F(1.74, 85.3) = 43.5, p < .001, ges = .14.
  - Post hocs: SD 5 − SD 3 = −0.13, SD 7 − SD 3 = −0.25, SD 7 − SD 5 = −0.11, all p < .001.
  - Linear trend p < .001; no curvature: a straight decline.
- **Linked model:** 1.095 [1.03, 1.16] → 0.965 [0.90, 1.03] → 0.856 [0.79, 0.93]; slope × level F(2, 74.8) = 41.1, p < .001; clean fit.
  - Pairs: −0.130, −0.239, −0.109, all p < .001; linear trend p < .001.
- **Say:** "Within the experience instruction, each step of variance lowers the slope, linearly."

**Reading the model's coefficient table.** `level` is sum-coded, so each `true_c:level` coefficient compares one level's slope with the *average* slope: `true_c` = 0.972 (average of the three); `true_c:level1` = +0.123 (SD 3 vs average, p < .001); `true_c:level2` = −0.007 (SD 5 vs average, p = .58). SD 5 is the middle level, so it equals the average by construction; that coefficient is not a test of anything we care about. The tests between levels are the pairs and the trend above.

### 3.1b Is the slope at each level different from 1? (`slope_vs_one.R` → `slope_vs_one.csv`)
Slope 1 means the report follows the colour exactly; below 1 = regression to the mean; above 1 = reports more extreme than the colours (expansion). Tested two ways, Holm-corrected across the three levels:
- **Per person:** one-sample t-test of the 50 people's own slopes against 1.
- **Model:** each level's slope from the linked model tested against 1.

| Experience, forward slope | SD 3 | SD 5 | SD 7 |
|---|---|---|---|
| Per-person mean [95% CI] | 1.100 [1.03, 1.17] | 0.966 [0.90, 1.03] | 0.855 [0.78, 0.93] |
| Difference from 1, p (Holm) | +0.10, p = .016 | −0.03, p = .29 | −0.15, p < .001 |
| People with slope below 1 | 20 of 50 | 25 of 50 | 32 of 50 |
| Linked model, p (Holm) | +0.10, p = .017 | −0.04, p = .29 | −0.14, p < .001 |

**What it means:** the slope is not simply "below 1 and getting lower". At SD 3 reports are slightly *more* extreme than the colours (slope above 1); at SD 5 they follow the colour (not different from 1); only at SD 7 is there a clear pull toward the middle. Variance moves the slope from expansion, through accuracy, to regression. Say it this way: "regression to the mean appears at the highest variance; at the lowest variance reports are, if anything, slightly expanded."

### 3.2 Reverse slope
- **Per-person ANOVA:** 0.63 → 0.62 → 0.60; F(1.49, 73.3) = 2.33, p = .12. No pair differs (all p ≥ .30); linear trend p = .099.
- **Linked model:** 0.629 → 0.619 → 0.598; F(2, 78.8) = 1.80, p = .17.
- **Post hocs:** no pair of levels differs (per person all p ≥ .30; model all p ≥ .19).
- **Against 1:** far below 1 at every level (0.63 / 0.62 / 0.60; all p < .001; 49–50 of 50 people below 1). Given a report, the array was always much less extreme than the report, at every level; that gap does not change with variance.
- **Say:** "Flat: about 0.6 at every level, far below 1, no pair differs." (Why: section 4.)

### 3.3 Tracking (r)
- .83 → .78 → .72; F(1.93, 94.8) = 69.9, p < .001, ges = .20. Every pair p < .001; linear trend p < .001.

### 3.4 SD of reports, and the control
- SD of reports: 8.47 → 7.84 → 7.49; F(1.60, 78.2) = 17.2, p < .001. Pairs: 3 vs 5 and 3 vs 7 p < .001, 5 vs 7 p = .019. Linear p < .001.
- SD of the true colours shown: 6.28 → 6.16 → 6.10; F = 2.36, p = .10; no pair differs after correction, linear p = .048. A small drop (3%) next to the 12% drop in the reports; the ratio G = SD reports ÷ SD true still falls: 1.35 → 1.27 → 1.23.

### 3.5 Bias (report − true)
- +0.36 → +0.98 → +1.42; F(1.39, 68.1) = 12.5, p < .001. All pairs differ (5 vs 7 p = .021). Reports drift more vivid with variance.

### 3.6 Extremes
- **Share of extreme reports:** 16.5% → 12.0% → 11.2%; F(1.75, 85.7) = 16.4, p < .001. 3 vs 5 and 3 vs 7 p < .001; 5 vs 7 n.s. Extreme reports become *rarer*.
- **Report windows** (true colour behind reports within ±2 of 10 and of 40): near 10: 17.0 / 16.8 / 17.2, p = .44; near 40: 31.8 / 31.3 / 31.3, p = .37. Not significant in the experience instruction alone.
- **Same-report test** (main exaggeration test: same report value, SD 7 vs SD 3, each report value counted once, participants resampled): vivid end +0.97 shades [0.42, 1.51], p = .002 (SD 5 vs 3 +0.50, p = .032; SD 7 vs 5 +0.47, p = .26); pale end +0.57 [−0.30, 1.55], p = .23.
- **Say:** "Behind the same vivid report, the array was about one shade less vivid at SD 7 than at SD 3. Not at the pale end. The coarser window test does not show it in the experience group alone."

### 3.7 Weight on the previous trial (serial dependence)
- 0.052 → 0.102 → 0.120; F(1.96, 96.0) = 3.80, p = .027; linear trend p = .016. Pairs: 3 vs 7 p = .049; others n.s.
- **Say:** "At higher variance, answers lean more on the previous trial."

---

## 4. Why the answers narrow, and why the reverse slope stays flat

**Fact:** answers became less spread out with variance (8.47 → 7.49) while the colours shown barely changed (6.28 → 6.10). Noise alone would make answers *more* spread out.

**Readings:**
1. **Reliance on an expectation.** Estimate = w × evidence + (1 − w) × expected value; w falls as the evidence gets noisier, so answers narrow, the slope flattens and extreme reports get rarer. Supported by 3.7: answers lean more on the previous trial at higher variance.
2. **A response strategy** (avoiding the slider ends when unsure) predicts the same; slider data cannot fully separate it.
3. **Not an artefact:** both ends of the scale narrow, and the narrowing does not grow over the session.

**Reverse slope:** forward = r × G, reverse = r ÷ G. Variance lowers r (3.3) and G (3.4). Forward: both push down → falls. Reverse: they cancel → flat (3.2).

---

## 5. Experiments 1–3, experience instruction

| Experience only | Exp 1 (2/6/8/12 sq.) | Exp 2 (2/10/20) | Exp 3 (2/10/20 + frame) |
|---|---|---|---|
| People | 21 | 77 | 38 |
| Forward slope by size (per person) | 0.89 / 0.84 / 0.76 / 0.94 | 0.92 / 0.83 / 0.88 | 0.89 / 0.80 / 0.88 |
| Per-person ANOVA | F(2.24, 44.9) = 4.99, p = .009 | F(1.94, 148) = 6.34, p = .003 | F(1.92, 71.0) = 3.71, p = .031 |
| Pairs that differ (Holm) | 8 < 2 (p = .038); 12 > 8 (p = .003) | 10 < 2 (p = .002) | none (10 vs 2 p = .059; 20 vs 10 p = .053) |
| Linear trend | p = .73 | p = .13 | p = .84 |
| Lowest vs the rest | p = .28 | p = .007 | p = .17 |
| Linked model, slope × size | F(3, 81.4) = 3.46, p = .020 | F(2, 84.6) = 5.95, p = .004 | F(2, 38.1) = 4.06, p = .025 |
| Reverse slope (per-person ANOVA / model) | p = .036 / .15 | p = .93 / .13 | p = .13 / .049 |
| Tracking r | .82 / .77 / .72 / .79, p = .003 | .81 / .77 / .78, p = .021 | .80 / .80 / .80, p = .98 |
| SD of reports | p = .24 | p = .15 | p = .12 |
| Share of extreme reports | p = .12 | p = .66 | p = .71 |

**Forward slope against 1 (per person, Holm):**

| Experience | Slope by size (p vs 1) |
|---|---|
| Exp 1 | 2: 0.89 (p = .19) · 6: 0.84 (p = .095) · 8: 0.76 (p = .003) · 12: 0.94 (p = .45) |
| Exp 2 | 2: 0.92 (p = .013) · 10: 0.83 (p < .001) · 20: 0.88 (p < .001) |
| Exp 3 | 2: 0.89 (p = .025) · 10: 0.80 (p < .001) · 20: 0.88 (p = .025) |

Reverse slopes are below 1 at every size in every experiment (0.63–0.77, all p < .001).

**Reading:** in each of Exps 1–3 the slope changes with array size, but **not in order of size**: the lowest slope is at 8 squares (Exp 1) or 10 squares (Exps 2–3), and there is no linear trend. This matches the template variance (8 = 12 in Exp 1; 10 > 20 in Exps 2–3). Unlike Exp 4, the answers do not narrow and extreme reports do not become rarer.

---

## 6. One-line answers

- *What is the DV of the main ANOVA?* Each experience participant's own forward slope (OLS, report on true colour) at each variance level.
- *What is the IV?* Variance level, within participants. No instruction factor.
- *What do the post hocs show?* In Exp 4 every pair of levels differs (all p < .001), linearly.
- *Why is `true_c:level2` not significant?* It compares SD 5 with the average slope; zero by construction. Use the pairs and the trend.
- *Why do answers narrow?* Most likely more reliance on an expectation when the input is noisy (answers lean more on the previous trial at higher variance); a response strategy cannot be ruled out.

---

## 7. What changed from the pooled analyses (for your control)

- Exp 4: same conclusions; every forward and tracking test holds in experience alone. Two things are weaker alone: the reverse slope (pooled p = .042 with a bump at SD 5 in the average group; experience alone p = .12, flat) and the report windows (pooled p = .026 / .009; experience alone n.s.). The same-report test holds (p = .002).
- Exps 1–3: the omnibus tests hold in experience alone; the ordered pattern does not (section 5).

## 8. Files

`experience_only.R`, `experience_only_results.csv`, `experience_only_cells.csv` (main); `same_report_by_instruction.R` (same-report test per instruction); `compare_linked_vs_maximal.R` (model check, set CONDITION = "experience").

---

## 9. Appendix: average instruction and comparisons between instructions (kept separate)

- **Average instruction, Exp 4:** forward slope 1.07 → 0.91 → 0.78; per-person ANOVA F(1.82, 91.1) = 96.2, model F(2, 66.2) = 85.9, all pairs p < .001. Reverse: 0.63 / 0.67 / 0.63, p = .030 (SD 5 highest). Tracking .83 / .79 / .71. Same-report vivid end +1.06, p < .001; pale end n.s.
- **Between instructions (Exp 4):** no difference in the forward slope (p = .24), tracking (p = .88) or SD of reports (p = .22).
- **Across all four experiments:** experience slopes are higher by 0.10 on average (p < .001), the opposite of the prediction.
- Scripts for the pooled and comparison analyses: `explain_exp4.R`, `posthocs_all.R`, `conservative.R`.
