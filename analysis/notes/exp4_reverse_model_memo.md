# Can we trust the objective ~ subjective model in Experiment 4?

**Question.** Experiment 4 (stimulus variance, SD 3/5/7; 52 average, 51 experience
participants, ~210 trials each) was analysed in both directions. The two models
disagree sharply in effect size. Before we use the reverse model to test the
exaggeration hypothesis, we need to know whether that disagreement reflects
something about perception or something about the measure.

**Short answer.** The data are sound and both models are correctly fitted, but the
reverse slope cannot carry the exaggeration claim: its near-null result is an
arithmetic cancellation, not a finding. The claim is testable — and significant —
in a different form, on the same data. No new data collection is needed for this
question.

---

## 1. What the two models say

Both are linear mixed models with the predictor centred within participant and
random intercepts and slopes by participant.

**Forward, report ~ objective mean.** The slope is the transfer function: 1 means
the report tracks the stimulus one-to-one, below 1 means compression toward the
participant's own mean.

| | SD 3 | SD 5 | SD 7 |
|---|---|---|---|
| experience | 1.095 | 0.966 | 0.859 |
| average | 1.073 | 0.916 | 0.777 |

Objective mean × variance: F(2, 10584) = 68.22, p < .001 (experience);
F(2, 10668) = 115.57, p < .001 (average); F(2, 21254) = 180.37, p < .001 in the
combined model. Every pairwise step is significant at p < .001 in both conditions.

**Reverse, objective mean ~ report.** The slope answers the inverse question:
given a report, how extreme was the stimulus behind it.

| | SD 3 | SD 5 | SD 7 |
|---|---|---|---|
| experience | 0.616 | 0.612 | 0.589 |
| average | 0.618 | 0.650 | 0.614 |

Report × variance: F(2, 10608) = 3.13, p = .044 (experience, only SD 3 vs SD 7
reaching significance); F(2, 10716) = 4.91, p = .007 (average, but non-monotonic —
the slope *rises* from SD 3 to SD 5, p = .023, then falls, p = .014).

## 2. The problem

Same trials, same participants, same random-effects structure. The forward slope
moves 22% (experience) and 28% (average) across the variance manipulation. The
reverse slope moves 4% and 1%, and in the average condition it does not even move
monotonically.

That is not a small difference in sensitivity, it is a difference in kind. Taken at
face value the reverse model says the inverse mapping is essentially unaffected by
stimulus variance — which, if true, would undercut the exaggeration hypothesis,
since that hypothesis is naturally phrased in the reverse direction ("when someone
reports 40, is there less behind it under noise?").

## 3. Why the two directions diverge

The two OLS slopes are not free to disagree. With `x` the objective mean, `y` the
report, `r` their correlation and `G = SD_y / SD_x`:

```
b_forward = r · G          b_reverse = r / G
b_forward · b_reverse = r²      b_forward / b_reverse = G²
```

There are only two independent quantities here, `r` and `G`; the two slopes are the
same information in different coordinates. Rearranging gives the key relation:

```
Δlog b_reverse  =  Δlog b_forward  −  2 · Δlog G
```

The reverse slope changes only to the extent that a change in the forward slope is
*not* accompanied by a proportional change in the spread of the responses. Noise
lowers both `r` and `G`, so the two effects compound in the forward slope and
cancel in the reverse one.

Checked against the fitted models, lowest versus highest stimulus dispersion:

| | Δlog r | Δlog G | predicted Δlog b_fwd | observed | predicted Δlog b_rev | observed |
|---|---|---|---|---|---|---|
| Exp 4, average | −0.163 | −0.147 | −0.310 | −0.323 | −0.016 | −0.007 |
| Exp 4, experience | −0.159 | −0.084 | −0.243 | −0.242 | −0.075 | −0.046 |
| Exp 2, average | −0.093 | −0.090 | −0.182 | −0.180 | −0.003 | −0.007 |
| Exp 2, experience | −0.048 | −0.048 | −0.096 | −0.104 | +0.000 | −0.006 |
| Exp 3, average | −0.120 | −0.068 | −0.188 | −0.169 | −0.052 | −0.033 |
| Exp 3, experience | −0.025 | −0.082 | −0.106 | −0.101 | +0.057 | +0.080 |

In the Experiment 4 average condition `r` and `G` fall almost exactly in step
(−0.163 against −0.147), so the reverse slope is frozen while the forward slope
drops 28%. In Experiment 3's experience condition `G` falls *more* than `r`, and
the reverse slope correspondingly goes up. The reverse column is predicted, sign
and rough magnitude, in every cell.

Two further properties of the reverse slope are worth recording:

**It is a function of quantities we already have.** Because the stimulus is set by
us and carries no measurement error, regressing it on the report gives
`b_rev = b · SD_x² / (b² · SD_x² + σ_e²)`, where `σ_e` is the residual SD of the
reports. Computing that from the forward fit alone predicts the fitted reverse
slopes to within 0.009 in every Experiment 4 cell. The reverse model estimates no
new parameter.

**Its conditional means are reconstructable.** Rebuilding E[objective | report]
from each participant's forward line, their own residual SD and their own set of
presented stimuli reproduces the observed binned means to a weighted mean absolute
error of **0.098 scale units** on a 50-point scale, across ~21,000 trials. The
pronounced curvature of the reverse function — reports spanning 2.5 to 47.5 map
onto objective means spanning only 16.8 to 32.3 — is fully accounted for by the
bounded stimulus list plus response noise, not by anything perceptual.

**One caveat that does *not* apply here.** Because `b_rev` depends on `SD_x`, it
changes when the stimulus range changes, so it is a property of observer *and*
design rather than of the observer alone. Within Experiment 4 the stimulus
distribution is matched across variance levels — the SD of the objective means
presented is 6.18 / 6.22 / 6.19, F(1.98, 199.7) = 1.41, p = .248 — so comparisons
*across variance levels* are clean. The dependence would only bite if we compared
across experiments with different stimulus ranges.

## 4. The alternative: test the claim where it lives

The exaggeration hypothesis, stated plainly, is about the tails: *when someone
reports an extreme value, is there less behind it under noise?* The global reverse
slope is a poor instrument for that, because it averages a large inward pull at the
edges together with nothing in the middle, on top of the cancellation above.

Mean objective value behind each report bin, Experiment 4:

| report | n at SD 3 | objective, SD 3 | objective, SD 7 | change |
|---|---|---|---|---|
| ≤ 12 | 784 | 17.33 | 17.92 | **+0.59** |
| 13–17 | 1334 | 18.82 | 19.27 | **+0.46** |
| 18–22 | 1397 | 21.67 | 21.70 | +0.03 |
| 23–27 | 1308 | 25.52 | 25.40 | −0.12 |
| 28–32 | 1197 | 28.83 | 27.96 | **−0.87** |
| 33–37 | 747 | 30.94 | 30.07 | **−0.86** |
| ≥ 38 | 390 | 32.17 | 30.93 | **−1.24** |

All of the movement is in the tails, in both directions, and it is flat through the
middle where most trials sit.

**Recommended test — symmetric report windows.** Take trials where the report fell
within ±2 of 10, and within ±2 of 40, and ask what was behind them:

| | SD 3 | SD 5 | SD 7 |
|---|---|---|---|
| reported ~10 | 17.06 | 16.96 | 17.64 |
| reported ~40 | 31.97 | 31.46 | 31.03 |
| **reality bracketed by a 30-unit report span** | **14.90** | **14.50** | **13.39** |

Low window: F(2, 1389) = 4.74, p = .009 (SD 3 vs 7, p = .026; SD 5 vs 7, p = .013).
High window: F(2, 798) = 4.73, p = .009 (SD 3 vs 7, p = .006). SEs on the widths are
~0.33, so the 1.5-unit narrowing is roughly 4–5 standard errors.

A report of 40 is backed by ~1 scale unit less extreme a stimulus at SD 7 than at
SD 3, and a report of 10 by ~0.6 units. **A given extreme report carries about 10%
less reality when the array is noisy.** That is the exaggeration claim, tested
directly, on the same data, significant on both tails.

The independent robustness check already run — the fixed cutoffs used for the
exaggeration measure (< 14, > 36) — gives the same answer: the bracketed window
narrows 14.97 → 14.54 → 13.54.

**Two constructs, currently both called "exaggeration".** They move in opposite
directions and separating them resolves most of the apparent tension in the
results:

- *Systematic overshoot* — the transfer function amplifies, forward slope > 1. In
  Experiment 4 this holds only at SD 3 (average 1.073, 95% CI [1.009, 1.137];
  experience 1.095, [1.031, 1.159]) and it **decreases** with variance.
- *Diagnosticity loss* — an extreme report backs less extreme reality, the windows
  result above. This **increases** with variance.

The rate of extreme responding, measured as the proportion of trials past the
fixed cutoffs, also falls with variance (0.169 → 0.121 → 0.108 in experience;
F(1.80, 181.5) = 46.06, p < .001), consistent with the first construct rather than
the second.

Worth noting for interpretation: the response-to-stimulus SD ratio `G`, which is
the natural "were responses more spread out than the stimuli" measure, does not
isolate systematic amplification either, since `G² = b² + (σ_e / SD_x)²`. A ratio
above 1 can be produced by response noise alone with a compressive transfer
function. The clean pair to report is the gain `b` and the response noise `σ_e`.

## 5. Verdict

**The data are trustworthy.** Both models fit correctly, the reverse model
reproduces the observed conditional means to a tenth of a scale unit, and the
stimulus distribution is matched across the variance levels, so the comparison of
interest is not confounded by design.

**The reverse slope is not trustworthy as evidence about the effect of variance.**
Its small, partly non-monotonic effect is what the algebra predicts when `r` and
`G` shrink together; reporting it as "variance does not affect the inverse mapping"
would be reporting a cancellation.

**What to do:**

1. Report the forward model as the estimate of the perceptual mapping. It is the
   direction in which the stimulus is error-free, so its slope is the unbiased
   transfer function.
2. Test the exaggeration hypothesis with the symmetric report windows, citing the
   fixed-cutoff version as a robustness check.
3. Keep the reverse model and the reverse Galton diagram as description — they
   communicate the double squeeze well — but state that the flat slope reflects `r`
   and `G` moving together rather than stability of the inverse mapping.
4. Separate the two exaggeration constructs by name in the write-up.

**No further data are needed for this question.** Two loose ends are worth
resolving separately: the analysis pipeline yields 52 average participants where
the current draft reports 50, and the set-size versus dispersion question raised by
the stimulus audit of Experiments 1–3 would need a design that crosses the two.

---

*All figures generated by `analysis/run_all.R`. Experiment 4 outputs in
`analysis/output/exp4/` (`lmm_bycondition_*`, `revlmm_*`, `tails_windows_*`,
`anova_*`); cross-experiment tables in `analysis/output/`.*
