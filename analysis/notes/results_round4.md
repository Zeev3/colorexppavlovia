# Regression to the mean in subjective experience — results

*Round 4. All analyses regenerated from the raw session logs by `analysis/run_all.R`;
every number below is traceable to a file in `analysis/output/`.*

---

## Exploratory analysis of existing data (Goldenberg et al., 2021)

### Method

To examine our hypothesis we looked for an existing paradigm that would let us
observe the effect of added variance on participants' experience. We used the open
data of Goldenberg et al. (2021), Experiment 1, which used the ensemble perception
paradigm to study how people estimate the average emotion of crowds.

### Materials

Fifty participants completed 150 trials each. On every trial participants saw an
array of up to 12 faces expressing different intensities of emotion, either neutral
to angry or neutral to happy. The average valence of the faces and the set size were
randomised. After each trial participants reported the average valence using a scale
that morphed a face on screen to match their answer. Faces and responses were scored
on a 100–150 valence scale. After removing trials with missing values, 7,304 trials
from 50 participants entered the analysis.

### Results

A linear mixed model predicted the group mean valence from the participant's rating,
the number of faces and their interaction, with random intercepts for participants,
fitted separately for positive and negative arrays.

For **positive** arrays there were significant main effects of rating, *b* = 0.71,
*SE* = 0.02, *t*(3783.87) = 35.64, *p* < .001, and of number of faces, *b* = 4.49,
*SE* = 0.41, *t*(3763.10) = 10.91, *p* < .001, and a significant rating × number of
faces interaction, *b* = −0.036, *SE* = 0.003, *t*(3762.25) = −11.14, *p* < .001: the
association between rating and group valence weakened as the number of faces
increased.

For **negative** arrays the pattern replicated: rating, *b* = 0.69, *SE* = 0.02,
*t*(3493.62) = 30.27, *p* < .001; number of faces, *b* = 3.74, *SE* = 0.44,
*t*(3481.55) = 8.53, *p* < .001; interaction, *b* = −0.030, *SE* = 0.003,
*t*(3481.70) = −8.66, *p* < .001.

**[Figure 1 about here]**

To place this on the same footing as the colour experiments that follow, we refitted
the same data in the opposite direction — rating predicted from the objective group
mean, centred within participant, with random slopes. The slope of the objective mean
declined with set size in both valences: positive, *b* = 0.70 at 2 faces, 0.64 at 6,
0.54 at 12; negative, 0.70, 0.64, 0.55. Slopes below 1 mean the ratings are compressed
toward the participant's own mean, and the compression grows with set size.

---

## The paradigm

To extend the exploratory analysis we looked for further evidence of regression to the
mean in low-level visual perception, for two reasons. A low-level paradigm lets us
control the amount and source of variance precisely, and it lets us ask whether
noise-driven distortions appear even in perceptual functions that are normally robust.

In the **Ensemble Phenomenology of Colour (EPoC)** paradigm each stimulus is a set of
coloured squares, all red or all blue in primary hue but varying in shade. The colour
scales were built by segmenting a continuous gradient into 50 distinct shades coded
1–50 (red #FFC0CB → #FF0000; blue #ADD8E6 → #0000FF). After the array disappeared,
participants used a continuous response scale to match a single square to the perceived
average hue of the set; moving the cursor left to right shifted the square from pale to
bright. The analysis compares the objective colour mean of each array with the
participant's report.

**[Figure 2 about here]**

Across experiments we manipulated perceptual noise by increasing either the number of
items in the array or the variance of item values. The prediction was that greater
variability makes extraction of the ensemble mean less precise, producing more
regression to the mean — a shallower relationship between objective mean and reported
value.

### Analysis strategy, common to all experiments

Data were cleaned identically throughout. Training trials were removed; trials with
RT < 200 ms or > 8,000 ms were excluded; a within-participant ±2.5 SD trim was applied
to RT; trials whose absolute deviation from the objective mean exceeded 30 scale units
were removed. Participants retaining fewer than 85% of their trials, and participants
whose Spearman correlation between objective mean and report was ≤ .30, were excluded.

Three analyses are reported for every experiment.

1. **Tracking accuracy.** The within-participant correlation between objective mean and
   report at each noise level, Fisher-*z* transformed, in a mixed ANOVA with noise
   within and condition between. Greenhouse–Geisser corrected throughout, so degrees of
   freedom are fractional.
2. **Regression to the mean.** A linear mixed model of the report on the objective mean
   (centred within participant), noise level and their interaction, with random
   intercepts and slopes. The slope of the objective mean is the measure: 1 is
   one-to-one tracking, below 1 is compression toward the participant's own mean.
   Each condition is modelled on its own first — this is the primary model — followed by
   a combined model that adds condition and tests whether the two differ.
3. **Exaggeration.** The proportion of responses more extreme than the stimulus
   (< 14 when the objective mean is < 20, > 36 when it is > 30), per participant and
   noise level, in the same mixed ANOVA. This is supplemented by a tail analysis asking
   what objective value lies behind an extreme report (Supplement C).

None of the experiments was preregistered.

---

## Experiment 1 — array size 2 / 6 / 8 / 12

### Participants

Participants were recruited on Prolific; inclusion required US nationality and
residence, fluent English, and no colour blindness. Thirty-three participants
(16 female, *M* age = 43) were assigned to the average-assessment condition and
thirty-six (18 female, *M* age = 41) to the experience-assessment condition. Usable
session files were returned by 32 and 30 participants respectively.

### Method

Participants completed an EPoC task with four array sizes — 2, 6, 8 or 12 squares.
After each array they gave one of two ratings. In the **average** condition they
adjusted the response square to match the average colour of the array; in the
**experience** condition they adjusted it to match how red or blue the stimulus felt
to them. The main task was 12 trials per array size within each colour block, 96 trials
in total, with block order randomised. Twenty learning trials preceded the main task.

### Results

**Preprocessing.** In the average condition 8 participants were excluded for low trial
retention and 1 for not tracking the stimulus, leaving *N* = 23. In the experience
condition 6 and 3 were excluded, leaving *N* = 21. Mean 92 trials retained per
participant; mean absolute error 5.08 (average) and 5.01 (experience) scale units.

**Tracking accuracy.** Correlations were positive throughout. Mean *r* fell from .78 to
.72 across array size in the average condition and from .82 to .72 in the experience
condition, with a recovery at size 12 in the experience condition (.79). Main effect of
array size, *F*(2.91, 122.25) = 5.73, *p* = .001; no effect of condition,
*F*(1, 42) = 0.67, *p* = .416; no interaction, *F*(2.91, 122.25) = 0.99, *p* = .399.
Only the size 2 versus size 8 contrast in the experience condition survived correction,
*p* = .006.

**Regression to the mean.** In the **experience** condition responses tracked the
objective mean overall, *b* = 0.85, 95% CI [0.73, 0.97], and the slope varied with array
size, *F*(3, 1881) = 3.94, *p* = .008. Slopes were 0.89, 0.83, 0.76 and 0.93 at sizes
2, 6, 8 and 12 — compression up to size 8 and then a return, with only the 8 versus 12
contrast significant, *p* = .009.

In the **average** condition the decline was orderly: 0.90, 0.80, 0.77, 0.72,
*F*(3, 2070) = 4.65, *p* = .003, with size 2 differing from both 8 (*p* = .043) and 12
(*p* = .002).

The combined model found no effect of condition, *F*(1, 42) = 0.06, *p* = .810, and no
objective mean × condition interaction, *F*(1, 41.99) = 0.48, *p* = .491, but a
significant three-way interaction, *F*(3, 3950) = 4.10, *p* = .006, driven entirely by
size 12, where the conditions differed by 0.21, *p* = .022.

**[Figure 3 about here]**

**Exaggeration.** No effects: array size, *F*(2.50, 105.16) = 1.41, *p* = .248;
condition, *F*(1, 42) = 0.88, *p* = .353; interaction, *F*(2.50, 105.16) = 1.62,
*p* = .196.

Experiment 1 is the smallest sample in the set and its experience-condition pattern
should be read with that in mind. As Section 7 shows, the apparent recovery at size 12
occurs at a stimulus dispersion identical to size 8, so it cannot be a variance effect
in either direction.

---

## Experiment 2 — array size 2 / 10 / 20

Because Experiment 1 produced only a narrow range of noise, Experiment 2 widened the
array sizes to create more distinct levels of visual load.

### Participants

One hundred participants (50 female, *M* age = 42) were assigned to the average
condition and one hundred (50 female, *M* age = 40.5) to the experience condition.
Usable session files were returned by 92 and 91 participants.

### Method

As Experiment 1, with array sizes of 2, 10 or 20 squares, 16 trials per array size
within each colour block, 96 trials in total.

### Results

**Preprocessing.** Five average and eight experience participants were excluded for low
retention, one and three for not tracking the stimulus, leaving *N* = 86 and *N* = 80.
Mean 92 trials per participant.

**Tracking accuracy.** Mean *r* = .81 / .74 / .77 (average) and .81 / .77 / .78
(experience). Main effect of array size, *F*(1.91, 313.24) = 16.77, *p* < .001; no
effect of condition, *F*(1, 164) = 0.66, *p* = .417; no interaction,
*F*(1.91, 313.24) = 1.57, *p* = .211. Note that accuracy is lowest at size 10, not at
size 20.

**Regression to the mean.** Experience: overall *b* = 0.87, 95% CI [0.86, 0.89], with
slopes 0.92, 0.83, 0.87 at sizes 2, 10, 20, *F*(2, 7210) = 8.03, *p* < .001; only the
2 versus 10 contrast was significant, *p* < .001. Average: overall *b* = 0.75, slopes
0.81, 0.68, 0.74, *F*(2, 7751) = 25.38, *p* < .001, with all three contrasts
significant.

The combined model found a main effect of condition, *F*(1, 164.02) = 11.07, *p* = .001,
and an objective mean × condition interaction, *F*(1, 15029) = 115.14, *p* < .001 —
slopes were higher in the experience condition at every array size (all *p* < .001) —
but no three-way interaction, *F*(2, 14956) = 1.16, *p* = .315.

**[Figure 4 about here]**

**Exaggeration.** Main effect of array size, *F*(1.92, 314.93) = 10.73, *p* < .001, and
of condition, *F*(1, 164) = 4.63, *p* = .033, with no interaction, *p* = .073. The array
size effect is again non-monotonic and driven by the average condition, where
exaggeration dipped at size 10 (.079) relative to sizes 2 (.129) and 20 (.122), both
*p* < .001.

Both the tracking and the slope results are **V-shaped in array size**, with the
strongest compression at 10 rather than at 20. Section 7 shows why.

---

## Experiment 3 — array size 2 / 10 / 20 with a Mondrian frame

Given the robustness of the paradigm, Experiment 3 introduced a second source of visual
noise.

### Participants

Fifty participants (30 female, *M* age = 42.8) were assigned to the average condition
and fifty-two (27 female, *M* age = 43) to the experience condition. Usable session
files were returned by 50 and 51.

### Method

As Experiment 2, with a frame of randomly generated coloured squares surrounding the
array on every trial. The frame never contained the target colour, and participants
were instructed to ignore it.

**[Figure 5 about here]**

### Results

**Preprocessing.** After exclusions, *N* = 46 (average) and *N* = 39 (experience).

**Tracking accuracy.** Mean *r* = .81 / .73 / .75 (average) and .80 / .79 / .80
(experience). Main effect of array size, *F*(1.99, 165.40) = 5.70, *p* = .004; no effect
of condition, *F*(1, 83) = 1.66, *p* = .201; but a significant array size × condition
interaction, *F*(1.99, 165.40) = 5.34, *p* = .006. Array size reduced tracking in the
average condition (2 vs 10, *p* < .001) and not at all in the experience condition (all
*p* > .95).

**Regression to the mean.** Experience: slopes 0.87, 0.79, 0.87, *F*(2, 3526) = 5.82,
*p* = .003, with 10 differing from both 2 (*p* = .012) and 20 (*p* = .007) — the same
V-shape as Experiment 2. Average: 0.83, 0.70, 0.74, *F*(2, 4123) = 13.34, *p* < .001.
The combined model gave no objective mean × condition interaction, *F*(1, 82.74) = 2.68,
*p* = .105, and a marginal three-way interaction, *F*(2, 7648) = 2.83, *p* = .059.

**[Figure 6 about here]**

**Exaggeration.** Main effect of array size, *F*(1.95, 161.83) = 9.07, *p* < .001; no
condition effect, *p* = .161; no interaction, *p* = .325. Again driven by the average
condition and again non-monotonic.

### Does the Mondrian frame add regression to the mean?

Because Experiments 2 and 3 used identical array sizes, we pooled them with frame as a
factor. The frame had no general effect: objective mean × frame, *F*(1, 22687) = 1.20,
*p* = .273; objective mean × array size × frame, *F*(2, 22572) = 0.33, *p* = .721. Nor
did it affect tracking accuracy — main effect of frame *F*(1, 247) = 0.01, *p* = .906,
with every frame comparison *p* > .74.

There was a three-way objective mean × frame × condition interaction,
*F*(1, 22687) = 4.46, *p* = .035: averaged over array sizes the frame lowered the slope
by 0.033 in the experience condition (*p* = .028) and by nothing in the average
condition (*p* = .457). No individual array size reached significance (*p* = .070, .061,
.957), and the effect vanished at size 20.

**[Figure 7 about here]**

We do not think this supports a frame effect. It is small, absent at the highest array
size, unaccompanied by any change in tracking accuracy, and — importantly — the frame is
between-subjects *and* between-batches, since the two experiments ran on different
samples several months apart. **Irrelevant colour noise placed around the array did not
measurably increase regression to the mean.**

---

## Experiment 4 — stimulus variance SD 3 / 5 / 7

Experiments 1–3 manipulated the number of items. Experiment 4 manipulated the dispersion
of the items directly, holding the number constant.

### Participants

Sixty participants (30 female) were assigned to the average condition and seventy-one
(35 female) to the experience condition. Usable session files were returned by 53 and 58.

### Method

Every array contained 10 squares generated around a mean drawn pseudorandomly from a
predefined list spanning 14–36 on the colour scale, with each participant seeing all
means. Deviations of individual squares from that mean were drawn with a standard
deviation of 3, 5 or 7, constrained so that values stayed within 1–50. Exposure was
shortened to 500 ms, allowing 216 trials: a 20-trial learning phase followed by two
blocks of 108 trials, each block in a single colour, variance level randomised within
block and block order counterbalanced.

### Results

**Preprocessing.** One average participant was excluded for low retention; two
experience participants for low retention and five for not tracking the stimulus.
*N* = 52 (average) and *N* = 51 (experience), with a mean of 208 and 210 trials retained.

*(The previous draft reported 50 average participants. The pipeline finds 52 under the
stated criteria; the discrepancy should be resolved before submission.)*

**Tracking accuracy.** Mean *r* fell monotonically: .82 / .78 / .71 (average) and
.83 / .78 / .71 (experience). Main effect of variance, *F*(1.91, 193.04) = 131.21,
*p* < .001, with every pairwise step significant at *p* < .001 in both conditions. No
effect of condition, *F*(1, 101) = 0.04, *p* = .845, and no interaction, *p* = .520.

**[Figure 8 about here]**

**Regression to the mean.** The slope declined step by step in both conditions:

| | SD 3 | SD 5 | SD 7 |
|---|---|---|---|
| experience | 1.095 [1.029, 1.161] | 0.966 [0.900, 1.033] | 0.860 [0.794, 0.926] |
| average | 1.073 [1.010, 1.136] | 0.916 [0.852, 0.979] | 0.777 [0.714, 0.840] |

Objective mean × variance: *F*(2, 10584) = 68.22, *p* < .001 (experience) and
*F*(2, 10668) = 115.57, *p* < .001 (average); every pairwise step *p* < .001 in both.
At the lowest variance the slope is significantly **above** 1 in both conditions — mild
systematic overshoot — crossing below 1 by SD 5.

The combined model found no effect of condition, *F*(1, 100.93) = 1.39, *p* = .241, no
objective mean × condition interaction, *F*(1, 100.92) = 1.46, *p* = .230, and no
three-way interaction, *F*(2, 21254) = 2.33, *p* = .097. **The variance effect is large,
graded and identical in the two conditions.**

**[Figure 9 about here]**

**Exaggeration.** Contrary to the previous draft, extreme responding *decreased* with
variance: .159 / .109 / .103 (average) and .169 / .121 / .108 (experience). Main effect
of variance, *F*(1.80, 181.54) = 46.06, *p* < .001, with SD 3 differing from both SD 5
and SD 7 (*p* < .001) and no difference between 5 and 7. No condition effect
(*p* = .494) and no interaction (*p* = .829). The number of eligible trials was
essentially constant across levels, so this is not a base-rate artefact. Fewer extreme
responses under higher variance is what compression predicts.

**What lies behind an extreme report.** Extreme responses also became less diagnostic.
Taking trials where the report fell within ±2 of 10 or of 40, the objective mean behind
them moved toward the centre — 17.06 / 16.96 / 17.64 for reports near 10 and
31.97 / 31.46 / 31.03 for reports near 40. Low window, *F*(2, 1389) = 4.74, *p* = .009
(SD 3 vs 7, *p* = .026); high window, *F*(2, 798) = 4.73, *p* = .009 (SD 3 vs 7,
*p* = .006). The reality bracketed by a 30-unit span of reports narrowed from 14.90 to
13.39 scale units, roughly 4–5 standard errors. The fixed exaggeration cutoffs give the
same answer (14.97 → 13.54).

**[Figure 10 about here]**

Note that these two results are not in conflict: extreme responses became *rarer*, and
the ones that occurred were backed by *less extreme* stimuli. Both follow from
compression of the response distribution. See Supplement B for why these are two
distinct constructs and why they should be named apart.

---

## Reanalysis across experiments: what the arrays actually looked like

The cleaned data record array size and objective mean, but not the individual square
values. Recovering those from the raw session logs shows that dispersion was **not** a
by-product of array size. The stimulus generator used a fixed deviation template per
level, so the displayed dispersion was:

| Experiment | levels | measured item SD |
|---|---|---|
| Experiment 1 | 2 / 6 / 8 / 12 | 3.98 / 4.39 / 5.47 / **5.47** |
| Experiments 2 and 3 | 2 / 10 / 20 | 3.97 / **5.93** / 5.50 |
| Experiment 4 | SD 3 / 5 / 7 | 2.86 / 4.75 / 6.65 |

The 10-square arrays were **more** dispersed than the 20-square arrays, and in
Experiment 1 sizes 8 and 12 were identical. Array size and stimulus variance were
therefore perfectly confounded in Experiments 1–3, and confounded non-monotonically. At
array size 20 the item SD was identical on every single trial.

**[Figure 11 about here]**

Re-ordering each experiment's levels by measured dispersion rather than by array size
makes the slopes monotonic in 7 of 9 independent experiment × condition datasets
(Spearman = −1.00 in each). The two exceptions are Experiment 3's experience condition,
where the inversion is 0.003, and Experiment 1's experience condition, where the
inversion falls between two levels of *identical* dispersion.

The decisive comparison is array size 10 against 20 in Experiments 2 and 3: the set size
doubles while dispersion falls, and the slope **rises** — 0.68 → 0.74, 0.70 → 0.74 in the
average conditions. A processing-load account predicts the opposite; a dispersion account
predicts exactly this.

Regressing cell slopes on measured item SD, weighted by precision, with an intercept per
experiment: *b* = −0.058, *SE* = 0.013, *t* = −4.54, *p* < .001. Each additional unit of
dispersion costs about 0.06 of slope, and the per-experiment intercepts do not differ
reliably.

**[Figure 12 about here]**

---

## Overview

**[Figure 13 about here]**

Across four experiments and roughly 400 participants, reports of colour ensembles are
systematically compressed toward the participant's own mean, and the degree of
compression is governed by the dispersion of the items in the array.

The experiments do not divide into three failures and one success. Experiments 1–3
manipulated dispersion incidentally, over a narrow range and in a scrambled order, which
is why their slope patterns looked non-monotonic in array size; Experiment 4 manipulated
the same variable deliberately, over roughly twice the range and in the right order, and
produced a graded effect roughly three to six times larger (generalised η² = .177 against
.027–.036). Ordered by what was actually on screen, the four experiments tell one story.

Three further conclusions follow.

**Number of items is not what matters.** Where set size and dispersion move in opposite
directions — array 10 versus 20 — regression to the mean follows dispersion. Adding
irrelevant colour noise around the array (Experiment 3's Mondrian frame) did nothing
either. What degrades the ensemble estimate is variability *within* the set being
averaged, not processing load or external clutter.

**Experience and estimation behave alike.** We found no consistent difference between
the two conditions. Tracking accuracy never differed (Exp 1 *p* = .416, Exp 2 *p* = .417,
Exp 3 *p* = .201, Exp 4 *p* = .845), and in Experiment 4 — the only experiment with a
clean, graded manipulation — the variance effect on the slope was statistically identical
in the two conditions (*p* = .097 for the three-way interaction). Experiment 2 showed
consistently higher slopes in the experience condition, but this did not replicate in
Experiments 1, 3 or 4. The original prediction that noise distorts *experience* more
than *estimation* is not supported.

**Exaggeration and compression coexist.** They are not competing predictions. Under
higher variance, extreme responses became rarer and less diagnostic, while at the lowest
variance level of Experiment 4 the transfer function genuinely overshot (slope > 1).
Naming these separately — systematic overshoot versus diagnosticity loss — removes most
of the apparent tension in the earlier draft.

---

# Supplementary material

## Supplement A — Analysis pipeline and reproducibility

All results were produced by a single pipeline (`analysis/run_all.R`) that reads the raw
Pavlovia session logs, applies identical cleaning to every experiment, and writes every
table and figure reported here. A complete rebuild from raw data takes under three
minutes and reproduces the output exactly. Per-experiment output includes the participant
flow through each exclusion stage, per-participant retention and tracking coefficients,
all model tables, and the random-effects structure each model settled on.

Random effects follow a Barr-style ladder: the maximal structure named for each
experiment is fitted first and simplified only until the model converges without a
singular fit. Slopes and contrasts are obtained with `emmeans::emtrends`, so they do not
depend on the contrast coding. Mixed ANOVAs use `afex::aov_ez` with Greenhouse–Geisser
correction, which scales both degrees of freedom; fractional *df* throughout are a
consequence of that correction.

Two datasets in the archive are not independent experiments and are excluded from all
pooled analyses: the `greaternoise2` folders are a partial mid-collection snapshot of
Experiment 2 (identical session files and participants), and a short-exposure pilot
(*N* = 38, experience only) that was never reported. The four reported experiments and
the pilot share no participants.

## Supplement B — Two constructs called "exaggeration"

The write-up has used one word for two measures that move in opposite directions.

**Systematic overshoot** is a property of the transfer function: the slope of the report
on the objective mean exceeds 1, so a stimulus one unit above the mean produces a report
more than one unit above it. In this dataset it occurs only in Experiment 4 at SD 3
(average 1.073, 95% CI [1.010, 1.136]; experience 1.095, [1.029, 1.161]) and it
*decreases* with variance.

**Diagnosticity loss** is a property of the inverse mapping: a given extreme report is
backed by a less extreme stimulus. It *increases* with variance (Experiment 4 windows,
above).

A third measure sometimes used for the same idea — whether responses are more spread out
than the stimuli, *SD*(report) / *SD*(objective) — does not isolate either, because

> *SD*(report)² = *b*² · *SD*(objective)² + σ²

so the ratio exceeds 1 whenever response noise is large enough, even when the transfer
function is compressive. In Experiments 2 and 3 the experience condition has a ratio above
1 with a slope below 1: the wider response distribution there is noise, not amplification.
The clean pair to report is the slope and the residual SD.

## Supplement C — What the reverse-direction model can and cannot show

Because the exaggeration hypothesis is naturally phrased in the inverse direction, we also
fitted the objective mean on the report. That model is a poor instrument for testing the
hypothesis, for a reason that is arithmetic rather than empirical.

Writing *r* for the correlation and *G* = *SD*(report)/*SD*(objective):

> *b*(forward) = *r* · *G*  *b*(reverse) = *r* / *G*

so *b*(forward) · *b*(reverse) = *r*², and the two slopes contain only two independent
quantities. In logs,

> Δlog *b*(reverse) = Δlog *b*(forward) − 2 · Δlog *G*

Noise lowers both *r* and *G*, so the two effects compound in the forward slope and cancel
in the reverse one. In Experiment 4's average condition Δlog *r* = −0.163 and
Δlog *G* = −0.147 — almost exactly equal — so the forward slope falls 28% while the reverse
slope moves under 1%. The near-null reverse result is that cancellation, not evidence that
variance leaves the inverse mapping intact.

Two further checks confirm the reverse model adds no independent information here.
Predicting the reverse slope from the forward fit alone — *b* · *SD*²(objective) /
(*b*² · *SD*²(objective) + σ²) — reproduces the fitted values to within 0.009 in every
Experiment 4 cell. Reconstructing the whole reverse conditional-mean function from each
participant's forward line, residual SD and own stimulus set reproduces the observed binned
means to a weighted mean absolute error of 0.098 scale units across ~21,000 trials,
including its pronounced curvature — which is therefore a consequence of the bounded
stimulus list, not of perception.

The inverse question is still worth asking; it is simply better asked at the tails, where
the effect is concentrated, using the report-window analysis reported for Experiment 4.
The reverse Galton diagrams are retained as description.

**[Figure S1 about here]**
**[Figure S2 about here]**
**[Figure S3 about here]**

## Supplement D — Model-free confirmation

Because the main tests are mixed-model *F*-tests, whose values depend on the random-effects
structure, every experiment was also analysed in two stages: each participant × noise-level
cell was reduced to a single number computed directly from the trials, and those numbers
were entered into a mixed ANOVA with no model involved.

The two approaches agree. Per-participant OLS slopes match the mixed-model estimates to
within 0.005 in Experiment 4 and within 0.019 anywhere in the set. Experiment 4 by ANOVA:
*F*(1.79, 181.22) = 126.76, *p* < .001, generalised η² = .177, with all six pairwise steps
*p* < .001.

Two measures from this analysis are informative in their own right. **SD of reports** is a
model-free index of compression, and it falls monotonically with variance in Experiment 4
(8.25 → 7.16 average, 8.50 → 7.50 experience; *F*(1.60, 162.03) = 50.11, *p* < .001).
**SD of the objective means presented** is a control: it is set by the stimulus generator
and should not vary. It does not, in Experiment 4 (*p* = .248) or in Experiments 2 and 3
(*p* = .600 and *p* = .876), which rules out the possibility that the noise manipulations
also shifted the distribution of stimuli that participants saw.

**[Figure S4 about here]**
**[Figure S5 about here]**

## Supplement E — Stimulus audit

Full per-level dispersion measurements, the slope ordering by dispersion, and the
meta-regression are in `analysis/output/stimulus_audit/`. Trial-level item SDs recovered
from the raw logs are cached there as well, so the audit can be re-run or extended without
re-parsing the session files.
