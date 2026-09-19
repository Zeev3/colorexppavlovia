# Handoff: designing the next experiments (A: extended lightness scale, B: saturation scale)

First written 2026-09-19 at the end of a long analysis session; rewritten the same day after a second design session. Use it to continue the design work in a fresh session.

## Where things live

- **Proposal doc (Claude Docs):** "Next experiments: lightness and saturation scales" — https://claude.ai/code/artifact/3a345ed7-7b1a-492d-84ac-07a5da75d52a (doc id `3a345ed7-7b1a-492d-84ac-07a5da75d52a`; last known rev 28). Sections: Why a new design · What Experiment 4 says about noise · Experiment A (lead) · Experiment B · How the new designs address each problem · Predictions and analysis plan · Limits and open decisions · Next steps.
- **Interactive simulation:** "Three Stimulus Designs" — https://claude.ai/artifact/FCr3Jdf1AwayQtxMBMs88c (version 5). The source is not in the repo; to change it, read it back with the Artifact tool (`action: "read"`, url above) and republish with `url`.
- **Experiment 4 task code:** `/Users/zeevbenamos/Documents/GitHub/colorvariance/` (`index.html`, `sad_sequential_task_functions.js`).
- **Analysis pipeline:** `analysis/` in this repo (`run_all.R`; exploratory modules `R/18_exploratory_report_to_true.R` and `R/19_exploratory_narrowing.R`; alternatives `--shown-mean` and `--full-random`), walkthrough documents in `analysis/reports/`.

## Decisions so far

- **Experiment A (lead): extended lightness scale.** Continues Experiment 4 directly; "extreme" = more intense.
- **Experiment B: saturation scale, grey to vivid red/blue.** Replaces the colour wheel: on a wheel participants would see greens and purples, and "how red did it feel" makes no sense there. Saturation makes "how red" literal. A reds-only hue arc was considered and dropped (hue is identity, not intensity).
- **Noise levels SD 4 / 9 / 14** (three levels), in both experiments. The simulation keeps Experiment 4's SD 3 / 5 / 7 as its default, with a switch to the wider levels.
- **Array generator:** jittered evenly spaced squares (one per tenth of the spread, jittered within the middle half of its tenth, rescaled to the exact mean and SD, shuffled in position). Farthest square at most 1.71 SD from the mean, so SD 14 fits at mean 75 with no redrawing. Experiment 4's `generateArray` reaches about 2.3 SD and redraws arrays near the ends, which changes their shape exactly where the analysis looks.
- **Two blocks of means**, one per colour (counterbalanced): normal SD 12, centred at 42 (cut 25–59) and at 58 (cut 41–75), on a 0–100 answer bar. A pull toward each block's own centre = the observer regressing toward the means it has seen; a pull toward 50 for both = a pull toward the middle of the bar. Fallback: one centre at 50, cut 25–75.
- **The forward test is primary** (report given the true value: no wall, every true value was a real stimulus). The reverse test (exaggeration) is secondary.
- **Design criterion for any noise manipulation:** tracking falls while the forward slope holds. Since forward × reverse slope = r², exaggeration can only grow with noise if the drop in r² goes into the reverse slope.

## What changed in the understanding (keep this framing)

- **The longer scale is mostly a stretch.** Relative to the spread of means, room beyond the most extreme mean is about the same (2.3 vs 2.2 SDs of means). The extra room for noise comes from the array generator; the physical range is wider (more intense extremes); the wall behind reports beyond the most extreme mean remains in any bounded design.
- **Experiment 4's noise narrowed reports.** SD of reports 8.53 / 7.76 / 7.50 at SD 3 / 5 / 7 with the SD of true means flat (6.28 / 6.20 / 6.23); r .80 / .75 / .68 (pooled). Noise alone would widen reports. The narrowing cancelled the growth of exaggeration: the reverse slope stayed about .6 at every level.
- **Do not treat the narrowing as "compensation" or "hedging" by default.** Active correction is a legitimate hypothesis, but the data cannot tell why reports narrow, and regression within perception itself fits equally well. Describe it neutrally ("reports narrow") and weigh the explanations against the data. In Experiment 4 the levels were interleaved, and response times did not differ by level (median 1,890 / 1,888 / 1,872 ms at SD 3 / 5 / 7; SD 7 vs SD 3 −19 ms, p = .30), so there is no sign that participants treated the levels differently — weak evidence, since RT is a crude measure.
- **Narrowing checks** (`R/19_exploratory_narrowing.R`, SD 3 vs SD 7, within participant): lowest true means (14–18) reported 2.5 steps higher, highest (31–35) 1.8 steps lower, both p < .001; stronger at the light end, so the compressed vivid end of Experiment 4's scale (last ten steps about half size) is not the cause. No growth over the session (halves p = .89, blocks p = .94). The average and experience instructions both narrow (46% vs 38% of the slope drop from the SD ratio). Array-size experiments 1–3 show little pull once a common shift is removed (+0.4 to +0.6 steps, n.s., against +2.1 in Experiment 4), which hints that the visible spread of the array matters.
- **Reverse-analysis power** is set by the manipulation, not the analysis: better binning or more trials cannot create a reverse effect if the forward slope absorbs the drop in r². The biggest lever is a noise manipulation that does not narrow reports, e.g. hidden noise (exposure duration or mask timing with a fixed spread) — to be piloted, not assumed.

## Simulation (version 5) — what it shows

- Cards: Experiment 4, Experiment A (lightness, lead), Experiment B (saturation). Controls: colour, noise set (3/5/7 or 4/9/14), noise level, trial mean, distribution of means (normal SD 8 / 12 / 16, flat), blocks (one centre or two blocks 42/58), direction (reverse or forward), pull target (centre of the means seen, or middle of the bar).
- Observer fitted to Experiment 4: perceived mean = true + noise with variance 8.5 + 0.42 × SD²; shrunk toward the pull target with weight w = SDmeans² / (SDmeans² + that variance); gain 1.40; response noise SD 4.14 steps. Reproduces Experiment 4's slopes (1.07 / 0.95 / 0.81) and error SDs (5.6–6.0). Beyond SD 7 it is extrapolation.
- Predicted tracking r: one centre SD 4 / 9 / 14 = .90 / .81 / .70; two blocks = .85 / .73 / .59. Extreme trials (outer fifth of the range of means) per level of 72: about 13 (normal SD 12, one centre), 17 (two blocks), 29 (flat).
- Saturation scale in the simulation: OKLCh lightness 0.58, hue 29° (red) / 264° (blue), chroma 0–0.21 (about 10% headroom in sRGB).

## Fixes carried over from Experiment 4 (must be in both)

- Proper shuffle (`jsPsych.randomization.shuffle`) on a full crossing of level × mean — Experiment 4 used `.sort(() => Math.random() - 0.5)` on ordered lists, so SD 3 trials came earlier / SD 7 later and SD 7 got slightly higher true means.
- Build exactly the trials needed; never slice before shuffling (Experiment 4 built 14–36 ×5 = 115 values and kept the first 108, so 35 was rare and 36 never shown).
- Response colour must equal the stimulus colour it names (Experiment 4: response index i shows image i+1 → about −1 step offset; the CSS sprite also maps value 0 to the opposite end).
- No default response (a 0 was saved when people clicked without crossing the scale).
- Replace `generateArray` with jittered evenly spaced arrays that never need redrawing.
- Add single-square trials (response noise and colour mapping) and consider average vs experience within participants.

## Open items

- **Noise manipulation:** visible spread (array SD) vs hidden noise (exposure duration / mask timing with fixed spread), judged in the pilot on the design criterion (per level: tracking r, SD of reports, forward slope).
- **Per-trial confidence rating** ("how sure are you?") in the pilot, added to the doc's pilot checklist: tests whether participants register the noise level, and whether low-confidence trials narrow more — a direct test of active correction against regression within perception. Optional alternative: a short end-of-session discrimination block ("which array is more mixed?").
- **Distribution of means:** two blocks (42 / 58) vs one centre; normal vs flat (flat gives more extreme trials but a weaker reverse effect in the simulation).
- **Colour tied to block centre** if each colour gets one block: counterbalance; allow a few trials for the pull to settle after a block change.
- **Saturation scale pilot:** hue drift (OKLCh), Helmholtz–Kohlrausch brightness, steps even in ΔE2000 or OKLab, grey as an anchor at the low end, monitor variation of vivid colours.
- **Lightness scale pilot:** how far red and blue extend with one hue; SD 4 / 9 / 14 feeling similar across each scale.
- Average vs experience between or within; preregistration details (forward slope by noise, slope split, pull target, regression-based same-report test).
