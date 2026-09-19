# Handoff: designing the next experiments (A: extended lightness scale, B: colour wheel)

Written 2026-09-19 at the end of a long analysis session, to continue the design work in a fresh session.

## Where things live

- **Proposal doc (Claude Docs):** "Next experiments: extended lightness scale and colour wheel" — https://claude.ai/code/artifact/3a345ed7-7b1a-492d-84ac-07a5da75d52a (doc id `3a345ed7-7b1a-492d-84ac-07a5da75d52a`; last known rev 20). Sections: Why a new design · Experiment A (lead) · Experiment B (second) · How each design solves each problem (with a mermaid comparison diagram) · Predictions and analysis plan · Limits and open decisions · Next steps.
- **Interactive simulation:** "Three Stimulus Designs" — https://claude.ai/artifact/FCr3Jdf1AwayQtxMBMs88c. Source file was in the old session's scratchpad (not in the repo); to change it, read it back with the Artifact tool (`action: "read"`, url above) and republish with `url`.
- **Experiment 4 task code:** `/Users/zeevbenamos/Documents/GitHub/colorvariance/` (`index.html`, `sad_sequential_task_functions.js`).
- **Analysis pipeline:** `analysis/` in this repo (`run_all.R`, exploratory module `R/18_exploratory_report_to_true.R`, alternatives `--shown-mean` and `--full-random`), walkthrough documents in `analysis/reports/`.

## The decision so far

- Present **both** designs, **lead with Experiment A** (extended lightness scale) because it keeps "extreme" = more intense experience (the thesis claim); Experiment B (colour wheel) tests the same prediction on an identity dimension with no scale ends.
- The user has further thoughts, caveats and improvements for the designs and the simulation — start by asking for them.

## Why a new design (the problem)

Experiment 4 used one 0–50 lightness line for three purposes at once: where trial means come from (14–35, flat), room for the squares around each mean (only 14 steps at each end; SD 7 with 10 squares needs about ±17.5), and where people answer (0–48 in practice). Consequences: no stimuli near the ends; reports above 35 sit on a hard wall (flat ends of the reverse relationship, untestable outer report bands); noise level and range of means are coupled. The comparisons between noise levels are still clean because all levels share the same distribution of means.

## Experiment A (lead): extended lightness scale

- Red and blue scales of 100 steps, same perceptual step size as Experiment 4 (one step ≈ 0.6 L*), extended toward lighter and darker shades (red roughly L* 25–92 instead of 53–84), perceptually even in CIELAB. The pilot must check that hue holds at the extremes (dark red → brown, light red → pink) and on typical screens.
- Means: normal, centre 50, SD 12 steps, truncated 25–75 (optional range manipulation: SD 8 vs 16).
- Arrays: 10 squares, exact mean, exact SD 3 / 5 / 7 steps (as Experiment 4's `generateArray`); most extreme array (mean 75, SD 7) reaches about 92.5 — nothing clipped.
- Trial: fixation 500 ms, array 500 ms, mask, response bar over all 100 steps, nothing preselected, a click only counts after crossing the bar. Instructions unchanged ("how red/blue on average" / "how red/blue did it feel").
- 72 trials per noise level, red and blue blocks, 216 total (as Experiment 4).

## Experiment B (second): colour wheel

- 360 hues at CIELAB L* 60, chroma 40. Hidden reference hue R per block (e.g. 0/90/180/270° + random offset per participant).
- Means: normal around R, SD 30°, truncated ±75° (optional SD 15° vs 35°).
- Arrays: 10 squares, SD 5° / 15° / 25°; widest reach 75 + 2.5×25 ≈ 137° < 180°, so no wrap ambiguity.
- Response: full wheel, randomly rotated every trial, nothing preselected. Experience instruction: "the colour the group appeared to be".
- Analysis in signed angles from R (wrapped ±180°); mixture model or 90° cut-off for guesses.
- Caveats: identity rather than intensity; hue category boundaries; monitor variation.

## Fixes carried over from Experiment 4 (must be in both)

- Proper shuffle (`jsPsych.randomization.shuffle`) on a full crossing of level × mean — Experiment 4 used `.sort(() => Math.random() - 0.5)` on ordered lists, so SD 3 trials came earlier / SD 7 later and SD 7 got slightly higher true means.
- Build exactly the trials needed; never slice before shuffling (Experiment 4 built 14–36 ×5 = 115 values and kept the first 108, so 35 was rare and 36 never shown).
- Response colour must equal the stimulus colour it names (Experiment 4: response index i shows image i+1 → about −1 step offset; the CSS sprite also maps value 0 to the opposite end).
- No default response (a 0 was saved when people clicked without crossing the scale).
- Add single-square trials (response noise and colour mapping) and consider average vs experience within participants.

## Key Experiment 4 numbers the designs build on

- Tracking r .83 / .78 / .71 (SD 3/5/7); forward slope 1.08 / 0.94 / 0.82 (strict model F(2,191) = 30.3, p < .001); 58% of the drop from the correlation, 42% from narrower reports; reverse slope ≈ .6 at every level.
- Same extreme report: stimulus 0.6–1.0 unit less extreme at SD 7 (high reports as given; both tails with each participant's shift removed); same extreme true value: reports pulled 1.8–2.4 units further toward the middle at SD 7.
- Simulation observer uses slope 1.08/0.94/0.82 and residual SD 5.66/5.73/6.09 steps (wheel: 3° per step, illustrative).

## Open items

- The user's thoughts, caveats and improvements on both designs and the simulation (not yet heard).
- Pilot plan: hue range of the extended scale, noise levels feeling similar across the scale, tracking spanning roughly r = .85 to .65.
- Decide: range manipulation in or out; average vs experience between or within; preregistration details.
