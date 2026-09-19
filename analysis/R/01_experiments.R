# =========================================================
# 01_experiments.R -- the experiment registry
#
# One entry per experiment reported in "results round 3".
# Everything downstream (cleaning, models, figures, report text)
# is driven from this list, so adding an experiment means adding
# an entry here and nothing else.
#
# Fields
#   id           short slug, used for output folder names
#   label        human readable title used in figures / report
#   dirs         path (relative to data/) of the raw + cleaned files
#                for each condition
#   noise        name of the noise factor in the write-up
#   noise_var    column used in the models ("array_length" or "variance_level")
#   noise_label  axis label
#   levels       levels of the noise factor, in ascending order of noise
#   recode       optional named vector: raw array_length value -> analysis level
#   min_trials   minimum number of raw main-task trials to keep a participant
#   ref_level    reference level for treatment-coded follow-ups
#   rand_start   which rung of the random-effects ladder to start at
#                (see 05_lmm.R); the ladder walks down from here until a model
#                converges without a singular fit
#   later_session optional named vector: participant_id -> condition. The
#                participant also took part in the other condition; that
#                later session is dropped so every participant contributes
#                one session (the first, by raw-file timestamp)
#   reported     TRUE for the four experiments in the manuscript,
#                FALSE for pilots / extra batches kept for reference
# =========================================================

EXPERIMENTS <- list(

  # ------------------------------------------------------------------
  # Experiment 1 -- array size 2 / 6 / 8 / 12, 1.5 s exposure
  # Reported N after cleaning: 23 average, 21 experience
  # ------------------------------------------------------------------
  exp1 = list(
    id          = "exp1",
    label       = "Experiment 1 - array size (2/6/8/12)",
    dirs        = c(average    = "avg_datapilot1",
                    experience = "exp_datapilot1"),
    noise       = "array size",
    noise_var   = "array_length",
    noise_label = "Number of squares",
    levels      = c(2, 6, 8, 12),
    recode      = NULL,
    min_trials  = 96,
    ref_level   = "2",
    rand_start  = "mean_slope",
    reported    = TRUE
  ),

  # ------------------------------------------------------------------
  # Experiment 2 -- array size 2 / 10 / 20
  # Reported N after cleaning: 86 average, 80 experience
  # ------------------------------------------------------------------
  exp2 = list(
    id          = "exp2",
    label       = "Experiment 2 - array size (2/10/20)",
    dirs        = c(average    = "greaternoise/colorblocksavggraternoiselevels",
                    experience = "greaternoise/colorsquersexpgreaternoise"),
    noise       = "array size",
    noise_var   = "array_length",
    noise_label = "Number of squares",
    levels      = c(2, 10, 20),
    recode      = NULL,
    min_trials  = 96,
    ref_level   = "2",
    rand_start  = "noise_slope",
    # took part in both conditions, average first each time:
    # 6143af87 2025-09-17 / 2025-09-18, 676ed801 2025-07-04 / 2025-09-18
    later_session = c("6143af8721d1445864c4f6c8" = "experience",
                      "676ed8014625d5ebd417b5f2" = "experience"),
    reported    = TRUE
  ),

  # ------------------------------------------------------------------
  # Experiment 3 -- array size 2 / 10 / 20 plus a Mondrian frame
  # Reported N after cleaning: 46 average, 39 experience
  # ------------------------------------------------------------------
  exp3 = list(
    id          = "exp3",
    label       = "Experiment 3 - Mondrian frame (2/10/20)",
    dirs        = c(average    = "mondrian/mondrian_avg",
                    experience = "mondrian/mondrian_exp"),
    noise       = "array size",
    noise_var   = "array_length",
    noise_label = "Number of squares",
    levels      = c(2, 10, 20),
    recode      = NULL,
    min_trials  = 96,
    ref_level   = "2",
    rand_start  = "mean_slope",
    # took part in both conditions: average 2025-10-05, experience 2025-10-15
    later_session = c("66a02d45a7db35d93c9f6a81" = "experience"),
    reported    = TRUE
  ),

  # ------------------------------------------------------------------
  # Experiment 4 -- variance control, SD 3 / 5 / 7, always 10 squares,
  # 500 ms exposure, 216 trials.
  #
  # NOTE: the raw logs reuse the `array_length` column to store the
  # variance level, coded 2 -> SD 3, 6 -> SD 5, 8 -> SD 7. The real array
  # length is always 10. `recode` below undoes that.
  # Reported N after cleaning: 50 average, 51 experience
  # ------------------------------------------------------------------
  exp4 = list(
    id          = "exp4",
    label       = "Experiment 4 - stimulus variance (SD 3/5/7)",
    dirs        = c(average    = "variancecontrol/variance_control_avg",
                    experience = "variancecontrol/variance_control_exp"),
    noise       = "stimulus variance",
    noise_var   = "variance_level",
    noise_label = "Variance level (SD)",
    levels      = c(3, 5, 7),
    recode      = c("2" = 3, "6" = 5, "8" = 7),
    min_trials  = 216,
    ref_level   = "3",
    # the write-up keeps random intercepts and slopes for both the objective
    # mean and the variance level here, even though the fit is singular, so
    # this rung is forced rather than walked down from
    rand_start  = "mean_plus_noise",
    rand_force  = "mean_plus_noise",
    # took part in both conditions: average 2026-03-24, experience 2026-03-25
    later_session = c("5eb278ce4602871c6d7e335d" = "experience"),
    reported    = TRUE
  ),

  # ------------------------------------------------------------------
  # NOT an independent dataset. The greaternoise2 folders hold a partial
  # early snapshot of the Experiment 2 collection: every raw session file
  # there is also in the greaternoise folders (74 of 110 and 74 of 117),
  # all 57 + 49 participant ids are Experiment 2 participants, and the
  # cleaned trial rows are identical. Kept so the snapshot can still be
  # run, but `subset_of` marks it as redundant, and anything that pools
  # across experiments must exclude it or it double-counts.
  # ------------------------------------------------------------------
  exp2_batch2 = list(
    id          = "exp2_batch2",
    label       = "Partial snapshot of Experiment 2 (subset, not independent)",
    subset_of   = "exp2",
    dirs        = c(average    = "greaternoise2/grater_noise_avg2",
                    experience = "greaternoise2/greater_noise_exp2"),
    noise       = "array size",
    noise_var   = "array_length",
    noise_label = "Number of squares",
    levels      = c(2, 10, 20),
    recode      = NULL,
    min_trials  = 96,
    ref_level   = "2",
    rand_start  = "noise_slope",
    # 6143af87 took part in both conditions (average first); in this snapshot
    # 676ed801 appears only in experience, but that too was a second session
    later_session = c("6143af8721d1445864c4f6c8" = "experience",
                      "676ed8014625d5ebd417b5f2" = "experience"),
    reported    = FALSE
  ),

  short_exposure = list(
    id          = "short_exposure",
    label       = "Pilot - short exposure, experience only (2/10/20)",
    dirs        = c(experience = "greater_variance"),
    noise       = "array size",
    noise_var   = "array_length",
    noise_label = "Number of squares",
    levels      = c(2, 10, 20),
    recode      = NULL,
    min_trials  = 96,
    ref_level   = "2",
    rand_start  = "mean_slope",
    reported    = FALSE
  )
)

# =========================================================
# Cross-experiment comparisons (see 14_compare.R)
#
# Two experiments that share a noise factor get pooled so the design difference
# between them enters as a factor rather than being compared by eye.
# =========================================================

COMPARISONS <- list(
  compare_exp2_exp3 = list(
    id     = "compare_exp2_exp3",
    label  = "Experiment 2 vs 3 - does the Mondrian frame add regression to the mean?",
    a      = "exp2",
    b      = "exp3",
    factor = "frame",
    labels = c(exp2 = "no frame", exp3 = "Mondrian frame")
  )
)

# Experiments reported in the manuscript, in order
REPORTED <- names(EXPERIMENTS)[vapply(EXPERIMENTS, function(e) isTRUE(e$reported), logical(1))]

exp_out_dir <- function(exp) {
  d <- file.path(OUT, exp$id)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}
