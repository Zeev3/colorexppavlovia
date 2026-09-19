# =========================================================
# 03_prepare.R -- build the analysis-ready trial table for one experiment
#
# Combines the two conditions, applies the noise recode where needed, makes
# participant ids unique across conditions, sets up the factor coding used in
# the models, and centres the objective mean within participant.
# =========================================================

# `source`:
#   "recompute" (default) runs the cleaning pipeline on
#                summary_all_participants.csv, so the whole analysis is
#                reproducible from the raw session logs
#   "cleaned"    reads the data_cleaned.csv files that are already on disk
epoc_prepare <- function(exp, source = c("recompute", "cleaned"), write_clean = FALSE) {

  source <- match.arg(source)
  if (source == "cleaned" && TRUE_VALUE == "shown")
    stop("the shown-mean pipeline re-cleans from the raw logs; it cannot use data_cleaned.csv")

  per_condition <- lapply(names(exp$dirs), function(cond) {
    dir <- file.path(DATA, exp$dirs[[cond]])
    if (source == "recompute") {
      cl <- epoc_clean_condition(exp, cond, write = write_clean)
    } else {
      cl <- list(data       = read.csv(file.path(dir, "data_cleaned.csv"), stringsAsFactors = FALSE),
                 exclusions = NULL, participants = NULL, trial_loss = NULL)
    }
    cl$data <- cl$data %>%
      select(participant_id, trial, fixationTime, meanVal, indexSelected,
             array_length, color, rt) %>%
      mutate(condition = cond)
    cl$condition <- cond
    cl
  })
  names(per_condition) <- names(exp$dirs)

  dat <- bind_rows(lapply(per_condition, `[[`, "data")) %>%
    filter(!is.na(meanVal), !is.na(indexSelected))

  # ---- noise factor ------------------------------------------------------
  if (!is.null(exp$recode)) {
    dat <- dat %>%
      mutate(variance_level = unname(exp$recode[as.character(array_length)]))
  }
  noise <- exp$noise_var
  dat <- dat %>% filter(.data[[noise]] %in% exp$levels)
  dat[[noise]] <- factor(dat[[noise]], levels = exp$levels)

  # ---- ids, condition coding --------------------------------------------
  dat <- dat %>%
    mutate(
      participant_id = factor(paste(condition, participant_id, sep = ".")),
      condition      = factor(condition, levels = intersect(c("average", "experience"),
                                                            unique(condition)))
    )

  # Effect (sum) coding for condition and the noise factor: this makes the
  # Type-III tests from anova() the tests we actually want. All slopes are
  # reported through emmeans/emtrends, which is invariant to the coding.
  if (nlevels(dat$condition) > 1) contrasts(dat$condition) <- contr.sum(nlevels(dat$condition))
  contrasts(dat[[noise]]) <- contr.sum(nlevels(dat[[noise]]))

  # ---- centre the objective mean within participant ----------------------
  dat <- dat %>%
    group_by(participant_id) %>%
    mutate(meanVal_c = meanVal - mean(meanVal, na.rm = TRUE)) %>%
    ungroup()

  # ---- exaggeration indicator -------------------------------------------
  dat <- dat %>%
    mutate(exaggerated = as.integer(
      (meanVal < EXAGG_LOW_MEAN  & indexSelected < EXAGG_LOW_RESP) |
      (meanVal > EXAGG_HIGH_MEAN & indexSelected > EXAGG_HIGH_RESP)
    ))

  attr(dat, "exp") <- exp
  list(
    data       = dat,
    exclusions = bind_rows(lapply(names(per_condition), function(c) {
      e <- per_condition[[c]]$exclusions
      if (is.null(e)) NULL else mutate(e, condition = c)
    })),
    participants = bind_rows(lapply(names(per_condition), function(c) {
      p <- per_condition[[c]]$participants
      if (is.null(p)) NULL else mutate(p, condition = c)
    })),
    trial_loss = bind_rows(lapply(names(per_condition), function(c) {
      t <- per_condition[[c]]$trial_loss
      if (is.null(t)) NULL else mutate(t, condition = c)
    }))
  )
}

# Sample descriptives that go straight into the Method / Results text
epoc_descriptives <- function(dat, exp) {
  dat %>%
    group_by(condition) %>%
    summarise(
      n_participants   = n_distinct(participant_id),
      n_trials         = n(),
      trials_per_part  = round(n() / n_distinct(participant_id), 1),
      mean_rt          = round(mean(rt, na.rm = TRUE)),
      mean_abs_error   = round(mean(abs(indexSelected - meanVal)), 2),
      .groups = "drop"
    ) %>%
    mutate(experiment = exp$id, .before = 1)
}
