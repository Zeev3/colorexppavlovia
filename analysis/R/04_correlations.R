# =========================================================
# 04_correlations.R -- Analysis 1: single-subject tracking accuracy
#
# For every participant x noise level we correlate the objective mean of the
# array with the reported value, Fisher-z transform, and run a mixed ANOVA
# (noise level within, condition between) on the z values.
# =========================================================

epoc_correlations <- function(dat, exp) {

  noise <- exp$noise_var

  by_participant <- dat %>%
    group_by(participant_id, condition, .level = .data[[noise]]) %>%
    summarise(
      n_trials = n(),
      r = if (n() > 2 && sd(meanVal) > 0 && sd(indexSelected) > 0)
            cor(meanVal, indexSelected) else NA_real_,
      .groups = "drop"
    ) %>%
    mutate(z = atanh(pmin(pmax(r, -0.999), 0.999))) %>%
    rename(!!noise := .level)

  # afex needs a complete design; drop participants missing a cell
  complete_ids <- by_participant %>%
    filter(!is.na(z)) %>%
    count(participant_id) %>%
    filter(n == length(exp$levels)) %>%
    pull(participant_id)

  aov_dat <- by_participant %>% filter(participant_id %in% complete_ids)

  has_two_conditions <- nlevels(droplevels(aov_dat$condition)) > 1

  fit <- afex::aov_ez(
    id      = "participant_id",
    dv      = "z",
    data    = as.data.frame(aov_dat),
    within  = noise,
    between = if (has_two_conditions) "condition" else NULL,
    type    = 3
  )

  anova_tab <- as.data.frame(fit$anova_table) %>%
    tibble::rownames_to_column("effect") %>%
    rename(df1 = `num Df`, df2 = `den Df`, F = `F`, p = `Pr(>F)`) %>%
    mutate(experiment = exp$id, analysis = "tracking_correlation", .before = 1)

  # follow-ups: noise levels overall and within each condition,
  # plus condition differences at each noise level
  emm_noise <- emmeans(fit, as.formula(paste("~", noise)))
  posthoc <- list(
    noise_overall = as.data.frame(pairs(emm_noise, adjust = "tukey"))
  )
  if (has_two_conditions) {
    posthoc$noise_within_condition <-
      as.data.frame(pairs(emmeans(fit, as.formula(paste("~", noise, "| condition"))), adjust = "tukey"))
    posthoc$condition_within_noise <-
      as.data.frame(pairs(emmeans(fit, as.formula(paste("~ condition |", noise))), adjust = "tukey"))
  }
  posthoc <- bind_rows(lapply(names(posthoc), function(n)
    mutate(posthoc[[n]], comparison_set = n, .before = 1)))

  # descriptive table on the r scale (back-transformed from mean z)
  summary_tab <- by_participant %>%
    group_by(condition, .level = .data[[noise]]) %>%
    summarise(
      n        = sum(!is.na(z)),
      mean_z   = mean(z, na.rm = TRUE),
      se_z     = sd(z, na.rm = TRUE) / sqrt(sum(!is.na(z))),
      mean_r   = tanh(mean_z),
      ci_low_r = tanh(mean_z - 1.96 * se_z),
      ci_high_r= tanh(mean_z + 1.96 * se_z),
      .groups  = "drop"
    ) %>%
    rename(!!noise := .level) %>%
    mutate(experiment = exp$id, .before = 1)

  list(
    by_participant = by_participant,
    summary        = summary_tab,
    anova          = anova_tab,
    posthoc        = posthoc,
    fit            = fit,
    n_dropped      = n_distinct(by_participant$participant_id) - length(complete_ids)
  )
}
