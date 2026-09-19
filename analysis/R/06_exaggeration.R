# =========================================================
# 06_exaggeration.R -- Analysis 3: exaggeration
#
# A response counts as exaggerated when it is more extreme than the stimulus
# on the side the stimulus was already on: response < 14 for objective means
# below 20, response > 36 for objective means above 30 (thresholds live in
# 00_setup.R). We take the proportion of exaggerated trials per participant x
# noise level and run the same mixed ANOVA as for the correlations.
#
# Exaggeration and regression to the mean are opposite predictions about what
# noise does, which is why both are tested on every experiment.
# =========================================================

epoc_exaggeration <- function(dat, exp) {

  noise <- exp$noise_var

  by_participant <- dat %>%
    group_by(participant_id, condition, .level = .data[[noise]]) %>%
    summarise(
      n_trials      = n(),
      n_eligible    = sum(meanVal < EXAGG_LOW_MEAN | meanVal > EXAGG_HIGH_MEAN),
      n_exaggerated = sum(exaggerated),
      prop_exagg    = mean(exaggerated),
      .groups = "drop"
    ) %>%
    rename(!!noise := .level)

  complete_ids <- by_participant %>%
    count(participant_id) %>%
    filter(n == length(exp$levels)) %>%
    pull(participant_id)

  aov_dat <- by_participant %>% filter(participant_id %in% complete_ids)
  has_two_conditions <- nlevels(droplevels(aov_dat$condition)) > 1

  fit <- afex::aov_ez(
    id      = "participant_id",
    dv      = "prop_exagg",
    data    = as.data.frame(aov_dat),
    within  = noise,
    between = if (has_two_conditions) "condition" else NULL,
    type    = 3
  )

  anova_tab <- as.data.frame(fit$anova_table) %>%
    tibble::rownames_to_column("effect") %>%
    rename(df1 = `num Df`, df2 = `den Df`, F = `F`, p = `Pr(>F)`) %>%
    mutate(experiment = exp$id, analysis = "exaggeration", .before = 1)

  posthoc <- list(
    noise_overall = as.data.frame(pairs(emmeans(fit, as.formula(paste("~", noise))), adjust = "tukey"))
  )
  if (has_two_conditions) {
    posthoc$noise_within_condition <-
      as.data.frame(pairs(emmeans(fit, as.formula(paste("~", noise, "| condition"))), adjust = "tukey"))
    posthoc$condition_within_noise <-
      as.data.frame(pairs(emmeans(fit, as.formula(paste("~ condition |", noise))), adjust = "tukey"))
  }
  posthoc <- bind_rows(lapply(names(posthoc), function(n)
    mutate(posthoc[[n]], comparison_set = n, .before = 1)))

  summary_tab <- by_participant %>%
    group_by(condition, .level = .data[[noise]]) %>%
    summarise(
      n         = n(),
      mean_prop = mean(prop_exagg),
      se        = sd(prop_exagg) / sqrt(n()),
      .groups   = "drop"
    ) %>%
    rename(!!noise := .level) %>%
    mutate(experiment = exp$id, .before = 1)

  list(
    by_participant = by_participant,
    summary        = summary_tab,
    anova          = anova_tab,
    posthoc        = posthoc,
    fit            = fit
  )
}
