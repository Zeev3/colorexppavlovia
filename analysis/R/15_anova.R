# =========================================================
# 15_anova.R -- two-stage ANOVAs on the data itself
#
# Everything else in the set tests noise effects inside a mixed model: the
# F-values come from the model's variance-covariance structure, and they depend
# on the random-effects structure the ladder happened to settle on. This module
# does the classic two-stage alternative instead:
#
#   stage 1  reduce each participant x noise-level cell to a single number
#            straight from the trials (an OLS slope, a mean error, an SD)
#   stage 2  run a mixed ANOVA on those numbers -- noise within, condition
#            between -- with no mixed model anywhere in sight
#
# If the two approaches agree, the model's conclusions are not an artefact of
# its random-effects structure. If they disagree, that is worth knowing before
# anything goes in the paper.
#
# The measures:
#   slope_fwd    per-cell OLS slope of the report on the objective mean.
#                The model-free version of the regression-to-the-mean measure.
#   slope_rev    per-cell OLS slope of the objective mean on the report.
#   r            per-cell Pearson correlation (Fisher-z transformed for the test)
#   bias         mean(report - objective mean); over- or under-shooting
#   abs_error    mean |report - objective mean|; raw accuracy
#   sd_response  SD of the reports; direct read on whether responses contract
#   sd_objective SD of the objective means presented. A control: this is set by
#                the stimulus generator, so it should not differ across noise
#                levels. If it does, the "noise" manipulation moved the stimulus
#                distribution as well, which is exactly the confound the
#                write-up suspects at array size 10 in Experiments 2 and 3.
#   mean_rt      mean response time
# =========================================================

ANOVA_MEASURES <- c(
  slope_fwd    = "OLS slope: report on objective mean",
  slope_rev    = "OLS slope: objective mean on report",
  r            = "Correlation, Fisher z (back-transform for r)",
  bias         = "Mean signed error (report - objective)",
  abs_error    = "Mean absolute error",
  sd_response  = "SD of reports",
  sd_objective = "SD of objective means presented (control)",
  mean_rt      = "Mean response time (ms)"
)

ols_slope <- function(y, x) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(NA_real_)
  vx <- var(x[ok])
  if (!is.finite(vx) || vx == 0) return(NA_real_)
  cov(x[ok], y[ok]) / vx
}

# stage 1: one row per participant x condition x noise level
epoc_cell_measures <- function(dat, exp) {
  noise <- exp$noise_var
  dat %>%
    group_by(across(all_of(c("participant_id", "condition", noise)))) %>%
    summarise(
      n_trials     = n(),
      slope_fwd    = ols_slope(indexSelected, meanVal),
      slope_rev    = ols_slope(meanVal, indexSelected),
      r            = if (n() > 2 && sd(meanVal) > 0 && sd(indexSelected) > 0)
                       cor(meanVal, indexSelected) else NA_real_,
      bias         = mean(indexSelected - meanVal),
      abs_error    = mean(abs(indexSelected - meanVal)),
      sd_response  = sd(indexSelected),
      sd_objective = sd(meanVal),
      mean_rt      = mean(rt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(z = atanh(pmin(pmax(r, -0.999), 0.999)),
           experiment = exp$id, .before = 1)
}

# stage 2: the mixed ANOVA, one measure at a time
epoc_anova_one <- function(cells, exp, measure) {

  noise <- exp$noise_var
  dv <- if (measure == "r") "z" else measure   # correlations are tested on Fisher z

  d <- cells %>% filter(is.finite(.data[[dv]]))
  complete_ids <- d %>%
    count(participant_id) %>%
    filter(n == length(exp$levels)) %>%
    pull(participant_id)
  d <- d %>% filter(participant_id %in% complete_ids)
  if (!nrow(d)) return(NULL)

  two <- nlevels(droplevels(d$condition)) > 1

  fit <- afex::aov_ez(id = "participant_id", dv = dv, data = as.data.frame(d),
                      within = noise, between = if (two) "condition" else NULL,
                      type = 3)

  anova_tab <- as.data.frame(fit$anova_table) %>%
    tibble::rownames_to_column("effect") %>%
    rename(df1 = `num Df`, df2 = `den Df`, F = `F`, p = `Pr(>F)`) %>%
    mutate(experiment = exp$id, measure = measure,
           label = unname(ANOVA_MEASURES[measure]),
           n_participants = length(complete_ids), .before = 1)

  posthoc <- as.data.frame(
    pairs(emmeans(fit, as.formula(paste("~", noise, if (two) "| condition" else ""))),
          adjust = "tukey")) %>%
    mutate(experiment = exp$id, measure = measure, .before = 1)

  between_cond <- if (two)
    as.data.frame(pairs(emmeans(fit, as.formula(paste("~ condition |", noise))),
                        adjust = "tukey")) %>%
      mutate(experiment = exp$id, measure = measure, comparison_set = "condition", .before = 1)
  else NULL

  cell_means <- d %>%
    group_by(across(all_of(c("condition", noise)))) %>%
    summarise(n = n(),
              mean = mean(.data[[dv]]),
              se   = sd(.data[[dv]]) / sqrt(n()),
              .groups = "drop") %>%
    mutate(experiment = exp$id, measure = measure, .before = 1)

  list(anova = anova_tab,
       posthoc = bind_rows(mutate(posthoc, comparison_set = "noise", .before = 1),
                           between_cond),
       means = cell_means,
       fit = fit)
}

epoc_anovas <- function(dat, exp, measures = names(ANOVA_MEASURES)) {

  cells <- epoc_cell_measures(dat, exp)
  res <- lapply(measures, function(m) epoc_anova_one(cells, exp, m))
  names(res) <- measures
  res <- res[!vapply(res, is.null, logical(1))]

  list(
    cells   = cells,
    results = res,
    anova   = bind_rows(lapply(res, `[[`, "anova")),
    posthoc = bind_rows(lapply(res, `[[`, "posthoc")),
    means   = bind_rows(lapply(res, `[[`, "means"))
  )
}

# Do the two-stage slopes agree with the mixed model's slopes?
epoc_anova_vs_lmm <- function(anovas, lmm, exp) {
  noise <- exp$noise_var
  two_stage <- anovas$means %>%
    filter(measure == "slope_fwd") %>%
    select(all_of(c("condition", noise)), two_stage = mean, two_stage_se = se)
  model <- lmm$slopes %>%
    select(any_of(c("condition", noise)), lmm = slope, lmm_se = SE)
  join_by <- intersect(names(two_stage), names(model))
  two_stage %>%
    left_join(model, by = join_by) %>%
    mutate(difference = two_stage - lmm, experiment = exp$id, .before = 1)
}

# --- figures -------------------------------------------------------------
fig_anova_measures <- function(anovas, exp,
                               measures = c("slope_fwd", "slope_rev", "bias",
                                            "abs_error", "sd_response", "sd_objective")) {
  noise <- exp$noise_var
  d <- anovas$means %>%
    filter(measure %in% measures) %>%
    mutate(label = factor(unname(ANOVA_MEASURES[measure]),
                          levels = unname(ANOVA_MEASURES[measures])))
  ggplot(d, aes(x = .data[[noise]], y = mean, colour = condition, group = condition)) +
    geom_line(linewidth = .9) +
    geom_point(size = 2.2) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .12) +
    facet_wrap(~ label, scales = "free_y") +
    scale_colour_manual(values = COND_COLOURS, drop = TRUE) +
    labs(x = exp$noise_label, y = NULL, colour = "Condition",
         title = "Participant-level measures, straight from the trials",
         subtitle = paste0(exp$label, " - means +/- SE, no model involved")) +
    theme_epoc()
}

fig_anova_vs_lmm <- function(cmpr, exp) {
  noise <- exp$noise_var
  d <- cmpr %>%
    tidyr::pivot_longer(c(two_stage, lmm), names_to = "source", values_to = "slope") %>%
    mutate(source = factor(source, levels = c("lmm", "two_stage"),
                           labels = c("mixed model", "per-participant OLS")))
  p <- ggplot(d, aes(x = .data[[noise]], y = slope, colour = source, group = source)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_line(linewidth = .9) +
    geom_point(size = 2.4) +
    scale_colour_manual(values = c("mixed model" = "#2C6E9B",
                                   "per-participant OLS" = "#B08A26")) +
    labs(x = exp$noise_label, y = "Slope of the objective mean", colour = NULL,
         title = "Model-based and model-free slopes agree",
         subtitle = exp$label) +
    theme_epoc()
  if ("condition" %in% names(d)) p <- p + facet_wrap(~ condition)
  p
}

epoc_anova_outputs <- function(anovas, lmm, exp, dir) {
  write.csv(anovas$cells,   file.path(dir, "anova_cell_measures.csv"), row.names = FALSE)
  write.csv(anovas$anova,   file.path(dir, "anova_tests.csv"),         row.names = FALSE)
  write.csv(anovas$posthoc, file.path(dir, "anova_posthoc.csv"),       row.names = FALSE)
  write.csv(anovas$means,   file.path(dir, "anova_cell_means.csv"),    row.names = FALSE)
  cmpr <- epoc_anova_vs_lmm(anovas, lmm, exp)
  write.csv(cmpr,           file.path(dir, "anova_vs_lmm_slopes.csv"), row.names = FALSE)
  epoc_save(fig_anova_measures(anovas, exp), file.path(dir, "fig16_anova_measures.png"),
            width = 9, height = 6)
  epoc_save(fig_anova_vs_lmm(cmpr, exp),     file.path(dir, "fig17_anova_vs_lmm.png"),
            width = 8, height = 4.5)
  invisible(cmpr)
}
