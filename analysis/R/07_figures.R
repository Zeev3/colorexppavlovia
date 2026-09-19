# =========================================================
# 07_figures.R -- the standard four-figure set produced for every experiment
#
#   fig1_tracking      mean within-participant correlation per noise level
#   fig2_slopes        model-estimated slope of the objective mean per cell,
#                      with 1 = perfect tracking marked
#   fig3_predictions   model-implied report as a function of the objective
#                      mean, one line per noise level, identity line dashed
#   fig4_exaggeration  proportion of exaggerated responses per noise level
# =========================================================

fig_tracking <- function(corr, exp) {
  noise <- exp$noise_var
  ggplot(corr$summary, aes(x = .data[[noise]], y = mean_r,
                           colour = condition, group = condition)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.6) +
    geom_errorbar(aes(ymin = ci_low_r, ymax = ci_high_r), width = .12, linewidth = .6) +
    scale_colour_manual(values = COND_COLOURS, drop = TRUE) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = exp$noise_label, y = "Mean r (objective, reported)",
         colour = "Condition",
         title = "Tracking accuracy",
         subtitle = exp$label) +
    theme_epoc()
}

fig_slopes <- function(lmm, exp) {
  noise <- exp$noise_var
  d <- lmm$slopes
  has_cond <- "condition" %in% names(d)
  p <- ggplot(d, aes(x = .data[[noise]], y = slope,
                     colour = if (has_cond) condition else NULL,
                     group  = if (has_cond) condition else 1)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_line(linewidth = 1) +
    geom_point(size = 2.6) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = .12, linewidth = .6) +
    labs(x = exp$noise_label, y = "Slope of objective mean",
         colour = "Condition",
         title = "Regression to the mean (slope < 1 = compression)",
         subtitle = exp$label) +
    theme_epoc()
  if (has_cond) p <- p + scale_colour_manual(values = COND_COLOURS, drop = TRUE)
  p
}

fig_predictions <- function(lmm, exp) {
  noise <- exp$noise_var
  d <- lmm$predictions
  p <- ggplot(d, aes(x = meanVal, y = predicted,
                     colour = .data[[noise]], fill = .data[[noise]])) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .18, colour = NA) +
    geom_line(linewidth = 1) +
    coord_cartesian(xlim = c(SCALE_MIN, SCALE_MAX), ylim = c(SCALE_MIN, SCALE_MAX)) +
    scale_colour_viridis_d(option = "C", end = .8) +
    scale_fill_viridis_d(option = "C", end = .8) +
    labs(x = "Objective mean of the array", y = "Predicted report",
         colour = exp$noise_label, fill = exp$noise_label,
         title = "Model-implied objective-subjective mapping",
         subtitle = exp$label) +
    theme_epoc()
  if ("condition" %in% names(d)) p <- p + facet_wrap(~ condition)
  p
}

fig_exaggeration <- function(exg, exp) {
  noise <- exp$noise_var
  ggplot(exg$by_participant, aes(x = .data[[noise]], y = prop_exagg)) +
    geom_line(aes(group = participant_id), alpha = .12) +
    geom_point(alpha = .18, size = 1) +
    stat_summary(aes(group = 1), fun = mean, geom = "line", linewidth = 1.1, colour = "black") +
    stat_summary(aes(group = 1), fun.data = mean_se, geom = "errorbar",
                 width = .1, colour = "black") +
    stat_summary(aes(group = 1), fun = mean, geom = "point", size = 2.6, colour = "black") +
    facet_wrap(~ condition) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = exp$noise_label, y = "Proportion of exaggerated responses",
         title = "Exaggeration",
         subtitle = exp$label) +
    theme_epoc()
}

epoc_figures <- function(corr, lmm, exg, exp, dir) {
  epoc_save(fig_tracking(corr, exp),     file.path(dir, "fig1_tracking.png"))
  epoc_save(fig_slopes(lmm, exp),        file.path(dir, "fig2_slopes.png"))
  epoc_save(fig_predictions(lmm, exp),   file.path(dir, "fig3_predictions.png"),
            width = 8, height = 4.5)
  epoc_save(fig_exaggeration(exg, exp),  file.path(dir, "fig4_exaggeration.png"),
            width = 8, height = 4.5)
  invisible(TRUE)
}
