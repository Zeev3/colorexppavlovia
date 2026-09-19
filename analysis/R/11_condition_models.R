# =========================================================
# 11_condition_models.R -- the within-condition models
#
# The combined model in 05_lmm.R answers "do the two conditions differ?".
# This one answers the question the write-up leads with: within the experience
# condition on its own, does the objective-subjective slope shrink as noise
# increases? The experience condition is the primary model; the average
# condition is fitted the same way as its comparison.
#
#   report ~ objective_mean_c * noise + (random | participant)
#
# fitted separately per condition, same random-effects ladder as the combined
# model. These are the fits behind statements like "in the experience
# condition, b = 0.90, SE = 0.04, t(44.47) = 22.77".
# =========================================================

# Order conditions so the experience condition is always reported first
condition_order <- function(dat) {
  lv <- levels(droplevels(dat$condition))
  c(intersect("experience", lv), setdiff(lv, "experience"))
}

epoc_lmm_one_condition <- function(dat, exp, cond, verbose = TRUE) {

  noise <- exp$noise_var
  d <- dat %>% filter(condition == cond) %>% droplevels()
  # re-apply sum coding after dropping the other condition's rows
  contrasts(d[[noise]]) <- contr.sum(nlevels(d[[noise]]))

  fixed <- sprintf("indexSelected ~ meanVal_c * %s", noise)

  rungs <- epoc_rungs(exp)

  model <- NULL; used <- NA_character_; singular <- NA
  for (rung in rungs) {
    f <- as.formula(paste(fixed, "+", rand_formula(rung, noise, levels = levels(d[[noise]]))))
    if (verbose) message("    [", cond, "] random structure: ", rung)
    m <- suppressWarnings(suppressMessages(
      lmerTest::lmer(f, data = d,
                     control = lmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 2e5)))))
    model <- m; used <- rung; singular <- lme4::isSingular(m, tol = 1e-4)
    if (fit_is_clean(m)) break
  }

  anova_tab <- as.data.frame(anova(model)) %>%
    tibble::rownames_to_column("effect") %>%
    rename(df1 = NumDF, df2 = DenDF, F = `F value`, p = `Pr(>F)`) %>%
    mutate(experiment = exp$id, condition = cond, .before = 1)

  coef_tab <- as.data.frame(coef(summary(model))) %>%
    tibble::rownames_to_column("term") %>%
    rename(b = Estimate, SE = `Std. Error`, t = `t value`, p = `Pr(>|t|)`) %>%
    mutate(experiment = exp$id, condition = cond, .before = 1)

  trends <- emtrends(model, as.formula(paste("~", noise)), var = "meanVal_c",
                     lmer.df = "satterthwaite")

  slopes <- as.data.frame(summary(trends, infer = c(TRUE, TRUE))) %>%
    rename(slope = meanVal_c.trend) %>%
    rename_with(~ "ci_low",  any_of(c("lower.CL", "asymp.LCL"))) %>%
    rename_with(~ "ci_high", any_of(c("upper.CL", "asymp.UCL"))) %>%
    mutate(experiment = exp$id, condition = cond, .before = 1)

  contrasts_tab <- as.data.frame(pairs(trends, adjust = "tukey")) %>%
    mutate(experiment = exp$id, condition = cond, .before = 1)

  # the overall slope, collapsing noise levels: "responses tracked the
  # objective mean, b = ..., t(...) = ..."
  overall <- as.data.frame(summary(
    emtrends(model, ~ 1, var = "meanVal_c", lmer.df = "satterthwaite"),
    infer = c(TRUE, TRUE))) %>%
    rename(slope = meanVal_c.trend) %>%
    rename_with(~ "ci_low",  any_of(c("lower.CL", "asymp.LCL"))) %>%
    rename_with(~ "ci_high", any_of(c("upper.CL", "asymp.UCL"))) %>%
    mutate(experiment = exp$id, condition = cond, .before = 1)

  predictions <- as.data.frame(
    ggeffects::ggpredict(model, terms = c("meanVal_c [all]", noise))) %>%
    rename(meanVal_c = x, !!noise := group) %>%
    mutate(condition = cond,
           meanVal = meanVal_c + mean(d$meanVal, na.rm = TRUE))

  list(
    condition    = cond,
    model        = model,
    formula      = paste(fixed, "+", rand_formula(used, noise, levels = levels(d[[noise]]))),
    rand_used    = used,
    singular     = singular,
    anova        = anova_tab,
    coefficients = coef_tab,
    slopes       = slopes,
    overall      = overall,
    contrasts    = contrasts_tab,
    predictions  = predictions
  )
}

# Fit every condition and bind the tables together
epoc_lmm_by_condition <- function(dat, exp, verbose = TRUE) {
  conds <- condition_order(dat)
  fits <- lapply(conds, function(cond) epoc_lmm_one_condition(dat, exp, cond, verbose))
  names(fits) <- conds
  list(
    fits         = fits,
    order        = conds,
    anova        = bind_rows(lapply(fits, `[[`, "anova")),
    coefficients = bind_rows(lapply(fits, `[[`, "coefficients")),
    slopes       = bind_rows(lapply(fits, `[[`, "slopes")),
    overall      = bind_rows(lapply(fits, `[[`, "overall")),
    contrasts    = bind_rows(lapply(fits, `[[`, "contrasts")),
    predictions  = bind_rows(lapply(fits, `[[`, "predictions")),
    structures   = tibble::tibble(
      experiment = exp$id,
      condition  = conds,
      formula    = vapply(fits, `[[`, character(1), "formula"),
      rand_used  = vapply(fits, `[[`, character(1), "rand_used"),
      singular   = vapply(fits, `[[`, logical(1), "singular")
    )
  )
}

# Slopes from the separate per-condition models, side by side
fig_condition_model_slopes <- function(cm, exp) {
  noise <- exp$noise_var
  ggplot(cm$slopes, aes(x = .data[[noise]], y = slope,
                        colour = condition, group = condition)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_line(linewidth = 1) +
    geom_point(size = 2.6) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = .12, linewidth = .6) +
    scale_colour_manual(values = COND_COLOURS, drop = TRUE) +
    labs(x = exp$noise_label, y = "Slope of objective mean", colour = "Condition",
         title = "Within-condition models fitted separately",
         subtitle = exp$label) +
    theme_epoc()
}

# Model-implied mapping for one condition's own model
fig_condition_model_predictions <- function(cm, exp) {
  noise <- exp$noise_var
  ggplot(cm$predictions, aes(x = meanVal, y = predicted,
                             colour = .data[[noise]], fill = .data[[noise]])) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .18, colour = NA) +
    geom_line(linewidth = 1) +
    facet_wrap(~ condition) +
    scale_colour_viridis_d(option = "C", end = .8) +
    scale_fill_viridis_d(option = "C", end = .8) +
    coord_cartesian(xlim = c(SCALE_MIN, SCALE_MAX), ylim = c(SCALE_MIN, SCALE_MAX)) +
    labs(x = "Objective mean of the array", y = "Predicted report",
         colour = exp$noise_label, fill = exp$noise_label,
         title = "Objective-subjective mapping, each condition modelled on its own",
         subtitle = exp$label) +
    theme_epoc()
}

epoc_condition_model_outputs <- function(cm, exp, dir) {
  write.csv(cm$anova,        file.path(dir, "lmm_bycondition_anova.csv"),        row.names = FALSE)
  write.csv(cm$coefficients, file.path(dir, "lmm_bycondition_coefficients.csv"), row.names = FALSE)
  write.csv(cm$slopes,       file.path(dir, "lmm_bycondition_slopes.csv"),       row.names = FALSE)
  write.csv(cm$overall,      file.path(dir, "lmm_bycondition_overall_slope.csv"),row.names = FALSE)
  write.csv(cm$contrasts,    file.path(dir, "lmm_bycondition_contrasts.csv"),    row.names = FALSE)
  write.csv(cm$structures,   file.path(dir, "lmm_bycondition_structures.csv"),   row.names = FALSE)
  capture.output(
    for (f in cm$fits) {
      cat("\n=====", f$condition, "condition =====\n")
      cat("formula:", f$formula, "\nsingular:", f$singular, "\n\n")
      print(summary(f$model))
    },
    file = file.path(dir, "lmm_bycondition_models.txt")
  )
  epoc_save(fig_condition_model_slopes(cm, exp),
            file.path(dir, "fig9_slopes_by_condition_model.png"))
  epoc_save(fig_condition_model_predictions(cm, exp),
            file.path(dir, "fig10_predictions_by_condition_model.png"),
            width = 8, height = 4.5)
  invisible(TRUE)
}
