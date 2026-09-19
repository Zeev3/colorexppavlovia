# =========================================================
# 12_reverse_models.R -- the reverse direction: objective ~ subjective
#
#   objective_mean ~ report_c * noise (* condition) + (random | participant)
#
# The forward models (05, 11) ask how the report follows the stimulus. These
# ask Galton's question instead: given what someone reported, what was actually
# out there? It is the direction used for the Goldenberg exploratory analysis
# (meanGroup ~ rating * numberFaces), so running it here puts the colour
# experiments on the same footing, and it is the model behind the reverse
# Galton squeeze diagrams in 10_galton.R.
#
# The two directions are not redundant. Regression to the mean appears as a
# slope below 1 in *both* directions -- that is the whole point of Galton's
# double squeeze -- and the reverse slope is the one that is insensitive to
# measurement noise in the stimulus but sensitive to noise in the response.
# Reporting both is what rules out a pure attenuation artefact.
#
# Everything below mirrors 05_lmm.R / 11_condition_models.R with the roles of
# `meanVal` and `indexSelected` swapped; `resp_c` is the report centred within
# participant.
# =========================================================

# Add the centred report, if it is not there yet
add_resp_c <- function(dat) {
  if ("resp_c" %in% names(dat)) return(dat)
  dat %>%
    group_by(participant_id) %>%
    mutate(resp_c = indexSelected - mean(indexSelected, na.rm = TRUE)) %>%
    ungroup()
}

# One reverse model. `cond = NULL` fits the combined model with condition in it;
# passing a condition name fits that condition on its own.
epoc_reverse_one <- function(dat, exp, cond = NULL, verbose = TRUE) {

  noise <- exp$noise_var
  d <- add_resp_c(dat)
  if (!is.null(cond)) {
    d <- d %>% filter(condition == cond) %>% droplevels()
    contrasts(d[[noise]]) <- contr.sum(nlevels(d[[noise]]))
  }
  two <- is.null(cond) && nlevels(droplevels(d$condition)) > 1

  fixed <- if (two)
    sprintf("meanVal ~ resp_c * %s * condition", noise)
  else
    sprintf("meanVal ~ resp_c * %s", noise)

  rungs <- epoc_rungs(exp, reverse = TRUE)
  rev_int <- RANDOM_MODE != "full"   # see FULL_LADDER_REVERSE in 05_lmm.R

  model <- NULL; used <- NA_character_; singular <- NA
  for (rung in rungs) {
    f <- as.formula(paste(fixed, "+", rand_formula(rung, noise, slope_var = "resp_c", levels = levels(d[[noise]]), intercept = rev_int)))
    if (verbose) message("    [", cond %||% "both conditions", "] random structure: ", rung)
    m <- suppressWarnings(suppressMessages(
      lmerTest::lmer(f, data = d,
                     control = lmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 2e5)))))
    model <- m; used <- rung; singular <- lme4::isSingular(m, tol = 1e-4)
    if (fit_is_clean(m)) break
  }

  tag <- cond %||% "combined"

  anova_tab <- as.data.frame(anova(model)) %>%
    tibble::rownames_to_column("effect") %>%
    rename(df1 = NumDF, df2 = DenDF, F = `F value`, p = `Pr(>F)`) %>%
    mutate(experiment = exp$id, model = tag, .before = 1)

  coef_tab <- as.data.frame(coef(summary(model))) %>%
    tibble::rownames_to_column("term") %>%
    rename(b = Estimate, SE = `Std. Error`, t = `t value`, p = `Pr(>|t|)`) %>%
    mutate(experiment = exp$id, model = tag, .before = 1)

  grid <- if (two) paste("~ condition *", noise) else paste("~", noise)
  trends <- emtrends(model, as.formula(grid), var = "resp_c",
                     lmer.df = "satterthwaite")

  slopes <- as.data.frame(summary(trends, infer = c(TRUE, TRUE))) %>%
    rename(slope = resp_c.trend) %>%
    rename_with(~ "ci_low",  any_of(c("lower.CL", "asymp.LCL"))) %>%
    rename_with(~ "ci_high", any_of(c("upper.CL", "asymp.UCL"))) %>%
    mutate(experiment = exp$id, model = tag, .before = 1) %>%
    { if (!is.null(cond)) mutate(., condition = cond) else . }

  overall <- as.data.frame(summary(
    emtrends(model, ~ 1, var = "resp_c", lmer.df = "satterthwaite"),
    infer = c(TRUE, TRUE))) %>%
    rename(slope = resp_c.trend) %>%
    rename_with(~ "ci_low",  any_of(c("lower.CL", "asymp.LCL"))) %>%
    rename_with(~ "ci_high", any_of(c("upper.CL", "asymp.UCL"))) %>%
    mutate(experiment = exp$id, model = tag, .before = 1)

  within_noise <- if (two)
    as.data.frame(pairs(emtrends(model, as.formula(paste("pairwise ~", noise, "| condition")),
                                 var = "resp_c", lmer.df = "satterthwaite")$emtrends,
                        adjust = "tukey"))
  else
    as.data.frame(pairs(trends, adjust = "tukey"))

  between_cond <- if (two)
    as.data.frame(pairs(emtrends(model, as.formula(paste("pairwise ~ condition |", noise)),
                                 var = "resp_c", lmer.df = "satterthwaite")$emtrends,
                        adjust = "tukey"))
  else NULL

  contrasts_tab <- bind_rows(
    mutate(within_noise, comparison_set = "noise_within_condition", .before = 1),
    if (!is.null(between_cond)) mutate(between_cond, comparison_set = "condition_within_noise", .before = 1)
  ) %>% mutate(experiment = exp$id, model = tag, .before = 1) %>%
    { if (!is.null(cond)) mutate(., condition = cond) else . }

  terms <- c("resp_c [all]", noise)
  if (two) terms <- c(terms, "condition")
  predictions <- as.data.frame(ggeffects::ggpredict(model, terms = terms)) %>%
    rename(resp_c = x, !!noise := group) %>%
    { if (two) rename(., condition = facet) else mutate(., condition = cond) } %>%
    mutate(response_value = resp_c + mean(d$indexSelected, na.rm = TRUE),
           model = tag)

  list(
    model_name   = tag,
    model        = model,
    formula      = paste(fixed, "+", rand_formula(used, noise, slope_var = "resp_c", levels = levels(d[[noise]]), intercept = rev_int)),
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

# Combined reverse model plus one per condition, in the same call
epoc_reverse_models <- function(dat, exp, verbose = TRUE) {

  conds <- condition_order(dat)
  fits <- list()
  if (length(conds) > 1) fits[["combined"]] <- epoc_reverse_one(dat, exp, NULL, verbose)
  for (cond in conds) fits[[cond]] <- epoc_reverse_one(dat, exp, cond, verbose)

  list(
    fits         = fits,
    order        = names(fits),
    anova        = bind_rows(lapply(fits, `[[`, "anova")),
    coefficients = bind_rows(lapply(fits, `[[`, "coefficients")),
    slopes       = bind_rows(lapply(fits, `[[`, "slopes")),
    overall      = bind_rows(lapply(fits, `[[`, "overall")),
    contrasts    = bind_rows(lapply(fits, `[[`, "contrasts")),
    predictions  = bind_rows(lapply(fits, `[[`, "predictions")),
    structures   = tibble::tibble(
      experiment = exp$id,
      model      = names(fits),
      formula    = vapply(fits, `[[`, character(1), "formula"),
      rand_used  = vapply(fits, `[[`, character(1), "rand_used"),
      singular   = vapply(fits, `[[`, logical(1), "singular")
    )
  )
}

# ---------------------------------------------------------
# Figures
# ---------------------------------------------------------

# Slopes from the reverse models, per condition model
fig_reverse_slopes <- function(rev, exp) {
  noise <- exp$noise_var
  d <- rev$slopes %>% filter(model != "combined")
  if (!nrow(d)) d <- rev$slopes
  ggplot(d, aes(x = .data[[noise]], y = slope, colour = model, group = model)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_line(linewidth = 1) +
    geom_point(size = 2.6) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = .12, linewidth = .6) +
    scale_colour_manual(values = COND_COLOURS, drop = TRUE) +
    labs(x = exp$noise_label, y = "Slope of the report on the objective mean",
         colour = "Condition",
         title = "Reverse direction: objective mean predicted from the report",
         subtitle = exp$label) +
    theme_epoc()
}

# Model-implied mapping, response on x and objective mean on y
fig_reverse_predictions <- function(rev, exp) {
  noise <- exp$noise_var
  d <- rev$predictions %>% filter(model != "combined")
  if (!nrow(d)) d <- rev$predictions
  ggplot(d, aes(x = response_value, y = predicted,
                colour = .data[[noise]], fill = .data[[noise]])) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .18, colour = NA) +
    geom_line(linewidth = 1) +
    facet_wrap(~ model) +
    scale_colour_viridis_d(option = "C", end = .8) +
    scale_fill_viridis_d(option = "C", end = .8) +
    coord_cartesian(xlim = c(SCALE_MIN, SCALE_MAX), ylim = c(SCALE_MIN, SCALE_MAX)) +
    labs(x = "Report given", y = "Predicted objective mean",
         colour = exp$noise_label, fill = exp$noise_label,
         title = "Reverse direction: what was really out there, given the report",
         subtitle = exp$label) +
    theme_epoc()
}

# Forward and reverse slopes side by side. Under regression to the mean both
# sit below 1; a slope above 1 in one direction only points at the scaling of
# that variable rather than at compression.
fig_both_directions <- function(lmm, rev, exp) {
  noise <- exp$noise_var
  fwd <- lmm$slopes %>%
    mutate(direction = "report ~ objective mean",
           model = if ("condition" %in% names(.)) as.character(condition) else "all")
  rv <- rev$slopes %>%
    filter(model != "combined") %>%
    mutate(direction = "objective mean ~ report")
  if (!nrow(rv)) rv <- rev$slopes %>% mutate(direction = "objective mean ~ report")

  d <- bind_rows(
    fwd %>% select(all_of(noise), model, slope, ci_low, ci_high, direction),
    rv  %>% select(all_of(noise), model, slope, ci_low, ci_high, direction)
  )

  ggplot(d, aes(x = .data[[noise]], y = slope, colour = direction, group = direction)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_line(linewidth = 1) +
    geom_point(size = 2.4) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = .12, linewidth = .6) +
    facet_wrap(~ model) +
    scale_colour_manual(values = c("report ~ objective mean" = "#2C6E9B",
                                   "objective mean ~ report" = "#B08A26")) +
    labs(x = exp$noise_label, y = "Slope", colour = "Direction",
         title = "Both directions of the objective-subjective relationship",
         subtitle = paste0(exp$label,
                           " - regression to the mean compresses both slopes below 1")) +
    theme_epoc()
}

epoc_reverse_outputs <- function(rev, lmm, exp, dir) {
  write.csv(rev$anova,        file.path(dir, "revlmm_anova.csv"),         row.names = FALSE)
  write.csv(rev$coefficients, file.path(dir, "revlmm_coefficients.csv"),  row.names = FALSE)
  write.csv(rev$slopes,       file.path(dir, "revlmm_slopes.csv"),        row.names = FALSE)
  write.csv(rev$overall,      file.path(dir, "revlmm_overall_slope.csv"), row.names = FALSE)
  write.csv(rev$contrasts,    file.path(dir, "revlmm_contrasts.csv"),     row.names = FALSE)
  write.csv(rev$structures,   file.path(dir, "revlmm_structures.csv"),    row.names = FALSE)
  write.csv(rev$predictions,  file.path(dir, "revlmm_predictions.csv"),   row.names = FALSE)
  capture.output(
    for (f in rev$fits) {
      cat("\n=====", f$model_name, "=====\n")
      cat("formula:", f$formula, "\nsingular:", f$singular, "\n\n")
      print(summary(f$model))
    },
    file = file.path(dir, "revlmm_models.txt")
  )
  epoc_save(fig_reverse_slopes(rev, exp),
            file.path(dir, "fig11_reverse_slopes.png"))
  epoc_save(fig_reverse_predictions(rev, exp),
            file.path(dir, "fig12_reverse_predictions.png"), width = 8, height = 4.5)
  epoc_save(fig_both_directions(lmm, rev, exp),
            file.path(dir, "fig13_both_directions.png"), width = 8, height = 4.5)
  invisible(TRUE)
}
