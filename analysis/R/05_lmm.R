# =========================================================
# 05_lmm.R -- Analysis 2: the regression-to-the-mean model
#
#   reported ~ objective_mean_c * noise * condition + (random | participant)
#
# The slope of the (within-participant centred) objective mean is the measure
# of regression to the mean: a slope of 1 means the report tracks the stimulus
# one-to-one, and slopes below 1 mean the reports are compressed towards the
# participant's own mean. The question in every experiment is whether that
# slope shrinks as noise increases, and whether it shrinks differently in the
# experience and average conditions.
#
# Random effects follow a Barr-style ladder: start at the rung named in the
# registry and walk down until a model converges without a singular fit.
# =========================================================

# Rungs, from maximal to minimal
RAND_LADDER <- c("max", "mean_plus_noise", "mean_plus_noise_nocorr",
                 "mean_slope", "noise_slope", "intercept")

# The alternative (RANDOM_MODE == "full") ladder. Every rung keeps a
# by-participant slope on the continuous predictor, and the first three also
# let that slope differ by noise level for each participant, which is the
# random effect that matches the key test (does the slope change with noise?):
#   max                    own intercept, slope, noise shifts and slope x noise, correlated
#   slope_by_level         own intercept + own slope at each noise level (correlated)
#   slope_by_level_nocorr  the same, slopes uncorrelated
#   mean_plus_noise(_nocorr), mean_slope   as in the default ladder
# mean_slope is the floor: if nothing above is clean it is kept regardless.
FULL_LADDER <- c("max", "slope_by_level", "slope_by_level_nocorr",
                 "mean_plus_noise", "mean_plus_noise_nocorr", "mean_slope")

# Reverse models (objective ~ report) in the full ladder carry no
# by-participant intercept or level shifts: the outcome is the objective value,
# and every participant saw practically the same mix of stimuli (their mean
# objective value differs by SD ~0.1), so those variances are zero by design and
# made every rung singular. Only the by-participant slopes remain, so the
# mean_plus_noise rungs collapse into mean_slope and are skipped.
FULL_LADDER_REVERSE <- c("max", "slope_by_level", "slope_by_level_nocorr", "mean_slope")

# Which rungs to try for one experiment, shared by every model-fitting module
epoc_rungs <- function(exp, reverse = FALSE) {
  if (RANDOM_MODE == "full") return(if (reverse) FULL_LADDER_REVERSE else FULL_LADDER)
  if (!is.null(exp$rand_force)) return(exp$rand_force)
  start <- match(exp$rand_start %||% "max", RAND_LADDER)
  RAND_LADDER[start:length(RAND_LADDER)]
}

# `slope_var` is the continuous predictor that carries the by-participant slope:
# the centred objective mean in the forward models, the centred report in the
# reverse (objective ~ subjective) models of 12_reverse_models.R
rand_formula <- function(rung, noise, slope_var = "meanVal_c", levels = NULL, intercept = TRUE) {
  if (!intercept) return(switch(rung,
    max                   = sprintf("(0 + %s + %s:%s | participant_id)", slope_var, slope_var, noise),
    slope_by_level        = sprintf("(0 + %s:%s | participant_id)", slope_var, noise),
    slope_by_level_nocorr = paste(sprintf("(0 + I(%s * (%s == \"%s\")) | participant_id)", slope_var, noise, levels),
                                  collapse = " + "),
    mean_slope            = sprintf("(0 + %s | participant_id)", slope_var),
    stop("rung without intercept not defined: ", rung)))
  switch(rung,
    slope_by_level         = sprintf("(1 | participant_id) + (0 + %s:%s | participant_id)", slope_var, noise),
    # one uncorrelated slope per noise level, built with I() so no extra columns are needed
    slope_by_level_nocorr  = paste(c("(1 | participant_id)",
                                     sprintf("(0 + I(%s * (%s == \"%s\")) | participant_id)", slope_var, noise, levels)),
                                   collapse = " + "),
    max                    = sprintf("(1 + %s * %s | participant_id)", slope_var, noise),
    mean_plus_noise        = sprintf("(1 + %s + %s | participant_id)", slope_var, noise),
    mean_plus_noise_nocorr = sprintf("(1 + %s + %s || participant_id)", slope_var, noise),
    mean_slope             = sprintf("(1 + %s | participant_id)", slope_var),
    noise_slope            = sprintf("(1 + %s | participant_id)", noise),
    intercept              = "(1 | participant_id)",
    stop("unknown random-effects rung: ", rung)
  )
}

# TRUE when the fit converged and is not singular
fit_is_clean <- function(m) {
  ok_conv <- length(m@optinfo$conv$lme4$messages %||% character(0)) == 0
  ok_conv && !lme4::isSingular(m, tol = 1e-4)
}

epoc_lmm <- function(dat, exp, verbose = TRUE) {

  noise <- exp$noise_var
  two_conditions <- nlevels(droplevels(dat$condition)) > 1

  fixed <- if (two_conditions)
    sprintf("indexSelected ~ meanVal_c * %s * condition", noise)
  else
    sprintf("indexSelected ~ meanVal_c * %s", noise)

  # rand_force pins the structure (used where the write-up deliberately keeps a
  # singular but theoretically motivated fit); otherwise walk down the ladder
  rungs <- epoc_rungs(exp)

  model <- NULL; used <- NA_character_; singular <- NA
  attempts <- list()
  for (rung in rungs) {
    f <- as.formula(paste(fixed, "+", rand_formula(rung, noise, levels = levels(dat[[noise]]))))
    if (verbose) message("  fitting random structure: ", rung)
    m <- suppressWarnings(suppressMessages(
      lmerTest::lmer(f, data = dat,
                     control = lmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 2e5)))))
    attempts[[rung]] <- list(singular = lme4::isSingular(m, tol = 1e-4),
                             messages = paste(m@optinfo$conv$lme4$messages %||% "", collapse = "; "))
    model <- m; used <- rung; singular <- attempts[[rung]]$singular
    if (fit_is_clean(m)) break
  }
  if (!fit_is_clean(model) && verbose)
    message("  note: no rung gave a clean fit; keeping '", used, "' (singular = ", singular, ")")

  # ---- fixed effects -------------------------------------------------------
  anova_tab <- as.data.frame(anova(model)) %>%          # Type III, Satterthwaite
    tibble::rownames_to_column("effect") %>%
    rename(df1 = NumDF, df2 = DenDF, F = `F value`, p = `Pr(>F)`) %>%
    mutate(experiment = exp$id, .before = 1)

  coef_tab <- as.data.frame(coef(summary(model))) %>%
    tibble::rownames_to_column("term") %>%
    rename(b = Estimate, SE = `Std. Error`, df = df, t = `t value`, p = `Pr(>|t|)`) %>%
    mutate(experiment = exp$id, .before = 1)

  # ---- slopes of the objective mean in every cell --------------------------
  grid <- if (two_conditions) paste("~ condition *", noise) else paste("~", noise)
  trends <- emtrends(model, as.formula(grid), var = "meanVal_c",
                     lmer.df = "satterthwaite")

  slopes <- as.data.frame(summary(trends, infer = c(TRUE, TRUE))) %>%
    rename(slope = meanVal_c.trend) %>%
    rename_with(~ "ci_low",  any_of(c("lower.CL", "asymp.LCL"))) %>%
    rename_with(~ "ci_high", any_of(c("upper.CL", "asymp.UCL"))) %>%
    mutate(experiment = exp$id, .before = 1) %>%
    # single-condition experiments carry no condition column out of emtrends;
    # name it anyway so downstream joins have something to match on
    { if (two_conditions) . else mutate(., condition = levels(droplevels(dat$condition))[1]) }

  # Does the slope shrink across noise levels, within each condition?
  within_cond <- if (two_conditions)
    as.data.frame(pairs(emtrends(model, as.formula(paste("pairwise ~", noise, "| condition")),
                                 var = "meanVal_c", lmer.df = "satterthwaite")$emtrends,
                        adjust = "tukey"))
  else
    as.data.frame(pairs(trends, adjust = "tukey"))

  # Do the two conditions differ at each noise level?
  between_cond <- if (two_conditions)
    as.data.frame(pairs(emtrends(model, as.formula(paste("pairwise ~ condition |", noise)),
                                 var = "meanVal_c", lmer.df = "satterthwaite")$emtrends,
                        adjust = "tukey"))
  else NULL

  contrasts_tab <- bind_rows(
    mutate(within_cond, comparison_set = "noise_within_condition", .before = 1),
    if (!is.null(between_cond)) mutate(between_cond, comparison_set = "condition_within_noise", .before = 1)
  ) %>% mutate(experiment = exp$id, .before = 1)

  # ---- model-implied lines for the figures ---------------------------------
  terms <- c("meanVal_c [all]", noise)
  if (two_conditions) terms <- c(terms, "condition")
  predictions <- as.data.frame(ggeffects::ggpredict(model, terms = terms)) %>%
    rename(meanVal_c = x, !!noise := group) %>%
    { if (two_conditions) rename(., condition = facet) else . } %>%
    mutate(meanVal = meanVal_c + mean(dat$meanVal, na.rm = TRUE))

  list(
    model        = model,
    formula      = paste(fixed, "+", rand_formula(used, noise, levels = levels(dat[[noise]]))),
    rand_used    = used,
    singular     = singular,
    attempts     = attempts,
    anova        = anova_tab,
    coefficients = coef_tab,
    slopes       = slopes,
    contrasts    = contrasts_tab,
    predictions  = predictions
  )
}
