# =========================================================
# 14_compare.R -- compare two experiments that share a noise factor
#
# Written for the Experiment 2 vs Experiment 3 question: both used array sizes
# 2/10/20, and Experiment 3 added a Mondrian frame around the array. Pooling
# them lets the frame enter as a factor, so the question "does the Mondrian
# noise increase regression to the mean?" becomes a single interaction term
# rather than an eyeball comparison of two separate write-ups.
#
#   report ~ objective_mean_c * noise * frame (* condition) + (random | participant)
#
# The terms that answer the question:
#   objective_mean x frame          does the frame compress the slope overall
#   objective_mean x noise x frame  does the frame change how the slope falls
#                                   off with array size
#
# IMPORTANT: the frame is between-subjects and between-batches. The two
# experiments were run on different Prolific samples at different times, so a
# frame effect here is not as clean as a within-subject manipulation would be;
# anything found should be read as "the group that saw frames differed", not
# "adding a frame to a given participant does X".
# =========================================================

# Pool two prepared datasets, adding the contrast factor
epoc_pool <- function(prep_a, prep_b, cmp) {

  a <- prep_a$data %>% mutate(.study = cmp$a)
  b <- prep_b$data %>% mutate(.study = cmp$b)

  noise <- EXPERIMENTS[[cmp$a]]$noise_var
  lv_a <- levels(a[[noise]]); lv_b <- levels(b[[noise]])
  if (!identical(lv_a, lv_b))
    stop("cannot compare ", cmp$a, " and ", cmp$b,
         ": noise levels differ (", paste(lv_a, collapse = "/"), " vs ",
         paste(lv_b, collapse = "/"), ")")

  dat <- bind_rows(a, b) %>%
    mutate(
      participant_id = factor(paste(.study, participant_id, sep = ".")),
      frame          = factor(unname(cmp$labels[.study]), levels = unname(cmp$labels)),
      condition      = factor(as.character(condition))
    )

  # sum coding throughout, so the Type-III tests are the ones we want
  contrasts(dat[[noise]]) <- contr.sum(nlevels(dat[[noise]]))
  contrasts(dat$frame)    <- contr.sum(nlevels(dat$frame))
  if (nlevels(dat$condition) > 1) contrasts(dat$condition) <- contr.sum(nlevels(dat$condition))

  # re-centre the objective mean within participant (ids changed above)
  dat %>%
    group_by(participant_id) %>%
    mutate(meanVal_c = meanVal - mean(meanVal, na.rm = TRUE)) %>%
    ungroup()
}

# One pooled model. `cond = NULL` keeps condition in the model; naming a
# condition fits that condition on its own.
epoc_compare_lmm <- function(dat, exp, cmp, cond = NULL, verbose = TRUE) {

  noise <- exp$noise_var
  d <- dat
  if (!is.null(cond)) {
    d <- d %>% filter(condition == cond) %>% droplevels()
    contrasts(d[[noise]]) <- contr.sum(nlevels(d[[noise]]))
    contrasts(d$frame)    <- contr.sum(nlevels(d$frame))
  }
  two <- is.null(cond) && nlevels(droplevels(d$condition)) > 1

  fixed <- if (two)
    sprintf("indexSelected ~ meanVal_c * %s * frame * condition", noise)
  else
    sprintf("indexSelected ~ meanVal_c * %s * frame", noise)

  rungs <- epoc_rungs(exp)

  model <- NULL; used <- NA_character_; singular <- NA
  for (rung in rungs) {
    f <- as.formula(paste(fixed, "+", rand_formula(rung, noise, levels = levels(d[[noise]]))))
    if (verbose) message("    [", cond %||% "both conditions", "] random structure: ", rung)
    m <- suppressWarnings(suppressMessages(
      lmerTest::lmer(f, data = d,
                     control = lmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 2e5)))))
    model <- m; used <- rung; singular <- lme4::isSingular(m, tol = 1e-4)
    if (fit_is_clean(m)) break
  }

  tag <- cond %||% "both conditions"

  anova_tab <- as.data.frame(anova(model)) %>%
    tibble::rownames_to_column("effect") %>%
    rename(df1 = NumDF, df2 = DenDF, F = `F value`, p = `Pr(>F)`) %>%
    mutate(comparison = cmp$id, model = tag, .before = 1)

  grid <- if (two) paste("~ frame * condition *", noise) else paste("~ frame *", noise)
  trends <- emtrends(model, as.formula(grid), var = "meanVal_c",
                     lmer.df = "satterthwaite")

  slopes <- as.data.frame(summary(trends, infer = c(TRUE, TRUE))) %>%
    rename(slope = meanVal_c.trend) %>%
    rename_with(~ "ci_low",  any_of(c("lower.CL", "asymp.LCL"))) %>%
    rename_with(~ "ci_high", any_of(c("upper.CL", "asymp.UCL"))) %>%
    mutate(comparison = cmp$id, model = tag, .before = 1)

  # the contrast that answers the question: frame vs no frame, at each array size
  by_noise <- if (two)
    paste("pairwise ~ frame |", noise, "* condition")
  else
    paste("pairwise ~ frame |", noise)
  frame_at_noise <- as.data.frame(
    emtrends(model, as.formula(by_noise), var = "meanVal_c",
             lmer.df = "satterthwaite")$contrasts) %>%
    mutate(comparison_set = "frame_at_each_noise_level", .before = 1)

  # and averaged over array sizes
  frame_overall <- as.data.frame(
    emtrends(model, if (two) pairwise ~ frame | condition else pairwise ~ frame,
             var = "meanVal_c", lmer.df = "satterthwaite")$contrasts) %>%
    mutate(comparison_set = "frame_overall", .before = 1)

  contrasts_tab <- bind_rows(frame_at_noise, frame_overall) %>%
    mutate(comparison = cmp$id, model = tag, .before = 1)

  list(model = model,
       model_name = tag,
       formula = paste(fixed, "+", rand_formula(used, noise, levels = levels(d[[noise]]))),
       rand_used = used, singular = singular,
       anova = anova_tab, slopes = slopes, contrasts = contrasts_tab)
}

# Tracking correlations with the frame as a second between-subjects factor
epoc_compare_correlations <- function(dat, exp, cmp) {

  noise <- exp$noise_var

  by_participant <- dat %>%
    group_by(across(all_of(c("participant_id", "condition", "frame", noise)))) %>%
    summarise(
      n_trials = n(),
      r = if (n() > 2 && sd(meanVal) > 0 && sd(indexSelected) > 0)
            cor(meanVal, indexSelected) else NA_real_,
      .groups = "drop") %>%
    mutate(z = atanh(pmin(pmax(r, -0.999), 0.999)))

  complete_ids <- by_participant %>%
    filter(!is.na(z)) %>%
    count(participant_id) %>%
    filter(n == nlevels(dat[[noise]])) %>%
    pull(participant_id)

  aov_dat <- by_participant %>% filter(participant_id %in% complete_ids)
  between <- c("frame", if (nlevels(droplevels(aov_dat$condition)) > 1) "condition")

  fit <- afex::aov_ez(id = "participant_id", dv = "z",
                      data = as.data.frame(aov_dat),
                      within = noise, between = between, type = 3)

  anova_tab <- as.data.frame(fit$anova_table) %>%
    tibble::rownames_to_column("effect") %>%
    rename(df1 = `num Df`, df2 = `den Df`, F = `F`, p = `Pr(>F)`) %>%
    mutate(comparison = cmp$id, analysis = "tracking_correlation", .before = 1)

  posthoc <- as.data.frame(
    pairs(emmeans(fit, as.formula(paste("~ frame |", noise))), adjust = "tukey")) %>%
    mutate(comparison = cmp$id, .before = 1)

  summary_tab <- by_participant %>%
    group_by(across(all_of(c("condition", "frame", noise)))) %>%
    summarise(n = sum(!is.na(z)),
              mean_z = mean(z, na.rm = TRUE),
              se_z = sd(z, na.rm = TRUE) / sqrt(sum(!is.na(z))),
              mean_r = tanh(mean_z),
              ci_low_r = tanh(mean_z - 1.96 * se_z),
              ci_high_r = tanh(mean_z + 1.96 * se_z),
              .groups = "drop") %>%
    mutate(comparison = cmp$id, .before = 1)

  list(by_participant = by_participant, summary = summary_tab,
       anova = anova_tab, posthoc = posthoc, fit = fit)
}

# --- figures -------------------------------------------------------------
fig_compare_slopes <- function(res, exp, cmp) {
  noise <- exp$noise_var
  d <- res$lmm_by_condition$slopes
  ggplot(d, aes(x = .data[[noise]], y = slope, colour = frame, group = frame)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_line(linewidth = 1) +
    geom_point(size = 2.6) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = .12, linewidth = .6) +
    facet_wrap(~ model) +
    scale_colour_manual(values = setNames(c("#2C6E9B", "#7B4EA8"), unname(cmp$labels))) +
    labs(x = exp$noise_label, y = "Slope of the objective mean", colour = NULL,
         title = cmp$label,
         subtitle = "Lower slope = more compression; bars are 95% CIs") +
    theme_epoc()
}

fig_compare_tracking <- function(res, exp, cmp) {
  noise <- exp$noise_var
  ggplot(res$corr$summary,
         aes(x = .data[[noise]], y = mean_r, colour = frame, group = frame)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.6) +
    geom_errorbar(aes(ymin = ci_low_r, ymax = ci_high_r), width = .12, linewidth = .6) +
    facet_wrap(~ condition) +
    scale_colour_manual(values = setNames(c("#2C6E9B", "#7B4EA8"), unname(cmp$labels))) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = exp$noise_label, y = "Mean r (objective, reported)", colour = NULL,
         title = "Tracking accuracy",
         subtitle = cmp$label) +
    theme_epoc()
}

# --- driver --------------------------------------------------------------
epoc_compare <- function(cmp, source = "recompute", verbose = TRUE) {

  exp_a <- EXPERIMENTS[[cmp$a]]
  exp_b <- EXPERIMENTS[[cmp$b]]
  message("\n=== ", cmp$label, " ===")

  prep_a <- epoc_prepare(exp_a, source = source)
  prep_b <- epoc_prepare(exp_b, source = source)
  dat <- epoc_pool(prep_a, prep_b, cmp)

  exp <- exp_a   # the two share noise_var / levels / labels

  desc <- dat %>%
    group_by(frame, condition) %>%
    summarise(n_participants = n_distinct(participant_id), n_trials = n(),
              .groups = "drop") %>%
    mutate(comparison = cmp$id, .before = 1)
  print(desc)

  message("  pooled model")
  lmm_all <- epoc_compare_lmm(dat, exp, cmp, cond = NULL, verbose = verbose)

  message("  pooled model within each condition")
  conds <- condition_order(dat)
  fits <- lapply(conds, function(cd) epoc_compare_lmm(dat, exp, cmp, cond = cd, verbose = verbose))
  names(fits) <- conds
  lmm_by_condition <- list(
    fits      = fits,
    order     = conds,
    anova     = bind_rows(lapply(fits, `[[`, "anova")),
    slopes    = bind_rows(lapply(fits, `[[`, "slopes")),
    contrasts = bind_rows(lapply(fits, `[[`, "contrasts")),
    structures = tibble::tibble(
      comparison = cmp$id, model = conds,
      formula   = vapply(fits, `[[`, character(1), "formula"),
      rand_used = vapply(fits, `[[`, character(1), "rand_used"),
      singular  = vapply(fits, `[[`, logical(1), "singular"))
  )

  message("  tracking correlations")
  corr <- epoc_compare_correlations(dat, exp, cmp)

  res <- list(cmp = cmp, exp = exp, data = dat, descriptives = desc,
              lmm_all = lmm_all, lmm_by_condition = lmm_by_condition, corr = corr)

  dir <- file.path(OUT, cmp$id)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  write.csv(desc,                          file.path(dir, "descriptives.csv"),        row.names = FALSE)
  write.csv(lmm_all$anova,                 file.path(dir, "lmm_pooled_anova.csv"),     row.names = FALSE)
  write.csv(lmm_all$slopes,                file.path(dir, "lmm_pooled_slopes.csv"),    row.names = FALSE)
  write.csv(lmm_all$contrasts,             file.path(dir, "lmm_pooled_contrasts.csv"), row.names = FALSE)
  write.csv(lmm_by_condition$anova,        file.path(dir, "lmm_bycondition_anova.csv"),     row.names = FALSE)
  write.csv(lmm_by_condition$slopes,       file.path(dir, "lmm_bycondition_slopes.csv"),    row.names = FALSE)
  write.csv(lmm_by_condition$contrasts,    file.path(dir, "lmm_bycondition_contrasts.csv"), row.names = FALSE)
  write.csv(lmm_by_condition$structures,   file.path(dir, "lmm_structures.csv"),       row.names = FALSE)
  write.csv(corr$summary,                  file.path(dir, "correlations_summary.csv"), row.names = FALSE)
  write.csv(corr$anova,                    file.path(dir, "correlations_anova.csv"),   row.names = FALSE)
  write.csv(corr$posthoc,                  file.path(dir, "correlations_posthoc.csv"), row.names = FALSE)
  capture.output(
    { cat("pooled model:", lmm_all$formula, "\nsingular:", lmm_all$singular, "\n\n")
      print(summary(lmm_all$model))
      for (f in fits) {
        cat("\n=====", f$model_name, "=====\n")
        cat("formula:", f$formula, "\nsingular:", f$singular, "\n\n")
        print(summary(f$model))
      } },
    file = file.path(dir, "lmm_models.txt"))

  epoc_save(fig_compare_slopes(res, exp, cmp),   file.path(dir, "fig1_slopes_by_frame.png"),
            width = 8, height = 4.5)
  epoc_save(fig_compare_tracking(res, exp, cmp), file.path(dir, "fig2_tracking_by_frame.png"),
            width = 8, height = 4.5)

  epoc_compare_report(res, file.path(dir, "results.md"))
  res
}

epoc_compare_report <- function(res, path) {

  cmp   <- res$cmp
  exp   <- res$exp
  noise <- exp$noise_var
  nice  <- exp$noise
  con   <- file(path, open = "wt"); on.exit(close(con))
  w     <- function(...) log_line(con, ...)

  w("# ", cmp$label)
  w("")
  w("_Generated by analysis/run_all.R on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "._")
  w("")
  w("`", EXPERIMENTS[[cmp$a]]$label, "` vs `", EXPERIMENTS[[cmp$b]]$label,
    "`, pooled so that ", cmp$factor, " enters as a factor. Both used the same ",
    nice, " levels, so the frame is the only design difference.")
  w("")
  w("**Caveat:** the frame is between-subjects *and* between-batches - the two ",
    "experiments were run on different samples at different times. A difference here ",
    "means the group that saw frames differed, not that adding a frame to a given ",
    "participant does this.")
  w("")

  d <- res$descriptives
  for (i in seq_len(nrow(d)))
    w("- ", chr(d$frame[i]), ", ", chr(d$condition[i]), ": N = ", d$n_participants[i],
      ", ", d$n_trials[i], " trials.")
  w("")

  w("## Does the frame add regression to the mean?")
  w("")
  la <- res$lmm_all$anova
  w("Pooled model: `", res$lmm_all$formula, "`")
  w("")
  w("- Objective mean x frame: ", f_line(get_effect(la, "meanVal_c:frame")), ".")
  w("- Objective mean x ", nice, " x frame: ",
    f_line(get_effect(la, paste0("meanVal_c:", noise, ":frame"))), ".")
  w("- Objective mean x frame x condition: ",
    f_line(get_effect(la, "meanVal_c:frame:condition")), ".")
  w("- Objective mean x ", nice, " x frame x condition: ",
    f_line(get_effect(la, paste0("meanVal_c:", noise, ":frame:condition"))), ".")
  w("")

  ov <- res$lmm_all$contrasts %>% filter(comparison_set == "frame_overall")
  if (nrow(ov)) {
    w("Frame effect on the slope, averaged over ", nice, ":")
    w("")
    for (i in seq_len(nrow(ov))) {
      lab <- if ("condition" %in% names(ov)) paste0(chr(ov$condition[i]), ": ") else ""
      w("- ", lab, chr(ov$contrast[i]), " = ", fmt_num(ov$estimate[i], 3),
        ", SE = ", fmt_num(ov$SE[i], 3), ", ", fmt_p(ov$p.value[i]),
        " (positive = the frame lowered the slope).")
    }
    w("")
  }

  w("## Slopes")
  w("")
  for (mdl in res$lmm_by_condition$order) {
    sl <- res$lmm_by_condition$slopes %>% filter(model == mdl)
    an <- res$lmm_by_condition$anova  %>% filter(model == mdl)
    ct <- res$lmm_by_condition$contrasts %>%
      filter(model == mdl, comparison_set == "frame_at_each_noise_level")
    w("### ", mdl, " condition")
    w("")
    w("- Objective mean x frame: ", f_line(get_effect(an, "meanVal_c:frame")), ".")
    w("- Objective mean x ", nice, " x frame: ",
      f_line(get_effect(an, paste0("meanVal_c:", noise, ":frame"))), ".")
    w("")
    for (i in seq_len(nrow(sl)))
      w("- ", chr(sl$frame[i]), ", ", nice, " ", chr(sl[[noise]][i]), ": b = ",
        fmt_num(sl$slope[i], 3), ", SE = ", fmt_num(sl$SE[i], 3),
        ", 95% CI [", fmt_num(sl$ci_low[i], 3), ", ", fmt_num(sl$ci_high[i], 3), "].")
    w("")
    if (nrow(ct)) {
      w("Frame vs no frame at each ", nice, ":")
      w("")
      for (i in seq_len(nrow(ct)))
        w("- ", nice, " ", chr(ct[[noise]][i]), ": ", chr(ct$contrast[i]), " = ",
          fmt_num(ct$estimate[i], 3), ", SE = ", fmt_num(ct$SE[i], 3), ", ",
          fmt_p(ct$p.value[i]), ".")
      w("")
    }
  }

  w("## Tracking accuracy")
  w("")
  cs <- res$corr$summary
  for (i in seq_len(nrow(cs)))
    w("- ", chr(cs$condition[i]), ", ", chr(cs$frame[i]), ", ", nice, " ",
      chr(cs[[noise]][i]), ": mean r = ", fmt_num(cs$mean_r[i], 2), ".")
  w("")
  ca <- res$corr$anova
  w("- Main effect of frame: ", f_line(get_effect(ca, "frame")), ".")
  w("- ", nice, " x frame: ", f_line(get_effect(ca, paste0("frame:", noise))), ".")
  w("- frame x condition: ", f_line(get_effect(ca, "frame:condition")), ".")
  w("")
  ph <- res$corr$posthoc
  if (nrow(ph)) {
    w("Frame vs no frame at each ", nice, " (Tukey-adjusted):")
    w("")
    for (i in seq_len(nrow(ph)))
      w("- ", nice, " ", chr(ph[[noise]][i]), ": ", chr(ph$contrast[i]), " = ",
        fmt_num(ph$estimate[i], 3), ", ", fmt_p(ph$p.value[i]), ".")
    w("")
  }

  w("## Files")
  w("")
  w("`lmm_pooled_*.csv`, `lmm_bycondition_*.csv`, `correlations_*.csv`, ",
    "`fig1_slopes_by_frame.png`, `fig2_tracking_by_frame.png`.")
  invisible(path)
}
