# =========================================================
# 13_tails.R -- what lies behind an extreme report
#
# The reverse models in 12_reverse_models.R give the slope over the whole
# response range. This module asks the same question at the tails, where the
# exaggeration measure lives: when someone gives an extreme report, how extreme
# was the stimulus, and does that change with noise?
#
# Two framings, because they answer different questions and can disagree:
#
#   fixed     trials whose report crosses a fixed cutoff (the exaggeration
#             thresholds, < 14 and > 36). Conditions on an absolute response
#             value, so it moves with any overall compression of the response
#             distribution: as reports contract, a fixed cutoff sits further
#             out in the tail and picks up less extreme stimuli.
#
#   matched   the most extreme `tail_prop` of responses *within each cell*
#             (default 10% per side), measured as deviations from the
#             participant's own mean. The selection rate is matched across
#             noise levels, so this asks whether a given percentile of the
#             response distribution is still as diagnostic as it was. The
#             headline number is the ratio |objective deviation| /
#             |response deviation|: how much real signal backs a unit of
#             reported extremity.
#
# A drop in the fixed version with a flat matched ratio means the tails moved
# because the whole distribution compressed, not because extreme reports became
# less informative.
# =========================================================

# --- fixed cutoff -------------------------------------------------------
epoc_tails_fixed <- function(dat, exp) {

  noise <- exp$noise_var
  two <- nlevels(droplevels(dat$condition)) > 1

  d <- dat %>%
    mutate(tail = case_when(
      indexSelected < EXAGG_LOW_RESP  ~ "low",
      indexSelected > EXAGG_HIGH_RESP ~ "high",
      TRUE ~ NA_character_)) %>%
    filter(!is.na(tail))

  summary_tab <- d %>%
    group_by(across(all_of(c("tail", "condition", noise)))) %>%
    summarise(n_trials     = n(),
              mean_report  = mean(indexSelected),
              mean_obj     = mean(meanVal),
              se_obj       = sd(meanVal) / sqrt(n()),
              .groups = "drop") %>%
    mutate(experiment = exp$id, .before = 1)

  # one model per tail: is the objective mean behind these reports pushed
  # towards the centre as noise increases?
  fits <- list(); anovas <- list(); emms <- list(); ctr <- list()
  for (tl in intersect(c("low", "high"), unique(d$tail))) {
    s <- d %>% filter(tail == tl) %>% droplevels()
    if (nlevels(s[[noise]]) < 2) next
    contrasts(s[[noise]]) <- contr.sum(nlevels(s[[noise]]))
    f <- as.formula(paste("meanVal ~", noise,
                          if (two) "* condition" else "", "+ (1 | participant_id)"))
    m <- suppressWarnings(suppressMessages(
      lmerTest::lmer(f, data = s,
                     control = lmerControl(optimizer = "bobyqa",
                                           optCtrl = list(maxfun = 2e5)))))
    fits[[tl]] <- m

    anovas[[tl]] <- as.data.frame(anova(m)) %>%
      tibble::rownames_to_column("effect") %>%
      rename(df1 = NumDF, df2 = DenDF, F = `F value`, p = `Pr(>F)`) %>%
      mutate(experiment = exp$id, tail = tl, .before = 1)

    em <- emmeans(m, as.formula(paste("~", noise)), lmer.df = "satterthwaite")
    emms[[tl]] <- as.data.frame(summary(em, infer = c(TRUE, TRUE))) %>%
      rename_with(~ "ci_low",  any_of(c("lower.CL", "asymp.LCL"))) %>%
      rename_with(~ "ci_high", any_of(c("upper.CL", "asymp.UCL"))) %>%
      mutate(experiment = exp$id, tail = tl, .before = 1)
    ctr[[tl]] <- as.data.frame(pairs(em, adjust = "tukey")) %>%
      mutate(experiment = exp$id, tail = tl, .before = 1)
  }

  # width of the window an extreme report brackets: high tail minus low tail
  window <- NULL
  if (length(emms) == 2) {
    window <- emms$high %>%
      select(all_of(noise), high = emmean) %>%
      left_join(emms$low %>% select(all_of(noise), low = emmean), by = noise) %>%
      mutate(window = high - low, experiment = exp$id, .before = 1)
  }

  list(summary = summary_tab,
       anova   = bind_rows(anovas),
       means   = bind_rows(emms),
       contrasts = bind_rows(ctr),
       window  = window,
       fits    = fits)
}

# --- quantile-matched tails ---------------------------------------------
epoc_tails_matched <- function(dat, exp, tail_prop = 0.10) {

  noise <- exp$noise_var

  d <- add_resp_c(dat) %>%
    group_by(participant_id) %>%
    mutate(obj_c = meanVal - mean(meanVal, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(across(all_of(c("condition", noise)))) %>%
    mutate(lo_cut = quantile(resp_c, tail_prop, na.rm = TRUE),
           hi_cut = quantile(resp_c, 1 - tail_prop, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(resp_c <= lo_cut | resp_c >= hi_cut)

  by_trial <- d %>%
    group_by(across(all_of(c("condition", noise)))) %>%
    summarise(
      n_trials      = n(),
      mean_abs_resp = mean(abs(resp_c)),
      mean_abs_obj  = mean(abs(obj_c)),
      ratio         = mean_abs_obj / mean_abs_resp,
      .groups = "drop"
    ) %>%
    mutate(experiment = exp$id, tail_prop = tail_prop, .before = 1)

  # participant-level ratios, so the ratio can be tested rather than eyeballed
  by_participant <- d %>%
    group_by(across(all_of(c("participant_id", "condition", noise)))) %>%
    summarise(n_trials      = n(),
              mean_abs_resp = mean(abs(resp_c)),
              mean_abs_obj  = mean(abs(obj_c)),
              ratio         = mean_abs_obj / mean_abs_resp,
              .groups = "drop")

  complete_ids <- by_participant %>%
    filter(is.finite(ratio)) %>%
    count(participant_id) %>%
    filter(n == length(exp$levels)) %>%
    pull(participant_id)

  aov_dat <- by_participant %>% filter(participant_id %in% complete_ids)
  two <- nlevels(droplevels(aov_dat$condition)) > 1

  fit <- afex::aov_ez(id = "participant_id", dv = "ratio",
                      data = as.data.frame(aov_dat), within = noise,
                      between = if (two) "condition" else NULL, type = 3)

  anova_tab <- as.data.frame(fit$anova_table) %>%
    tibble::rownames_to_column("effect") %>%
    rename(df1 = `num Df`, df2 = `den Df`, F = `F`, p = `Pr(>F)`) %>%
    mutate(experiment = exp$id, analysis = "matched_tail_ratio", .before = 1)

  posthoc <- as.data.frame(pairs(emmeans(fit, as.formula(paste("~", noise))),
                                 adjust = "tukey")) %>%
    mutate(experiment = exp$id, .before = 1)

  list(summary = by_trial, by_participant = by_participant,
       anova = anova_tab, posthoc = posthoc, fit = fit)
}

# --- symmetric report windows -------------------------------------------
# The version of the question stated in words: "when someone reported 40, what
# was actually out there, and does that change with noise?" Two narrow windows
# placed symmetrically about the scale centre, so the low and high tails are
# treated alike and neither depends on where the response distribution happens
# to sit. This is the sensitive form of the reverse-slope question -- the global
# reverse slope dilutes it by averaging over the middle of the range, where
# nothing moves.
REPORT_WINDOW_CENTRES   <- c(low = 10, high = 40)
REPORT_WINDOW_HALFWIDTH <- 2
MIN_WINDOW_TRIALS       <- 20   # per condition x level, else the window is skipped

epoc_tails_windows <- function(dat, exp,
                               centres = REPORT_WINDOW_CENTRES,
                               halfwidth = REPORT_WINDOW_HALFWIDTH) {

  noise <- exp$noise_var
  two   <- nlevels(droplevels(dat$condition)) > 1

  d <- dat %>%
    mutate(window = case_when(
      abs(indexSelected - centres[["low"]])  <= halfwidth ~ "low",
      abs(indexSelected - centres[["high"]]) <= halfwidth ~ "high",
      TRUE ~ NA_character_)) %>%
    filter(!is.na(window))

  summary_tab <- d %>%
    group_by(across(all_of(c("window", "condition", noise)))) %>%
    summarise(n_trials = n(),
              mean_report = mean(indexSelected),
              mean_obj = mean(meanVal),
              se_obj = sd(meanVal) / sqrt(n()),
              .groups = "drop") %>%
    mutate(experiment = exp$id, .before = 1)

  anovas <- list(); emms <- list(); ctr <- list(); coverage <- list()
  for (wd in intersect(c("low", "high"), unique(d$window))) {
    s <- d %>% filter(window == wd) %>% droplevels()
    enough <- s %>% count(across(all_of(c("condition", noise)))) %>% pull(n)
    coverage[[wd]] <- tibble::tibble(experiment = exp$id, window = wd,
                                     min_cell_n = min(enough),
                                     fitted = !(nlevels(s[[noise]]) < 2 ||
                                                any(enough < MIN_WINDOW_TRIALS)))
    if (nlevels(s[[noise]]) < 2 || any(enough < MIN_WINDOW_TRIALS)) next
    contrasts(s[[noise]]) <- contr.sum(nlevels(s[[noise]]))
    f <- as.formula(paste("meanVal ~", noise, if (two) "* condition" else "",
                          "+ (1 | participant_id)"))
    m <- suppressWarnings(suppressMessages(
      lmerTest::lmer(f, data = s, control = lmerControl(optimizer = "bobyqa"))))

    anovas[[wd]] <- as.data.frame(anova(m)) %>%
      tibble::rownames_to_column("effect") %>%
      rename(df1 = NumDF, df2 = DenDF, F = `F value`, p = `Pr(>F)`) %>%
      mutate(experiment = exp$id, window = wd, .before = 1)

    em <- emmeans(m, as.formula(paste("~", noise)), lmer.df = "satterthwaite")
    emms[[wd]] <- as.data.frame(summary(em, infer = c(TRUE, TRUE))) %>%
      rename_with(~ "ci_low",  any_of(c("lower.CL", "asymp.LCL"))) %>%
      rename_with(~ "ci_high", any_of(c("upper.CL", "asymp.UCL"))) %>%
      mutate(experiment = exp$id, window = wd, .before = 1)
    ctr[[wd]] <- as.data.frame(pairs(em, adjust = "tukey")) %>%
      mutate(experiment = exp$id, window = wd, .before = 1)
  }

  # how much reality a fixed span of reports brackets, per noise level
  width <- NULL
  if (length(emms) == 2) {
    width <- emms$high %>%
      select(all_of(noise), high = emmean, se_high = SE) %>%
      left_join(emms$low %>% select(all_of(noise), low = emmean, se_low = SE), by = noise) %>%
      mutate(width = high - low,
             # the two windows are disjoint sets of trials, so their errors add
             se_width = sqrt(se_high^2 + se_low^2),
             report_span = unname(diff(centres)),
             experiment = exp$id, .before = 1)
  }

  list(summary = summary_tab,
       anova = bind_rows(anovas),
       means = bind_rows(emms),
       contrasts = bind_rows(ctr),
       width = width,
       coverage = bind_rows(coverage),
       centres = centres, halfwidth = halfwidth,
       min_trials = MIN_WINDOW_TRIALS)
}

epoc_tails <- function(dat, exp, tail_prop = 0.10) {
  list(fixed   = epoc_tails_fixed(dat, exp),
       matched = epoc_tails_matched(dat, exp, tail_prop),
       windows = epoc_tails_windows(dat, exp))
}

# --- figures -------------------------------------------------------------
fig_tails_fixed <- function(tails, exp) {
  noise <- exp$noise_var
  d <- tails$fixed$means
  mid <- (SCALE_MIN + SCALE_MAX) / 2
  ggplot(d, aes(x = .data[[noise]], y = emmean, colour = tail, group = tail)) +
    geom_hline(yintercept = mid, linetype = "dotted", colour = "grey50") +
    geom_line(linewidth = 1) +
    geom_point(size = 2.6) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = .12, linewidth = .6) +
    scale_colour_manual(values = c(low = "#2C6E9B", high = "#C0563B"),
                        labels = c(low = paste0("reports < ", EXAGG_LOW_RESP),
                                   high = paste0("reports > ", EXAGG_HIGH_RESP))) +
    labs(x = exp$noise_label, y = "Objective mean behind those reports",
         colour = NULL,
         title = "What lies behind an extreme report",
         subtitle = paste0(exp$label, " - closing on the centre = less extreme stimuli")) +
    theme_epoc()
}

fig_tails_matched <- function(tails, exp) {
  noise <- exp$noise_var
  ggplot(tails$matched$by_participant,
         aes(x = .data[[noise]], y = ratio)) +
    geom_line(aes(group = participant_id), alpha = .12) +
    geom_point(alpha = .18, size = 1) +
    stat_summary(aes(group = 1), fun = mean, geom = "line", linewidth = 1.1, colour = "black") +
    stat_summary(aes(group = 1), fun.data = mean_se, geom = "errorbar",
                 width = .1, colour = "black") +
    stat_summary(aes(group = 1), fun = mean, geom = "point", size = 2.6, colour = "black") +
    facet_wrap(~ condition) +
    labs(x = exp$noise_label,
         y = "|objective deviation| / |response deviation|",
         title = "Diagnosticity of the most extreme responses",
         subtitle = paste0(exp$label, " - top and bottom ",
                           round(tails$matched$summary$tail_prop[1] * 100),
                           "% of responses within each cell")) +
    theme_epoc()
}

fig_tails_windows <- function(tails, exp) {
  noise <- exp$noise_var
  d <- tails$windows$means
  if (is.null(d) || !nrow(d)) return(NULL)
  ce <- tails$windows$centres
  mid <- (SCALE_MIN + SCALE_MAX) / 2
  ggplot(d, aes(x = .data[[noise]], y = emmean, colour = window, group = window)) +
    geom_hline(yintercept = mid, linetype = "dotted", colour = "grey50") +
    geom_line(linewidth = 1) +
    geom_point(size = 2.6) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = .12, linewidth = .6) +
    scale_colour_manual(values = c(low = "#2C6E9B", high = "#C0563B"),
                        labels = c(low  = sprintf("reported ~%g", ce[["low"]]),
                                   high = sprintf("reported ~%g", ce[["high"]]))) +
    labs(x = exp$noise_label, y = "Objective mean behind those reports", colour = NULL,
         title = sprintf("What was out there when someone reported ~%g or ~%g",
                         ce[["low"]], ce[["high"]]),
         subtitle = paste0(exp$label, " - lines closing = reports back less extreme stimuli")) +
    theme_epoc()
}

epoc_tail_outputs <- function(tails, exp, dir) {
  write.csv(tails$fixed$summary,   file.path(dir, "tails_fixed_summary.csv"),   row.names = FALSE)
  write.csv(tails$fixed$means,     file.path(dir, "tails_fixed_means.csv"),     row.names = FALSE)
  write.csv(tails$fixed$anova,     file.path(dir, "tails_fixed_anova.csv"),     row.names = FALSE)
  write.csv(tails$fixed$contrasts, file.path(dir, "tails_fixed_contrasts.csv"), row.names = FALSE)
  if (!is.null(tails$fixed$window))
    write.csv(tails$fixed$window,  file.path(dir, "tails_fixed_window.csv"),    row.names = FALSE)
  write.csv(tails$matched$summary,        file.path(dir, "tails_matched_summary.csv"),        row.names = FALSE)
  write.csv(tails$matched$by_participant, file.path(dir, "tails_matched_by_participant.csv"), row.names = FALSE)
  write.csv(tails$matched$anova,          file.path(dir, "tails_matched_anova.csv"),          row.names = FALSE)
  write.csv(tails$matched$posthoc,        file.path(dir, "tails_matched_posthoc.csv"),        row.names = FALSE)
  wn <- tails$windows
  write.csv(wn$summary,   file.path(dir, "tails_windows_summary.csv"),   row.names = FALSE)
  if (nrow(wn$anova))     write.csv(wn$anova,     file.path(dir, "tails_windows_anova.csv"),     row.names = FALSE)
  if (nrow(wn$means))     write.csv(wn$means,     file.path(dir, "tails_windows_means.csv"),     row.names = FALSE)
  if (nrow(wn$contrasts)) write.csv(wn$contrasts, file.path(dir, "tails_windows_contrasts.csv"), row.names = FALSE)
  if (!is.null(wn$width)) write.csv(wn$width,     file.path(dir, "tails_windows_width.csv"),     row.names = FALSE)
  if (nrow(wn$coverage))  write.csv(wn$coverage,  file.path(dir, "tails_windows_coverage.csv"),  row.names = FALSE)
  fw <- fig_tails_windows(tails, exp)
  if (!is.null(fw)) epoc_save(fw, file.path(dir, "fig18_report_windows.png"))
  epoc_save(fig_tails_fixed(tails, exp),   file.path(dir, "fig14_tails_fixed.png"))
  epoc_save(fig_tails_matched(tails, exp), file.path(dir, "fig15_tails_matched.png"),
            width = 8, height = 4.5)
  invisible(TRUE)
}
