# =========================================================
# 17_compression.R -- Analysis 4: centre-directed compression
#
# A model-free companion to the slope. For trials whose objective mean sits
# outside the middle band (below EXAGG_LOW_MEAN or above EXAGG_HIGH_MEAN) we
# measure how far the report was pulled TOWARDS the centre of the scale:
#
#   inward =  report - objective   on low trials
#   inward =  objective - report   on high trials
#
# so a positive value always means compression and a negative value means the
# report overshot outwards. The two sides are then averaged WITHIN each
# participant x noise level: any additive response bias enters the low and the
# high side with opposite signs and cancels. That cancellation is the whole
# point of the measure -- these data carry a downward response bias of about
# 1.2 scale units at the lowest variance level, which is large enough to
# reverse the sign of a naive |report - objective| comparison.
#
# Two things this measure is NOT:
#   - it is not independent of the slope (it is a two-point discretisation of
#     it, and recovers a near-identical generalised eta squared). Report it as
#     a plain-language restatement in scale units, not as separate evidence.
#   - it is not the threshold count in 06_exaggeration.R. That one is a
#     proportion of ALL retained trials, roughly half of which can never
#     qualify; this one needs no cutoff on the response and no denominator.
#
# The band analysis (`$band`) answers the adjacent question directly: is the
# pull larger on extreme trials than on middle ones? To make the middle band
# comparable every trial is referenced to the participant's own mean stimulus
# (meanVal_c), so "inward" is defined everywhere rather than only outside the
# band.
# =========================================================

# ---------------------------------------------------------
# helpers
# ---------------------------------------------------------

# afex anova_table -> the flat frame the other modules write
compression_anova_tab <- function(fit, exp, analysis) {
  as.data.frame(fit$anova_table) %>%
    tibble::rownames_to_column("effect") %>%
    rename(df1 = `num Df`, df2 = `den Df`, F = `F`, p = `Pr(>F)`) %>%
    mutate(experiment = exp$id, analysis = analysis, .before = 1)
}

compression_posthoc <- function(fit, noise, has_two_conditions, extra_within = NULL) {
  sets <- list(
    noise_overall = pairs(emmeans(fit, as.formula(paste("~", noise))), adjust = "tukey")
  )
  if (has_two_conditions) {
    sets$noise_within_condition <-
      pairs(emmeans(fit, as.formula(paste("~", noise, "| condition"))), adjust = "tukey")
    sets$condition_within_noise <-
      pairs(emmeans(fit, as.formula(paste("~ condition |", noise))), adjust = "tukey")
  }
  if (!is.null(extra_within)) {
    sets[[paste0(extra_within, "_within_noise")]] <-
      pairs(emmeans(fit, as.formula(paste("~", extra_within, "|", noise))), adjust = "tukey")
  }
  bind_rows(lapply(names(sets), function(n)
    mutate(as.data.frame(sets[[n]]), comparison_set = n, .before = 1)))
}

# one-sample tests of each cell against zero (no pull)
compression_vs_zero <- function(d, exp, noise, by_condition) {
  grp <- if (by_condition) c("condition", noise) else noise
  d %>%
    group_by(across(all_of(grp))) %>%
    group_modify(~ {
      tt <- t.test(.x$inward)
      tibble::tibble(
        n = nrow(.x), M = mean(.x$inward), SE = sd(.x$inward) / sqrt(nrow(.x)),
        ci_low = tt$conf.int[1], ci_high = tt$conf.int[2],
        df = unname(tt$parameter), t = unname(tt$statistic), p = tt$p.value
      )
    }) %>%
    ungroup() %>%
    mutate(experiment = exp$id,
           scope = if (by_condition) "by_condition" else "pooled", .before = 1)
}

# ---------------------------------------------------------
# main entry point
# ---------------------------------------------------------
epoc_compression <- function(dat, exp) {

  noise <- exp$noise_var

  d <- dat %>%
    mutate(
      err  = indexSelected - meanVal,
      side = case_when(meanVal < EXAGG_LOW_MEAN  ~ "low",
                       meanVal > EXAGG_HIGH_MEAN ~ "high",
                       TRUE                      ~ "middle"),
      inward_signed = case_when(side == "low"  ~  err,
                                side == "high" ~ -err,
                                TRUE           ~  NA_real_)
    )

  # ---- per side, then side-averaged per participant x level ----------------
  by_side <- d %>%
    filter(side != "middle") %>%
    group_by(participant_id, condition, .level = .data[[noise]], side) %>%
    summarise(n_trials = n(), inward = mean(inward_signed), .groups = "drop") %>%
    rename(!!noise := .level)

  by_participant <- by_side %>%
    group_by(participant_id, condition, .level = .data[[noise]]) %>%
    summarise(
      n_sides = n(),
      n_low   = sum(n_trials[side == "low"]),
      n_high  = sum(n_trials[side == "high"]),
      inward_low  = mean(inward[side == "low"]),
      inward_high = mean(inward[side == "high"]),
      inward      = mean(inward),          # equal weight to each side
      .groups = "drop"
    ) %>%
    rename(!!noise := .level) %>%
    filter(n_sides == 2)                   # both sides needed for the cancellation

  complete_ids <- by_participant %>%
    count(participant_id) %>%
    filter(n == length(exp$levels)) %>%
    pull(participant_id)

  aov_dat <- by_participant %>% filter(participant_id %in% complete_ids)
  has_two_conditions <- nlevels(droplevels(aov_dat$condition)) > 1

  fit <- afex::aov_ez(
    id      = "participant_id",
    dv      = "inward",
    data    = as.data.frame(aov_dat),
    within  = noise,
    between = if (has_two_conditions) "condition" else NULL,
    type    = 3
  )

  summary_tab <- by_participant %>%
    group_by(condition, .level = .data[[noise]]) %>%
    summarise(
      n           = n(),
      # se must be computed before `inward` below rebinds the name
      se          = sd(inward) / sqrt(n()),
      inward      = mean(inward),
      inward_low  = mean(inward_low),
      inward_high = mean(inward_high),
      .groups     = "drop"
    ) %>%
    rename(!!noise := .level) %>%
    mutate(experiment = exp$id, .before = 1)

  vs_zero <- bind_rows(
    compression_vs_zero(by_participant, exp, noise, by_condition = FALSE),
    if (has_two_conditions)
      compression_vs_zero(by_participant, exp, noise, by_condition = TRUE)
  )

  # ---- band analysis: extreme vs middle, referenced to the own mean --------
  band_trials <- d %>%
    filter(meanVal_c != 0) %>%
    mutate(band       = ifelse(side == "middle", "middle", "extreme"),
           inward_own = -sign(meanVal_c) * err,
           dist       = abs(meanVal_c))

  band_by_participant <- band_trials %>%
    group_by(participant_id, condition, .level = .data[[noise]], band) %>%
    summarise(n_trials = n(), dist = mean(dist), inward = mean(inward_own),
              .groups = "drop") %>%
    rename(!!noise := .level)

  band_complete <- band_by_participant %>%
    count(participant_id) %>%
    filter(n == 2 * length(exp$levels)) %>%
    pull(participant_id)

  band_aov_dat <- band_by_participant %>% filter(participant_id %in% band_complete)
  band_two_conditions <- nlevels(droplevels(band_aov_dat$condition)) > 1

  band_fit <- afex::aov_ez(
    id      = "participant_id",
    dv      = "inward",
    data    = as.data.frame(band_aov_dat),
    within  = c(noise, "band"),
    between = if (band_two_conditions) "condition" else NULL,
    type    = 3
  )

  band_summary <- band_by_participant %>%
    group_by(condition, .level = .data[[noise]], band) %>%
    summarise(n = n(), dist = mean(dist),
              se = sd(inward) / sqrt(n()),   # before `inward` is rebound
              inward = mean(inward), .groups = "drop") %>%
    rename(!!noise := .level) %>%
    mutate(experiment = exp$id, .before = 1)

  # ---- the additive response bias the side-averaging removes --------------
  # signed error on middle-band trials, where compression has least room to
  # act. Reported so the bias is visible rather than silently cancelled.
  bias <- d %>%
    filter(side == "middle") %>%
    group_by(participant_id, condition, .level = .data[[noise]]) %>%
    summarise(signed_err = mean(err), .groups = "drop") %>%
    rename(!!noise := .level) %>%
    group_by(condition, .level = .data[[noise]]) %>%
    summarise(n = n(),
              se = sd(signed_err) / sqrt(n()),   # before `signed_err` is rebound
              signed_err = mean(signed_err), .groups = "drop") %>%
    rename(!!noise := .level) %>%
    mutate(experiment = exp$id, .before = 1)

  list(
    by_side        = by_side,
    by_participant = by_participant,
    summary        = summary_tab,
    anova          = compression_anova_tab(fit, exp, "compression"),
    posthoc        = compression_posthoc(fit, noise, has_two_conditions),
    vs_zero        = vs_zero,
    band           = list(
      by_participant = band_by_participant,
      summary        = band_summary,
      anova          = compression_anova_tab(band_fit, exp, "compression_band"),
      posthoc        = compression_posthoc(band_fit, noise, band_two_conditions,
                                           extra_within = "band"),
      fit            = band_fit
    ),
    bias = bias,
    fit  = fit
  )
}

# ---------------------------------------------------------
# figure + output writing
# ---------------------------------------------------------
fig_compression <- function(comp, exp) {
  noise <- exp$noise_var
  ggplot(comp$summary, aes(x = .data[[noise]], y = inward,
                           colour = condition, group = condition)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_line(linewidth = 1) +
    geom_point(size = 2.6) +
    geom_errorbar(aes(ymin = inward - se, ymax = inward + se),
                  width = .12, linewidth = .6) +
    scale_colour_manual(values = COND_COLOURS, drop = TRUE) +
    labs(x = exp$noise_label, y = "Inward pull (scale units)",
         colour = "Condition",
         title = "Centre-directed compression on extreme trials",
         subtitle = paste0(exp$label,
                           " -- above 0 = report pulled towards the centre")) +
    theme_epoc()
}

fig_compression_band <- function(comp, exp) {
  noise <- exp$noise_var
  ggplot(comp$band$summary, aes(x = .data[[noise]], y = inward,
                                colour = band, group = band)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_line(linewidth = 1) +
    geom_point(size = 2.6) +
    geom_errorbar(aes(ymin = inward - se, ymax = inward + se),
                  width = .12, linewidth = .6) +
    facet_wrap(~ condition) +
    labs(x = exp$noise_label, y = "Inward pull (scale units)",
         colour = "Stimulus band",
         title = "Compression on extreme vs middle trials",
         subtitle = paste0(exp$label,
                           " -- every trial referenced to the participant's own mean")) +
    theme_epoc()
}

epoc_compression_outputs <- function(comp, exp, dir) {
  write.csv(comp$by_side,        file.path(dir, "compression_by_side.csv"),        row.names = FALSE)
  write.csv(comp$by_participant, file.path(dir, "compression_by_participant.csv"), row.names = FALSE)
  write.csv(comp$summary,        file.path(dir, "compression_summary.csv"),        row.names = FALSE)
  write.csv(comp$anova,          file.path(dir, "compression_anova.csv"),          row.names = FALSE)
  write.csv(comp$posthoc,        file.path(dir, "compression_posthoc.csv"),        row.names = FALSE)
  write.csv(comp$vs_zero,        file.path(dir, "compression_vs_zero.csv"),        row.names = FALSE)
  write.csv(comp$bias,           file.path(dir, "compression_response_bias.csv"),  row.names = FALSE)

  write.csv(comp$band$by_participant, file.path(dir, "compression_band_by_participant.csv"), row.names = FALSE)
  write.csv(comp$band$summary,        file.path(dir, "compression_band_summary.csv"),        row.names = FALSE)
  write.csv(comp$band$anova,          file.path(dir, "compression_band_anova.csv"),          row.names = FALSE)
  write.csv(comp$band$posthoc,        file.path(dir, "compression_band_posthoc.csv"),        row.names = FALSE)

  epoc_save(fig_compression(comp, exp),      file.path(dir, "fig19_compression.png"))
  epoc_save(fig_compression_band(comp, exp), file.path(dir, "fig20_compression_band.png"),
            width = 8, height = 4.5)
  invisible(TRUE)
}
