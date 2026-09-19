# =========================================================
# 10_galton.R -- Galton squeeze diagrams and bucketed scatter plots
#
# Ported from funcs/galtonsqueezandgraphvariance.R and
# funcs/galtonsqueezeforarraylength.R, which were the same script twice with a
# different noise variable. Here it is one set of functions driven by the
# registry, so every experiment gets the same diagrams.
#
#   forward squeeze   left axis = objective mean, right axis = mean response.
#                     Lines converging towards the middle on the right are
#                     regression to the mean.
#   reverse squeeze   left axis = the response given, right axis = the mean
#                     objective value that produced it. This is the direction
#                     Galton drew, and it answers the other question: given
#                     what someone said, what was really out there.
#   bucketed scatter  the same two directions with responses (or objective
#                     means) pooled into bins of 3 scale units, which is easier
#                     to read than the raw squeeze when there are many levels.
# =========================================================

GALTON_TICKS <- seq(10, 40, by = 5)

# one row per objective mean x cell: where does that stimulus land on average
galton_forward_data <- function(dat, noise) {
  dat %>%
    group_by(across(all_of(c("meanVal", noise, "condition")))) %>%
    summarise(mean_response = mean(indexSelected, na.rm = TRUE),
              n_trials = n(), .groups = "drop")
}

# one row per response value x cell: what was really out there when someone
# gave that response
galton_reverse_data <- function(dat, noise) {
  dat %>%
    group_by(across(all_of(c("indexSelected", noise, "condition")))) %>%
    summarise(mean_true = mean(meanVal, na.rm = TRUE),
              n_trials = n(), .groups = "drop") %>%
    rename(response_value = indexSelected)
}

# The squeeze itself: two vertical scales with a segment per stimulus value.
# `d` needs columns `from` and `to`; everything else is decoration.
galton_plot <- function(d, exp, left_label, right_label, title,
                        ylim = c(0, 50), ticks = GALTON_TICKS,
                        facet = c("grid", "row"),
                        from_colour = "black", to_colour = "firebrick") {

  facet <- match.arg(facet)
  noise <- exp$noise_var

  # tick marks and labels on both inner axes, one copy per panel
  scale_df <- expand.grid(
    condition = levels(droplevels(d$condition)),
    .lvl      = levels(droplevels(d[[noise]])),
    tick      = ticks
  )
  names(scale_df)[2] <- noise

  p <- ggplot(d) +
    geom_segment(aes(x = 1, xend = 2, y = from, yend = to),
                 alpha = .35, linewidth = .6, colour = "grey50") +
    geom_point(aes(x = 1, y = from), size = 1.2, alpha = .8, colour = from_colour) +
    geom_point(aes(x = 2, y = to),   size = 1.5, alpha = .9, colour = to_colour) +
    geom_vline(xintercept = c(1, 2), linewidth = .5) +
    geom_segment(data = scale_df, aes(x = 0.985, xend = 1, y = tick, yend = tick),
                 inherit.aes = FALSE, linewidth = .35) +
    geom_segment(data = scale_df, aes(x = 2, xend = 2.015, y = tick, yend = tick),
                 inherit.aes = FALSE, linewidth = .35) +
    geom_text(data = scale_df, aes(x = 0.96, y = tick, label = tick),
              inherit.aes = FALSE, hjust = 1, size = 2.7) +
    geom_text(data = scale_df, aes(x = 2.04, y = tick, label = tick),
              inherit.aes = FALSE, hjust = 0, size = 2.7) +
    scale_x_continuous(breaks = c(1, 2), labels = c(left_label, right_label),
                       expand = expansion(mult = c(.3, .3))) +
    coord_cartesian(ylim = ylim, clip = "off") +
    labs(title = title, subtitle = exp$label, x = NULL, y = NULL) +
    theme_classic(base_size = 13) +
    theme(
      axis.text.y      = element_blank(),
      axis.ticks.y     = element_blank(),
      strip.background = element_rect(fill = "white", colour = "black"),
      strip.text       = element_text(face = "bold"),
      panel.spacing    = unit(1.4, "lines"),
      plot.subtitle    = element_text(colour = "grey30"),
      plot.margin      = margin(t = 12, r = 30, b = 18, l = 30)
    )

  lab <- setNames(list(function(x) paste0(exp$noise_label, ": ", x)), noise)
  if (facet == "grid" && nlevels(droplevels(d$condition)) > 1) {
    p + facet_grid(as.formula(paste("condition ~", noise)),
                   labeller = do.call(labeller, lab))
  } else {
    p + facet_wrap(as.formula(paste("~", noise)), nrow = 1,
                   labeller = do.call(labeller, lab))
  }
}

# Responses (or objective means) pooled into bins of 3 scale units.
# direction = "reverse": x = response bucket,      y = mean objective value
# direction = "forward": x = objective mean bucket, y = mean response
bucket_scatter_data <- function(dat, noise, direction = c("reverse", "forward"),
                                width = 3) {
  direction <- match.arg(direction)
  src <- if (direction == "reverse") "indexSelected" else "meanVal"
  tgt <- if (direction == "reverse") "meanVal"       else "indexSelected"

  dat %>%
    mutate(
      bucket_start = floor(.data[[src]] / width) * width,
      bucket_end   = bucket_start + width - 1,
      bucket       = bucket_start + (width - 1) / 2,
      bucket_label = paste0(bucket_start, "-", bucket_end)
    ) %>%
    group_by(across(all_of(c("bucket", "bucket_label", noise, "condition")))) %>%
    summarise(value = mean(.data[[tgt]], na.rm = TRUE),
              n_trials = n(), .groups = "drop") %>%
    mutate(direction = direction)
}

bucket_scatter_plot <- function(d, exp, direction) {
  noise <- exp$noise_var
  labs_xy <- if (direction == "reverse")
    list(x = "Response given (buckets of 3)", y = "Mean objective value",
         title = "What was really out there, given the response")
  else
    list(x = "Objective mean (buckets of 3)", y = "Mean response",
         title = "What people reported, given the stimulus")

  ggplot(d, aes(x = bucket, y = value, colour = .data[[noise]])) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    geom_point(size = 1.8, alpha = .85) +
    geom_smooth(method = "lm", se = FALSE, linewidth = .8, formula = y ~ x) +
    facet_wrap(~ condition) +
    scale_colour_viridis_d(option = "C", end = .8) +
    coord_cartesian(xlim = c(SCALE_MIN, SCALE_MAX), ylim = c(SCALE_MIN, SCALE_MAX)) +
    labs(x = labs_xy$x, y = labs_xy$y, colour = exp$noise_label,
         title = labs_xy$title, subtitle = exp$label) +
    theme_epoc()
}

# Everything above, written out for one experiment
epoc_galton_figures <- function(dat, exp, dir) {

  noise <- exp$noise_var

  fwd <- galton_forward_data(dat, noise)
  rev <- galton_reverse_data(dat, noise)

  fwd_plot_data <- fwd %>% mutate(from = meanVal,        to = mean_response)
  rev_plot_data <- rev %>% mutate(from = response_value, to = mean_true)

  epoc_save(
    galton_plot(fwd_plot_data, exp, "Objective mean", "Mean response",
                "Galton squeeze", ylim = c(10, 40)),
    file.path(dir, "fig5_galton.png"), width = 9, height = 6)

  epoc_save(
    galton_plot(rev_plot_data, exp, "Response given", "Objective mean",
                "Reverse Galton squeeze", ylim = c(0, 50),
                from_colour = "firebrick", to_colour = "black"),
    file.path(dir, "fig6_galton_reverse.png"), width = 9, height = 6)

  # one panel row per condition as well: the grid gets crowded, and these are
  # the versions that go into talks
  for (cond in levels(droplevels(dat$condition))) {
    epoc_save(
      galton_plot(rev_plot_data %>% filter(condition == cond), exp,
                  "Response given", "Objective mean",
                  paste0("Reverse Galton squeeze - ", cond, " condition"),
                  ylim = c(0, 50), facet = "row",
                  from_colour = "firebrick", to_colour = "black"),
      file.path(dir, paste0("fig6_galton_reverse_", cond, ".png")),
      width = 9, height = 4.5)
  }

  buck_rev <- bucket_scatter_data(dat, noise, "reverse")
  buck_fwd <- bucket_scatter_data(dat, noise, "forward")

  epoc_save(bucket_scatter_plot(buck_rev, exp, "reverse"),
            file.path(dir, "fig7_bucket_reverse.png"), width = 8, height = 4.5)
  epoc_save(bucket_scatter_plot(buck_fwd, exp, "forward"),
            file.path(dir, "fig8_bucket_forward.png"), width = 8, height = 4.5)

  write.csv(fwd, file.path(dir, "galton_forward.csv"), row.names = FALSE)
  write.csv(rev, file.path(dir, "galton_reverse.csv"), row.names = FALSE)
  write.csv(bind_rows(buck_rev, buck_fwd),
            file.path(dir, "bucket_scatter.csv"), row.names = FALSE)

  invisible(TRUE)
}
