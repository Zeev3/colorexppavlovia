# =========================================================
# 18_exploratory_report_to_true.R -- EXPLORATORY, not part of the write-up
#
# What stimulus lies behind each individual report value, per noise level.
# For every report value (0-50) the mean objective value behind it is mapped
# separately for each noise level; at the tails, the question is whether the
# same extreme report comes from a less extreme stimulus when noise is higher.
#
# Two versions:
#   raw            reports as given
#   shift_removed  each participant's constant error at each noise level
#                  (mean report - objective in that cell) is subtracted first.
#                  Needed in Experiments 1-3, where 2-square arrays are
#                  reported 2.4-4 units low across the whole range; that shift
#                  moves both tails the same way and otherwise masquerades as
#                  (or hides) a tail effect.
#
# Tail test, per version and tail: objective ~ factor(report value) + level
# + (1 | participant), so only trials with the same report value are compared.
# "less_extreme" is signed so that a positive value means the stimulus behind
# an extreme report was less extreme at the noisier level (lower at the high
# tail, higher at the low tail).
#
# Also runs the same question on the Goldenberg data (see the end of this file).
# Everything goes to output/<exp>/exploratory/ and output/exploratory/; nothing
# here feeds results.md or the cross-experiment tables of the main analysis.
# =========================================================

EXPL_TAIL_LOW  <- 14   # reports <= this form the low tail
EXPL_TAIL_HIGH <- 36   # reports >= this form the high tail
EXPL_MIN_CELL  <- 10   # report values with fewer trials at a level are not plotted

expl_level_labels <- function(exp) {
  if (exp$noise_var == "variance_level") paste("SD", exp$levels) else paste(exp$levels, "squares")
}

expl_add_versions <- function(dat, exp) {
  labs_ <- expl_level_labels(exp)
  dat$level <- factor(labs_[match(as.character(dat[[exp$noise_var]]), as.character(exp$levels))], levels = labs_)
  dat %>%
    group_by(participant_id, level) %>%
    mutate(shift = mean(indexSelected - meanVal)) %>%
    ungroup() %>%
    mutate(report_raw = indexSelected,
           report_shift_removed = round(indexSelected - shift))
}

expl_tail_tests <- function(dat, exp, version) {
  rep_col <- paste0("report_", version)
  labs_ <- expl_level_labels(exp)
  bind_rows(lapply(c("low", "high"), function(side) {
    x <- dat %>% filter(if (side == "low") .data[[rep_col]] <= EXPL_TAIL_LOW else .data[[rep_col]] >= EXPL_TAIL_HIGH)
    x$rep <- factor(x[[rep_col]])
    m <- lmerTest::lmer(meanVal ~ rep + level + (1 | participant_id), data = x,
                        control = lmerControl(optimizer = "bobyqa"))
    em <- emmeans(m, ~ level, lmer.df = "satterthwaite")
    p_level <- anova(m)["level", "Pr(>F)"]
    means <- as.data.frame(summary(em))
    prs <- as.data.frame(summary(pairs(em, reverse = TRUE)))   # later level minus earlier
    # "a - b" with a the noisier level: at the high tail a lower objective value
    # is less extreme, at the low tail a higher one is
    prs %>%
      mutate(experiment = exp$id, version = version, tail = side, n_trials = nrow(x),
             n_participants = n_distinct(x$participant_id), level_effect_p = p_level,
             less_extreme = if (side == "high") -estimate else estimate,
             .before = 1) %>%
      cross_join(means %>% select(level, emmean) %>% mutate(level = as.character(level)) %>%
                   tidyr::pivot_wider(names_from = level, values_from = emmean, names_prefix = "objective_at_same_report: "))
  }))
}

expl_figure <- function(tab, exp, version, levels_shown) {
  labs_ <- expl_level_labels(exp)
  cols <- setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")[seq_along(labs_)], labs_)
  shp  <- setNames(c(16, 17, 15, 18)[seq_along(labs_)], labs_)
  d <- tab %>% filter(version == !!version, level %in% levels_shown, n >= EXPL_MIN_CELL) %>% droplevels()
  xlab <- if (version == "raw") "Reported value"
          else "Reported value, shift removed (each participant's mean error at that level subtracted)"
  ggplot(d, aes(report, mean_objective, colour = level, shape = level)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60", linewidth = .5) +
    annotate("text", x = 40.5, y = 38.5, label = "report = objective", colour = "grey45", size = 3.3, hjust = 0) +
    geom_line(linewidth = .7) + geom_point(size = 2.2) +
    scale_colour_manual(values = cols, name = exp$noise_label) +
    scale_shape_manual(values = shp, name = exp$noise_label) +
    scale_x_continuous(breaks = seq(0, 50, 5)) + scale_y_continuous(breaks = seq(10, 40, 5)) +
    coord_cartesian(xlim = c(0, 50), ylim = c(10, 40)) +
    labs(x = xlab, y = "Mean objective value behind the report",
         title = paste0(sub(" - .*", "", exp$label), ": what lies behind each report",
                        if (version == "shift_removed") ", constant shift removed" else ""),
         subtitle = paste0("EXPLORATORY. Both conditions pooled; report values with fewer than ",
                           EXPL_MIN_CELL, " trials at a level omitted")) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "top",
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(colour = "grey40"))
}

epoc_report_to_true <- function(dat, exp, dir) {
  out <- file.path(dir, "exploratory")
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  labs_ <- expl_level_labels(exp)
  d <- expl_add_versions(dat, exp)

  values <- bind_rows(lapply(c("raw", "shift_removed"), function(v) {
    d %>% group_by(level, report = .data[[paste0("report_", v)]]) %>%
      summarise(mean_objective = mean(meanVal), n = n(),
                sd_objective = if (n() > 1) sd(meanVal) else NA_real_,
                min_objective = min(meanVal), max_objective = max(meanVal), .groups = "drop") %>%
      mutate(experiment = exp$id, version = v, .before = 1)
  }))
  shifts <- d %>% group_by(level) %>%
    summarise(mean_shift = mean(shift), sd_shift = sd(shift[!duplicated(participant_id)]), .groups = "drop") %>%
    mutate(experiment = exp$id, .before = 1)
  tests <- bind_rows(expl_tail_tests(d, exp, "raw"), expl_tail_tests(d, exp, "shift_removed"))

  write.csv(values, file.path(out, "report_to_true_values.csv"), row.names = FALSE)
  write.csv(shifts, file.path(out, "report_to_true_shifts.csv"), row.names = FALSE)
  write.csv(tests,  file.path(out, "report_to_true_tests.csv"),  row.names = FALSE)

  ends <- labs_[c(1, length(labs_))]
  for (v in c("raw", "shift_removed")) {
    epoc_save(expl_figure(values, exp, v, labs_), file.path(out, paste0("report_to_true_", v, "_all_levels.png")), width = 8.5, height = 6, dpi = 200)
    epoc_save(expl_figure(values, exp, v, ends),  file.path(out, paste0("report_to_true_", v, "_extremes.png")),  width = 8.5, height = 6, dpi = 200)
  }

  # short readable summary
  con <- file(file.path(out, "exploratory.md"), open = "wt"); on.exit(close(con))
  w <- function(...) log_line(con, ...)
  w("# ", exp$label, " - what lies behind each report (EXPLORATORY)")
  w("")
  w("_Not part of the main write-up. Generated by analysis/run_all.R on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "._")
  w("")
  w("Tails: reports <= ", EXPL_TAIL_LOW, " and >= ", EXPL_TAIL_HIGH, ". Model: objective ~ report value (factor) + ",
    exp$noise, " + (1 | participant). `less extreme` > 0 means the stimulus behind an extreme report was less ",
    "extreme at the noisier level.")
  w("")
  w("Constant shift removed per level (mean report - objective): ",
    paste(sprintf("%s %+.2f", shifts$level, shifts$mean_shift), collapse = ", "), ".")
  w("")
  for (v in c("raw", "shift_removed")) {
    w("## ", if (v == "raw") "Reports as given" else "Constant shift removed")
    w("")
    for (side in c("low", "high")) {
      tt <- tests %>% filter(version == v, tail == side)
      w("- ", side, " tail (", tt$n_trials[1], " trials, ", tt$n_participants[1], " participants): effect of ",
        exp$noise, " ", fmt_p(tt$level_effect_p[1]), ".")
      for (i in seq_len(nrow(tt)))
        w("  - ", tt$contrast[i], ": less extreme by ", sprintf("%.2f", tt$less_extreme[i]), ", ", fmt_p(tt$p.value[i]), ".")
    }
    w("")
  }
  invisible(list(values = values, shifts = shifts, tests = tests))
}

# ---------------------------------------------------------
# Goldenberg et al. (2021), Exp. 1 (data/amit_exp1.csv) -- same question on
# the face-valence data. numberFaces is stored 0-11 and is 1-12 faces shown.
# Set sizes are grouped into three clusters; the tails are ratings <= 110 and
# >= 140 on the 100-150 scale, and "exaggeration" is how much more extreme the
# rating was than the group behind it (objective - rating on the low side,
# rating - objective on the high side), averaged within participant first.
# ---------------------------------------------------------
GB_TAIL_LOW  <- 110
GB_TAIL_HIGH <- 140
GB_CLUSTERS  <- c(0, 4, 8, 12)   # breaks on the number of faces: 1-4, 5-8, 9-12

epoc_goldenberg_report_to_true <- function(dir = file.path(OUT, "exp0_goldenberg", "exploratory")) {
  path <- file.path(DATA, "amit_exp1.csv")
  if (!file.exists(path)) {
    message("amit_exp1.csv not found; skipping the exploratory rating-to-true analysis")
    return(invisible(NULL))
  }
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  labs_ <- paste0(head(GB_CLUSTERS, -1) + 1, "-", GB_CLUSTERS[-1], " faces")
  d <- read.csv(path, stringsAsFactors = FALSE) %>%
    filter(!is.na(rating), !is.na(meanGroup), !is.na(numberFaces)) %>%
    mutate(faces   = numberFaces + 1,
           cluster = cut(faces, GB_CLUSTERS, labels = labs_),
           rating  = round(rating),
           id      = factor(id))

  values <- d %>% group_by(cluster, rating) %>%
    summarise(mean_objective = mean(meanGroup), n = n(), .groups = "drop")

  tails <- d %>%
    mutate(side = case_when(rating <= GB_TAIL_LOW ~ "low", rating >= GB_TAIL_HIGH ~ "high")) %>%
    filter(!is.na(side)) %>%
    mutate(exaggeration = ifelse(side == "low", meanGroup - rating, rating - meanGroup))

  exaggeration <- tails %>%
    group_by(side, cluster, id) %>%
    summarise(ex = mean(exaggeration), .groups = "drop") %>%
    group_by(side, cluster) %>%
    summarise(mean_exaggeration = mean(ex), se = sd(ex) / sqrt(n()), n_participants = n(), .groups = "drop") %>%
    left_join(tails %>% group_by(side, cluster) %>%
                summarise(n_trials = n(), mean_rating = mean(rating), mean_objective = mean(meanGroup), .groups = "drop"),
              by = c("side", "cluster"))

  tests <- bind_rows(lapply(c("low", "high"), function(s) {
    x <- tails %>% filter(side == s)
    m  <- lmerTest::lmer(exaggeration ~ cluster + (1 | id), data = x)
    m2 <- lmerTest::lmer(meanGroup ~ factor(rating) + cluster + (1 | id), data = x)   # same-rating check
    as.data.frame(summary(pairs(emmeans(m, ~ cluster, lmer.df = "satterthwaite"), reverse = TRUE))) %>%
      mutate(side = s, cluster_effect_p = anova(m)["cluster", "Pr(>F)"],
             same_rating_cluster_effect_p = anova(m2)["cluster", "Pr(>F)"], .before = 1)
  }))

  write.csv(values,       file.path(dir, "rating_to_true_values.csv"),        row.names = FALSE)
  write.csv(exaggeration, file.path(dir, "exaggeration_by_side_cluster.csv"), row.names = FALSE)
  write.csv(tests,        file.path(dir, "exaggeration_tests.csv"),           row.names = FALSE)

  cols <- setNames(c("#2a78d6", "#eb6834", "#1baf7a"), labs_)
  fig <- ggplot(values %>% filter(n >= EXPL_MIN_CELL), aes(rating, mean_objective, colour = cluster, shape = cluster)) +
    annotate("rect", xmin = -Inf, xmax = GB_TAIL_LOW + .5,  ymin = -Inf, ymax = Inf, fill = "grey92") +
    annotate("rect", xmin = GB_TAIL_HIGH - .5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "grey92") +
    annotate("text", x = 105.5, y = 148, label = paste0("low side\n(rating <= ", GB_TAIL_LOW, ")"),  colour = "grey40", size = 3.2) +
    annotate("text", x = 145,   y = 106, label = paste0("high side\n(rating >= ", GB_TAIL_HIGH, ")"), colour = "grey40", size = 3.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55", linewidth = .5) +
    geom_line(linewidth = .7) + geom_point(size = 2) +
    scale_colour_manual(values = cols, name = "Number of faces") +
    scale_shape_manual(values = setNames(c(16, 17, 15), labs_), name = "Number of faces") +
    scale_x_continuous(breaks = seq(100, 150, 5)) + scale_y_continuous(breaks = seq(100, 150, 5)) +
    coord_cartesian(xlim = c(100, 150), ylim = c(100, 150)) +
    labs(x = "Rating", y = "Mean objective group value behind the rating",
         title = "Goldenberg et al. (2021), Exp. 1: what lies behind each rating",
         subtitle = paste0("EXPLORATORY. Positive and negative arrays pooled; rating values with fewer than ",
                           EXPL_MIN_CELL, " trials in a cluster omitted\nDashed line: rating = objective value")) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "top",
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(colour = "grey40"))
  epoc_save(fig, file.path(dir, "rating_to_true.png"), width = 8.5, height = 7, dpi = 200)

  con <- file(file.path(dir, "exploratory.md"), open = "wt"); on.exit(close(con))
  w <- function(...) log_line(con, ...)
  w("# Goldenberg et al. (2021), Exp. 1 - what lies behind each rating (EXPLORATORY)")
  w("")
  w("_Not part of the main write-up. Generated by analysis/run_all.R on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "._")
  w("")
  w("N = ", n_distinct(d$id), " participants, ", nrow(d), " trials; positive and negative arrays pooled. ",
    "Exaggeration = how much more extreme the rating was than the group behind it, on the low side ",
    "(rating <= ", GB_TAIL_LOW, ") and the high side (rating >= ", GB_TAIL_HIGH, "), averaged within participant first.")
  w("")
  w("| Number of faces | Low side: exaggeration (SE) | High side: exaggeration (SE) |")
  w("|---|---|---|")
  for (cl in labs_) {
    lo <- exaggeration %>% filter(side == "low", cluster == cl)
    hi <- exaggeration %>% filter(side == "high", cluster == cl)
    w("| ", cl, " | ", sprintf("%.1f (%.2f)", lo$mean_exaggeration, lo$se), " | ", sprintf("%.1f (%.2f)", hi$mean_exaggeration, hi$se), " |")
  }
  w("")
  for (s in c("low", "high")) {
    tt <- tests %>% filter(side == s)
    w("- ", s, " side: effect of number of faces ", fmt_p(tt$cluster_effect_p[1]),
      "; comparing only equal rating values ", fmt_p(tt$same_rating_cluster_effect_p[1]), ".")
    for (i in seq_len(nrow(tt)))
      w("  - ", tt$contrast[i], ": ", sprintf("%+.2f", tt$estimate[i]), ", ", fmt_p(tt$p.value[i]), ".")
  }
  w("")
  w("Caution: averages of more faces are less dispersed and ratings get noisier with set size, and both widen ",
    "this gap even with no bias (in an ad hoc noise-only simulation, not part of this pipeline, noise alone ",
    "reproduced most of the growth). The pattern alone does not show that people inflate more.")

  invisible(list(values = values, exaggeration = exaggeration, tests = tests))
}

# ---------------------------------------------------------
# Slope decomposition: why does the slope change across noise levels?
#
# slope = r x (SD of reports / SD of objective values), so on a log scale
# log(slope) = log(r) + log(SD ratio) and a change in slope between two levels
# splits exactly into a change in correlation (a weaker relationship) and a
# change in the SD ratio (reports spreading less, or more, relative to the
# stimuli). Values are centred within participant (and within valence for the
# Goldenberg data), which is close to what the mixed models do. The paired
# tests compare the lowest and highest level within participant, on Fisher z
# for r and on log(SD ratio).
# ---------------------------------------------------------
expl_slope_split <- function(d, groups) {
  # d: participant_id, level (ordered), x, y (centred), and a column `group`
  bind_rows(lapply(groups, function(g) {
    dd <- if (g == "all") d else d %>% filter(group == g)
    tab <- dd %>% group_by(level) %>%
      summarise(n_trials = n(),
                mean_item_sd = if ("item_sd" %in% names(dd)) mean(item_sd, na.rm = TRUE) else NA_real_,
                r = cor(x, y), sd_objective = sd(x), sd_report = sd(y), .groups = "drop") %>%
      mutate(sd_ratio = sd_report / sd_objective, slope = r * sd_ratio) %>% arrange(level)
    lo <- tab[1, ]; hi <- tab[nrow(tab), ]
    tot <- log(hi$slope / lo$slope); fr <- log(hi$r / lo$r); fq <- log(hi$sd_ratio / lo$sd_ratio)
    pl <- dd %>% filter(level %in% c(lo$level, hi$level)) %>%
      group_by(participant_id, level) %>%
      summarise(z = atanh(cor(x, y)), lratio = log(sd(y) / sd(x)), .groups = "drop") %>%
      mutate(level = ifelse(level == lo$level, "lo", "hi")) %>%
      tidyr::pivot_wider(id_cols = participant_id, names_from = level, values_from = c(z, lratio)) %>%
      filter(is.finite(z_lo), is.finite(z_hi), is.finite(lratio_lo), is.finite(lratio_hi))
    tab %>% mutate(group = g, .before = 1) %>%
      mutate(split_from = as.character(lo$level), split_to = as.character(hi$level),
             log_change_slope = tot, log_change_from_r = fr, log_change_from_sd_ratio = fq,
             share_from_r = fr / tot, share_from_sd_ratio = fq / tot,
             paired_n = nrow(pl),
             paired_p_r = t.test(pl$z_lo, pl$z_hi, paired = TRUE)$p.value,
             paired_p_sd_ratio = t.test(pl$lratio_lo, pl$lratio_hi, paired = TRUE)$p.value)
  }))
}

expl_slope_split_md <- function(res, path, title, level_name) {
  con <- file(path, open = "wt"); on.exit(close(con))
  w <- function(...) log_line(con, ...)
  w("# ", title, " - why the slope changes (EXPLORATORY)")
  w("")
  w("_Not part of the main write-up. Generated by analysis/run_all.R on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "._")
  w("")
  w("slope = correlation x (SD of reports / SD of true values). On a log scale the change in slope ",
    "between the lowest and highest ", level_name, " is exactly the sum of the change in correlation and the ",
    "change in the SD ratio. Values centred within participant. `SD within arrays` is the mean dispersion of ",
    "the items inside each array (the perceptual noise actually shown); `SD of true values` is the spread of ",
    "the array means across trials.")
  w("")
  for (g in unique(res$group)) {
    r <- res %>% filter(group == g)
    w("## ", if (g == "all") "All participants" else g)
    w("")
    w("| ", level_name, " | trials | SD within arrays | SD of true values | SD of reports | SD ratio | r | slope |")
    w("|---|---|---|---|---|---|---|---|")
    for (i in seq_len(nrow(r)))
      w(sprintf("| %s | %d | %s | %.2f | %.2f | %.3f | %.3f | %.3f |", as.character(r$level[i]), r$n_trials[i],
                ifelse(is.na(r$mean_item_sd[i]), "n/a", sprintf("%.2f", r$mean_item_sd[i])),
                r$sd_objective[i], r$sd_report[i], r$sd_ratio[i], r$r[i], r$slope[i]))
    w("")
    w(sprintf("%s -> %s: log change in slope %+.3f = correlation %+.3f + SD ratio %+.3f (shares %.0f%% / %.0f%%).",
              r$split_from[1], r$split_to[1], r$log_change_slope[1], r$log_change_from_r[1], r$log_change_from_sd_ratio[1],
              100 * r$share_from_r[1], 100 * r$share_from_sd_ratio[1]))
    w("Within participant (n = ", r$paired_n[1], "): correlation (Fisher z) ", fmt_p(r$paired_p_r[1]),
      "; SD ratio (log) ", fmt_p(r$paired_p_sd_ratio[1]), ".")
    w("")
  }
}

epoc_slope_decomposition <- function(dat, exp, dir) {
  out <- file.path(dir, "exploratory")
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  # within-array dispersion, from the stimulus audit's trial-level cache when it exists
  cache <- file.path(ARRAY_CACHE_DIR, paste0("item_sd_trials_", exp$id, ".csv"))
  if (file.exists(cache)) {
    isd <- read.csv(cache, stringsAsFactors = FALSE) %>%
      distinct(participant_id, trial, condition, .keep_all = TRUE) %>%
      select(participant_id, trial, condition, item_sd)
    # prepared ids carry a "<condition>." prefix; the audit cache uses the raw id
    dat <- dat %>%
      mutate(condition = as.character(condition),
             raw_id = sub("^[^.]+\\.", "", as.character(participant_id))) %>%
      left_join(isd %>% rename(raw_id = participant_id), by = c("raw_id", "trial", "condition"))
  }
  d <- dat %>%
    mutate(level = as.numeric(as.character(.data[[exp$noise_var]])), group = as.character(condition)) %>%
    group_by(participant_id) %>%
    mutate(x = meanVal - mean(meanVal), y = indexSelected - mean(indexSelected)) %>%
    ungroup()
  res <- expl_slope_split(d, c("all", sort(unique(d$group)))) %>% mutate(experiment = exp$id, .before = 1)
  write.csv(res, file.path(out, "slope_decomposition.csv"), row.names = FALSE)
  expl_slope_split_md(res, file.path(out, "slope_decomposition.md"), exp$label, exp$noise)
  invisible(res)
}

epoc_goldenberg_slope_decomposition <- function(dir = file.path(OUT, "exp0_goldenberg", "exploratory")) {
  path <- file.path(DATA, "amit_exp1.csv")
  if (!file.exists(path)) return(invisible(NULL))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  labs_ <- paste0(head(GB_CLUSTERS, -1) + 1, "-", GB_CLUSTERS[-1], " faces")
  base <- read.csv(path, stringsAsFactors = FALSE) %>%
    filter(!is.na(rating), !is.na(meanGroup), !is.na(numberFaces)) %>%
    mutate(participant_id = factor(id), faces = numberFaces + 1, group = "all",
           # SD of the faces in the array; undefined for 1-face arrays, which are skipped in its mean
           item_sd = apply(across(any_of(c(paste0("par", 1:12), "part10"))), 1, function(v) sd(v, na.rm = TRUE))) %>%
    group_by(participant_id, condition) %>%        # centre within participant and valence
    mutate(x = meanGroup - mean(meanGroup), y = rating - mean(rating)) %>%
    ungroup()
  res <- bind_rows(
    expl_slope_split(base %>% mutate(level = faces), "all") %>% mutate(grouping = "each set size", level = as.character(level)),
    expl_slope_split(base %>% mutate(level = cut(faces, GB_CLUSTERS, labels = labs_)), "all") %>%
      mutate(grouping = "clusters", level = as.character(level))
  ) %>% mutate(experiment = "exp0_goldenberg", .before = 1)
  write.csv(res, file.path(dir, "slope_decomposition.csv"), row.names = FALSE)
  for (gr in c("each set size", "clusters"))
    expl_slope_split_md(res %>% filter(grouping == gr) %>% mutate(group = gr),
                        file.path(dir, paste0("slope_decomposition_", gsub(" ", "_", gr), ".md")),
                        "Goldenberg et al. (2021), Exp. 1", "number of faces")
  invisible(res)
}

# one readable table across experiments, written by run_all.R
expl_slope_split_all_md <- function(res, path) {
  con <- file(path, open = "wt"); on.exit(close(con))
  w <- function(...) log_line(con, ...)
  w("# Why the slope changes, all experiments (EXPLORATORY)")
  w("")
  w("_Not part of the main write-up. Generated by analysis/run_all.R on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "._")
  w("")
  w("All participants per experiment (conditions pooled). slope = r x (SD of reports / SD of true values); ",
    "the last columns split the log change in slope from the lowest to the highest level.")
  w("")
  w("| experiment | level | trials | SD within arrays | SD of true values | SD of reports | SD ratio | r | slope |")
  w("|---|---|---|---|---|---|---|---|---|")
  for (i in seq_len(nrow(res)))
    w(sprintf("| %s | %s | %d | %s | %.2f | %.2f | %.3f | %.3f | %.3f |", res$experiment[i], as.character(res$level[i]),
              res$n_trials[i], ifelse(is.na(res$mean_item_sd[i]), "n/a", sprintf("%.2f", res$mean_item_sd[i])),
              res$sd_objective[i], res$sd_report[i], res$sd_ratio[i], res$r[i], res$slope[i]))
  w("")
  sm <- res %>% distinct(experiment, split_from, split_to, log_change_slope, share_from_r, share_from_sd_ratio,
                         paired_n, paired_p_r, paired_p_sd_ratio)
  w("| experiment | from -> to | log change in slope | share from r | share from SD ratio | p (r) | p (SD ratio) | n |")
  w("|---|---|---|---|---|---|---|---|")
  for (i in seq_len(nrow(sm)))
    w(sprintf("| %s | %s -> %s | %+.3f | %.0f%% | %.0f%% | %s | %s | %d |", sm$experiment[i], sm$split_from[i], sm$split_to[i],
              sm$log_change_slope[i], 100 * sm$share_from_r[i], 100 * sm$share_from_sd_ratio[i],
              sub("^p (= )?", "", fmt_p(sm$paired_p_r[i])), sub("^p (= )?", "", fmt_p(sm$paired_p_sd_ratio[i])), sm$paired_n[i]))
}

# ---------------------------------------------------------
# Regression panels: forward (report ~ objective) and reverse (objective ~
# report), in z-scores and in raw units, one line per noise level.
# In z-scores both slopes equal r, so the two directions look the same; in raw
# units the forward slope is r x SD ratio and the reverse slope r / SD ratio,
# so noise and a narrowing of the reports pull the two directions apart.
# Values centred within participant (and valence, for Goldenberg); z-scores are
# computed within each level. Needs the patchwork package.
# ---------------------------------------------------------
expl_regression_panels <- function(d, title, raw_lims, raw_bin, out_png, out_csv, id) {
  d <- d %>% group_by(cen) %>% mutate(tc = true - mean(true), rc = rep - mean(rep)) %>% ungroup() %>%
    mutate(true_raw = tc + mean(true), rep_raw = rc + mean(rep)) %>%
    group_by(L) %>% mutate(true_z = as.numeric(scale(tc)), rep_z = as.numeric(scale(rc))) %>% ungroup()
  levs <- levels(droplevels(d$L))
  cols <- setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")[seq_along(levs)], levs)
  fits <- d %>% group_by(L) %>% summarise(
    n_trials = n(), r = cor(tc, rc), sd_true = sd(tc), sd_report = sd(rc),
    z_forward = coef(lm(rep_z ~ true_z))[2], z_reverse = coef(lm(true_z ~ rep_z))[2],
    raw_forward = coef(lm(rep_raw ~ true_raw))[2], raw_reverse = coef(lm(true_raw ~ rep_raw))[2], .groups = "drop") %>%
    mutate(experiment = id, .before = 1) %>% rename(level = L)
  write.csv(fits, out_csv, row.names = FALSE)

  one <- function(xv, yv, sc, binw, lims, xlab, ylab, ttl) {
    dd <- d %>% mutate(x = .data[[xv]], y = .data[[yv]]) %>% filter(x >= lims[1], x <= lims[2])
    pts <- dd %>% mutate(xb = round(x / binw) * binw) %>% group_by(L, xb) %>%
      summarise(y = mean(y), n = n(), .groups = "drop") %>% filter(n >= 15)
    lab <- setNames(paste0(fits$level, " (", sprintf("%.2f", fits[[sc]]), ")"), fits$level)
    ggplot(dd, aes(x, y, colour = L)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55", linewidth = .5) +
      geom_point(data = pts, aes(xb, y), size = 1.2, alpha = .5) +
      geom_smooth(method = "lm", se = FALSE, linewidth = .9, formula = y ~ x) +
      scale_colour_manual(values = cols, labels = lab, name = NULL) +
      coord_equal(xlim = lims, ylim = lims) + labs(x = xlab, y = ylab, title = ttl) +
      theme_minimal(base_size = 10.5) +
      theme(panel.grid.minor = element_blank(), legend.position = "top", legend.text = element_text(size = 8.5),
            plot.title = element_text(face = "bold", size = 11))
  }
  zl <- c(-2.2, 2.2)
  # wrap_plots rather than the | and / operators, so patchwork need not be attached
  p <- patchwork::wrap_plots(list(
         one("true_z", "rep_z", "z_forward", .25, zl, "Objective mean (z)", "Report (z)", "Forward, z-scores"),
         one("rep_z", "true_z", "z_reverse", .25, zl, "Report (z)", "Objective mean (z)", "Reverse, z-scores"),
         one("true_raw", "rep_raw", "raw_forward", raw_bin, raw_lims, "Objective mean", "Report", "Forward, raw units"),
         one("rep_raw", "true_raw", "raw_reverse", raw_bin, raw_lims, "Report", "Objective mean", "Reverse, raw units")),
         ncol = 2) +
    patchwork::plot_annotation(
      title = title,
      subtitle = paste("EXPLORATORY. Centred within participant; z-scores computed within each level.",
                       "Slopes in brackets. Dots: binned means; dashed line: slope 1"),
      theme = theme(plot.title = element_text(face = "bold", size = 14),
                    plot.subtitle = element_text(colour = "grey40", size = 9.5)))
  epoc_save(p, out_png, width = 11, height = 12, dpi = 170)
  invisible(fits)
}

epoc_regression_panels <- function(dat, exp, dir) {
  out <- file.path(dir, "exploratory")
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  labs_ <- expl_level_labels(exp)
  d <- dat %>%
    mutate(L = factor(labs_[match(as.character(.data[[exp$noise_var]]), as.character(exp$levels))], levels = labs_),
           cen = participant_id, true = meanVal, rep = indexSelected)
  ttl <- if (grepl(" - ", exp$label)) sub(" - ", ": ", exp$label) else exp$label
  expl_regression_panels(d, ttl,
                         c(8, 42), 1, file.path(out, "regression_panels.png"), file.path(out, "regression_panels.csv"), exp$id)
}

epoc_goldenberg_regression_panels <- function(dir = file.path(OUT, "exp0_goldenberg", "exploratory")) {
  path <- file.path(DATA, "amit_exp1.csv")
  if (!file.exists(path)) return(invisible(NULL))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  labs_ <- paste0(head(GB_CLUSTERS, -1) + 1, "-", GB_CLUSTERS[-1], " faces")
  d <- read.csv(path, stringsAsFactors = FALSE) %>%
    filter(!is.na(rating), !is.na(meanGroup), !is.na(numberFaces)) %>%
    mutate(L = cut(numberFaces + 1, GB_CLUSTERS, labels = labs_),
           cen = paste(id, condition), true = meanGroup, rep = rating)   # centre within participant x valence
  expl_regression_panels(d, "Goldenberg et al. (2021), Exp. 1: number of faces", c(100, 150), 2,
                         file.path(dir, "regression_panels.png"), file.path(dir, "regression_panels.csv"), "exp0_goldenberg")
}

# ---------------------------------------------------------
# Array make-up check: could the configuration of the squares, rather than
# the observer, produce the regression? Reads every displayed array from the
# raw session logs (cached in output/stimulus_audit/) and asks, per level:
#   1. is the recorded true mean the mean of the squares actually shown?
#   2. does the make-up (spread, skew, min, max, squares at the scale ends)
#      depend on the true mean, e.g. arrays clipped or lopsided at the ends?
#   3. at the same true mean, does the make-up predict the report, and does
#      controlling for it change the slope on the true mean?
#   4. are extreme arrays skewed toward the scale centre?
# If none of this holds, arrays with the same mean carry no hidden component
# that could produce Galton-style regression, and the slope reflects the observer.
# ---------------------------------------------------------
CFG_BANDS <- c(-Inf, 17, 21, 28, 31, Inf)   # true-mean bands: 14-17 / 18-21 / 22-28 / 29-31 / 32+
CFG_CENTRE <- 24.5                          # centre of the 14-35/36 true-mean range

expl_array_trials <- function(exp, force = FALSE) {
  cache <- file.path(ARRAY_CACHE_DIR, paste0("array_config_trials_", exp$id, ".csv"))
  if (file.exists(cache) && !force) return(read.csv(cache, stringsAsFactors = FALSE))
  dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
  raw <- bind_rows(lapply(names(exp$dirs), function(cond) {
    fs <- list.files(file.path(DATA, exp$dirs[[cond]]), pattern = "PARTICIPANT.*\\.csv$", full.names = TRUE)
    bind_rows(lapply(fs, .read_raw_arrays)) %>% mutate(condition = cond)
  }))
  vals <- lapply(raw$array_values, function(x)
    suppressWarnings(as.numeric(strsplit(gsub("\\[|\\]|\"", "", x), ",")[[1]])))
  feat <- function(v) {
    v <- v[is.finite(v)]; m <- mean(v); s <- sqrt(mean((v - m)^2))
    c(n_items = length(v), shown_mean = m, item_sd = s, item_min = min(v), item_max = max(v),
      skew = if (s > 0) mean((v - m)^3) / s^3 else 0, n_at_edge = sum(v <= 1 | v >= 49))
  }
  res <- bind_cols(raw %>% select(participant_id, trial, condition, array_length),
                   as.data.frame(do.call(rbind, lapply(vals, feat)))) %>%
    mutate(array_length = suppressWarnings(as.numeric(array_length))) %>%
    # main-task arrays only, as in the stimulus audit
    filter(if (is.null(exp$recode)) n_items == array_length else n_items == 10) %>%
    distinct(participant_id, trial, condition, .keep_all = TRUE)
  write.csv(res, cache, row.names = FALSE)
  res
}

epoc_array_configurations <- function(dat, exp, dir) {
  out <- file.path(dir, "exploratory")
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  labs_ <- expl_level_labels(exp)
  cfg <- expl_array_trials(exp)
  d <- dat %>%
    mutate(condition = as.character(condition),
           raw_id = sub("^[^.]+\\.", "", as.character(participant_id)),
           level = factor(labs_[match(as.character(.data[[exp$noise_var]]), as.character(exp$levels))], levels = labs_)) %>%
    inner_join(cfg %>% select(-array_length) %>% rename(raw_id = participant_id), by = c("raw_id", "trial", "condition")) %>%
    mutate(shown_minus_recorded = shown_mean - meanVal,
           band = cut(meanVal, CFG_BANDS, labels = c("14-17", "18-21", "22-28", "29-31", "32+")),
           reach_up = item_max - meanVal, reach_down = meanVal - item_min,
           inward_skew = ifelse(meanVal < CFG_CENTRE, skew, -skew))   # > 0: long tail toward the scale centre
  matched <- nrow(d) / nrow(dat)

  # 1 + 2: make-up by level and true-mean band
  by_band <- d %>% group_by(level, band) %>%
    summarise(n_trials = n(), item_sd = mean(item_sd), skew = mean(skew), item_min = mean(item_min),
              item_max = mean(item_max), squares_at_edge = mean(n_at_edge),
              shown_minus_recorded = mean(shown_minus_recorded), .groups = "drop")
  shown_vs_recorded <- d %>% group_by(level) %>%
    summarise(mean_diff = mean(shown_minus_recorded), max_abs_diff = max(abs(shown_minus_recorded)),
              r_true_itemsd = suppressWarnings(cor(meanVal, item_sd)), r_true_skew = suppressWarnings(cor(meanVal, skew)),
              .groups = "drop")

  # 3: does make-up predict the report at the same true mean? only features that vary within the level
  models <- bind_rows(lapply(labs_, function(l) {
    x <- d %>% filter(level == l) %>%
      mutate(across(c(skew, reach_up, reach_down, item_sd), ~ .x - mean(.x)))
    feats <- c("skew", "reach_up", "reach_down", "item_sd")
    feats <- feats[vapply(feats, function(f) sd(x[[f]]) > 1e-6, logical(1))]
    m0 <- lmerTest::lmer(indexSelected ~ meanVal + (1 | participant_id), data = x)
    row <- tibble::tibble(level = l, n_trials = nrow(x), slope_without = unname(fixef(m0)["meanVal"]))
    if (!length(feats)) return(row %>% mutate(slope_with = NA_real_, features = "none vary within level"))
    m1 <- lmerTest::lmer(as.formula(paste("indexSelected ~ meanVal +", paste(feats, collapse = " + "), "+ (1 | participant_id)")), data = x)
    cf <- summary(m1)$coefficients
    bind_cols(row %>% mutate(slope_with = unname(fixef(m1)["meanVal"]), features = paste(feats, collapse = ", ")),
              as.data.frame(t(setNames(c(cf[feats, 1], cf[feats, 5]), c(paste0("b_", feats), paste0("p_", feats))))))
  }))

  # 4: skew toward the centre at the extreme bands
  inward <- d %>% filter(band %in% c("14-17", "32+")) %>% group_by(level, band) %>%
    # p first: summarise evaluates in order, and inward_skew is replaced by its mean below
    summarise(n_trials = n(),
              p = if (n() > 1 && sd(inward_skew) > 1e-9) t.test(inward_skew)$p.value else NA_real_,
              inward_skew = mean(inward_skew), .groups = "drop")

  write.csv(by_band, file.path(out, "array_config_by_band.csv"), row.names = FALSE)
  write.csv(shown_vs_recorded, file.path(out, "array_config_shown_vs_recorded.csv"), row.names = FALSE)
  write.csv(models, file.path(out, "array_config_models.csv"), row.names = FALSE)
  write.csv(inward, file.path(out, "array_config_inward_skew.csv"), row.names = FALSE)

  con <- file(file.path(out, "array_configurations.md"), open = "wt"); on.exit(close(con))
  w <- function(...) log_line(con, ...)
  w("# ", exp$label, " - array make-up check (EXPLORATORY)")
  w("")
  w("_Not part of the main write-up. Generated by analysis/run_all.R on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "._")
  w("")
  w("Every displayed array read from the raw session logs; ", sprintf("%.1f%%", 100 * matched),
    " of analysed trials matched. Question: could the make-up of arrays with the same true mean, rather ",
    "than the observer, produce the regression?")
  w("")
  w("## 1-2. Recorded vs shown mean, and make-up by true-mean band")
  w("")
  for (i in seq_len(nrow(shown_vs_recorded))) with(shown_vs_recorded[i, ],
    w(sprintf("- %s: shown - recorded mean = %+.3f (max |diff| %.2f); r(true mean, item SD) = %+.3f; r(true mean, skew) = %+.3f.",
              level, mean_diff, max_abs_diff, r_true_itemsd, r_true_skew)))
  w("")
  w("| level | true mean | trials | item SD | skew | lowest square | highest square | squares at scale ends |")
  w("|---|---|---|---|---|---|---|---|")
  for (i in seq_len(nrow(by_band))) with(by_band[i, ],
    w(sprintf("| %s | %s | %d | %.2f | %+.3f | %.1f | %.1f | %.2f |", level, band, n_trials, item_sd, skew, item_min, item_max, squares_at_edge)))
  w("")
  w("## 3. Does make-up change the report at the same true mean?")
  w("")
  for (i in seq_len(nrow(models))) {
    r <- models[i, ]
    ps <- grep("^p_", names(r), value = TRUE)
    w(sprintf("- %s: slope on true mean %.3f without make-up, %s with it (%s)%s.", r$level, r$slope_without,
              ifelse(is.na(r$slope_with), "n/a", sprintf("%.3f", r$slope_with)), r$features,
              if (length(ps)) paste0("; ", paste(sprintf("%s %s", sub("^p_", "", ps), vapply(ps, function(p) fmt_p(r[[p]]), "")), collapse = ", ")) else ""))
  }
  w("")
  w("## 4. Are extreme arrays skewed toward the scale centre?")
  w("")
  for (i in seq_len(nrow(inward))) with(inward[i, ],
    w(sprintf("- %s, true mean %s: inward skew %+.3f (%d trials), %s.", level, band, inward_skew, n_trials,
              ifelse(is.na(p), "no variation", fmt_p(p)))))
  invisible(list(by_band = by_band, models = models, inward = inward))
}

# ---------------------------------------------------------
# Piecewise reverse model: objective ~ report with a separate slope for low
# reports (<= PW_K1), middle reports and high reports (>= PW_K2), per noise
# level, joined at the knots. A single straight reverse line mixes the
# nearly flat end segments (objective means only span 14-36) with the middle,
# and levels whose reports are more spread out put more trials in the flat
# ends, which hides slope differences in the middle. Random part: each
# participant's own middle slope at each level (uncorrelated), no intercept
# (participants' mean objective value is the same by design). Run on reports as
# given and with each participant's constant shift per level removed. A
# participant-level check fits the same piecewise line per participant x level.
# ---------------------------------------------------------
PW_K1 <- 14.5
PW_K2 <- 35.5

expl_pw_terms <- function(d) d %>%
  mutate(r_mid = pmin(pmax(r, PW_K1), PW_K2) - (PW_K1 + PW_K2) / 2,
         r_low = pmin(r - PW_K1, 0), r_high = pmax(r - PW_K2, 0))

epoc_piecewise_reverse <- function(dat, exp, dir) {
  out <- file.path(dir, "exploratory")
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  base <- expl_add_versions(dat, exp) %>% mutate(level = factor(level))
  contrasts(base$level) <- contr.treatment(nlevels(base$level))
  lv <- levels(base$level); lo <- lv[1]; hi <- lv[length(lv)]
  ctl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  coefs <- list(); tests <- list(); preds <- list(); pts <- list(); partl <- list()

  for (v in c("raw", "shift_removed")) {
    d <- base %>% mutate(r = .data[[paste0("report_", v)]]) %>% expl_pw_terms()
    re <- paste(sprintf("(0 + I(r_mid * (level == '%s')) | participant_id)", lv), collapse = " + ")
    m <- lmerTest::lmer(as.formula(paste("meanVal ~ 0 + level + level:r_mid + level:r_low + level:r_high +", re)),
                        data = d, control = ctl)
    status <- paste0(if (lme4::isSingular(m, tol = 1e-4)) "singular" else "clean",
                     if (length(m@optinfo$conv$lme4$messages)) paste0("; ", paste(m@optinfo$conv$lme4$messages, collapse = "; ")) else "")
    cf <- summary(m)$coefficients
    seg_n <- c(r_low = sum(d$r <= PW_K1), r_mid = sum(d$r > PW_K1 & d$r < PW_K2), r_high = sum(d$r >= PW_K2))
    L <- function(seg, a, b) { x <- setNames(rep(0, nrow(cf)), rownames(cf)); x[paste0("level", a, ":", seg)] <- 1; x[paste0("level", b, ":", seg)] <- -1; x }
    for (seg in c("r_low", "r_mid", "r_high")) {
      rows <- paste0("level", lv, ":", seg)
      coefs[[paste(v, seg)]] <- tibble::tibble(version = v, segment = sub("r_", "", seg), level = lv,
                                               slope = cf[rows, 1], SE = cf[rows, 2], n_trials_segment = seg_n[[seg]])
      joint <- lmerTest::contest(m, do.call(rbind, lapply(lv[-1], function(b) L(seg, lo, b))), joint = TRUE)
      ends  <- lmerTest::contest(m, L(seg, lo, hi), joint = FALSE)
      tests[[paste(v, seg)]] <- tibble::tibble(version = v, segment = sub("r_", "", seg), model_status = status,
                                               level_F = joint$`F value`, level_df2 = joint$DenDF, level_p = joint$`Pr(>F)`,
                                               lowest_minus_highest = ends$Estimate, lowest_minus_highest_p = ends$`Pr(>|t|)`)
    }
    nd <- expand.grid(level = lv, r = seq(2, 48, .5)) %>% mutate(level = factor(level, levels = lv)) %>% expl_pw_terms()
    preds[[v]] <- nd %>% mutate(pred = predict(m, newdata = nd, re.form = NA), version = v)
    pts[[v]] <- d %>% group_by(level, r) %>% summarise(obj = mean(meanVal), n = n(), .groups = "drop") %>%
      filter(n >= EXPL_MIN_CELL) %>% mutate(version = v)

    # participant-level check: the same piecewise line per participant x level
    pl <- d %>% group_by(participant_id, level) %>% group_modify(~ {
      f <- tryCatch(coef(lm(meanVal ~ r_mid + r_low + r_high, data = .x)), error = function(e) rep(NA_real_, 4))
      tibble::tibble(low = unname(f[3]), mid = unname(f[2]), high = unname(f[4])) }) %>% ungroup()
    for (seg in c("low", "mid", "high")) {
      w <- pl %>% select(participant_id, level, val = all_of(seg)) %>% tidyr::pivot_wider(names_from = level, values_from = val)
      ok <- is.finite(w[[lo]]) & is.finite(w[[hi]])
      t <- if (sum(ok) > 2) t.test(w[[lo]][ok], w[[hi]][ok], paired = TRUE) else NULL
      partl[[paste(v, seg)]] <- tibble::tibble(version = v, segment = seg, n_participants = sum(ok),
        lowest_minus_highest = if (is.null(t)) NA_real_ else unname(t$estimate),
        p = if (is.null(t)) NA_real_ else t$p.value)
    }
  }
  coefs <- bind_rows(coefs) %>% mutate(experiment = exp$id, .before = 1)
  tests <- bind_rows(tests) %>% mutate(experiment = exp$id, .before = 1)
  partl <- bind_rows(partl) %>% mutate(experiment = exp$id, .before = 1)
  write.csv(coefs, file.path(out, "piecewise_reverse_slopes.csv"), row.names = FALSE)
  write.csv(tests, file.path(out, "piecewise_reverse_tests.csv"), row.names = FALSE)
  write.csv(partl, file.path(out, "piecewise_reverse_participant_level.csv"), row.names = FALSE)

  cols <- setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")[seq_along(lv)], lv)
  vlab <- c(raw = "Reports as given", shift_removed = "Each participant's constant shift removed")
  fig <- ggplot() +
    annotate("rect", xmin = -Inf, xmax = PW_K1, ymin = -Inf, ymax = Inf, fill = "grey93") +
    annotate("rect", xmin = PW_K2, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "grey93") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
    geom_point(data = bind_rows(pts), aes(r, obj, colour = level), size = 1.3, alpha = .45) +
    geom_line(data = bind_rows(preds), aes(r, pred, colour = level), linewidth = 1) +
    facet_wrap(~ version, labeller = as_labeller(vlab)) +
    scale_colour_manual(values = cols, name = NULL) + scale_x_continuous(breaks = seq(5, 45, 5)) +
    coord_cartesian(xlim = c(2, 48), ylim = c(10, 40)) +
    labs(x = "Report", y = "Objective mean behind the report",
         title = paste0(sub(" - .*", "", exp$label), ": piecewise reverse model"),
         subtitle = paste0("EXPLORATORY. Separate slope below ", PW_K1, ", between, and above ", PW_K2,
                           " (shaded), per level. Dots: observed means; dashed: report = objective")) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "top", plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey40", size = 9.5), strip.text = element_text(face = "bold"))
  epoc_save(fig, file.path(out, "piecewise_reverse.png"), width = 11, height = 5.8, dpi = 200)

  con <- file(file.path(out, "piecewise_reverse.md"), open = "wt"); on.exit(close(con))
  w <- function(...) log_line(con, ...)
  w("# ", exp$label, " - piecewise reverse model (EXPLORATORY)")
  w("")
  w("_Not part of the main write-up. Generated by analysis/run_all.R on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "._")
  w("")
  w("`meanVal ~ 0 + level + level:(r_mid + r_low + r_high)`, one line per level with knots at ", PW_K1, " and ", PW_K2,
    "; each participant's own middle slope per level (uncorrelated, no intercept). A lower slope at the noisier level ",
    "means the objective value changed less per unit of report there, i.e. more regression (more exaggeration).")
  w("")
  for (v in c("raw", "shift_removed")) {
    w("## ", vlab[[v]])
    w("")
    w("| segment | trials | ", paste(lv, collapse = " | "), " | level effect | ", lo, " - ", hi, " | participant-level ", lo, " - ", hi, " |")
    w("|---|---|", paste(rep("---|", length(lv)), collapse = ""), "---|---|---|")
    for (seg in c("low", "mid", "high")) {
      cc <- coefs %>% filter(version == v, segment == seg); tt <- tests %>% filter(version == v, segment == seg)
      pp <- partl %>% filter(version == v, segment == seg)
      w("| ", seg, " | ", cc$n_trials_segment[1], " | ", paste(sprintf("%.3f", cc$slope), collapse = " | "), " | ",
        sprintf("F(%d, %.0f) = %.2f, %s", length(lv) - 1, tt$level_df2, tt$level_F, fmt_p(tt$level_p)), " | ",
        sprintf("%+.3f, %s", tt$lowest_minus_highest, fmt_p(tt$lowest_minus_highest_p)), " | ",
        if (is.na(pp$p)) "n/a" else sprintf("%+.3f, %s (n = %d)", pp$lowest_minus_highest, fmt_p(pp$p), pp$n_participants), " |")
    }
    w("")
    w("Model: ", tests$model_status[tests$version == v][1], ".")
    w("")
  }
  w("Only the middle slope has a per-participant random effect; the low and high segment tests therefore use ",
    "trial-level degrees of freedom and may be optimistic, so rely on the participant-level column for those. ",
    "The segments are joined at the knots, so an abrupt step exactly at a knot shows in the dots but not the lines; ",
    "the tail tests in `exploratory.md` capture that part.")
  invisible(list(slopes = coefs, tests = tests, participant = partl))
}

# ---------------------------------------------------------
# Report bands: extreme reports grouped into four bands, and the objective
# values behind them per noise level. "gap" = how much less extreme the
# stimulus was than the report (objective - report for low bands, report -
# objective for high bands). Within each band the lowest and highest noise
# levels are compared at the participant level, both on the raw gap and on
# trials with the same report value (deviation from the band's report-value
# mean), since levels can differ in which reports fall inside a band.
# ---------------------------------------------------------
BAND_BREAKS <- c(0, 9, 14, 35, 40, 50)
BAND_LABELS <- c("extreme low (1-9)", "low (10-14)", "middle", "high (36-40)", "extreme high (41-50)")

epoc_report_bands <- function(dat, exp, dir) {
  out <- file.path(dir, "exploratory")
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  base <- expl_add_versions(dat, exp)
  lv <- levels(droplevels(factor(base$level))); lo <- lv[1]; hi <- lv[length(lv)]
  tabs <- list(); tests <- list()
  for (v in c("raw", "shift_removed")) {
    d <- base %>% mutate(r = .data[[paste0("report_", v)]],
                         band = cut(r, BAND_BREAKS, labels = BAND_LABELS)) %>%
      filter(!is.na(band), band != "middle") %>%
      mutate(sgn = ifelse(grepl("low", band), 1, -1), gap = sgn * (meanVal - r)) %>% droplevels()
    tabs[[v]] <- d %>% group_by(band, level) %>%
      summarise(n_trials = n(), n_participants = n_distinct(participant_id),
                mean_report = mean(r), sd_report = sd(r), mean_objective = mean(meanVal), sd_objective = sd(meanVal),
                gap = mean(gap), .groups = "drop") %>% mutate(version = v, .before = 1)
    for (b in levels(d$band)) {
      x <- d %>% filter(band == b) %>% group_by(r) %>% mutate(dev = sgn * (meanVal - mean(meanVal))) %>% ungroup()
      pl <- x %>% group_by(participant_id, level) %>% summarise(gap = mean(gap), dev = mean(dev), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = level, values_from = c(gap, dev))
      g_lo <- paste0("gap_", lo); g_hi <- paste0("gap_", hi); d_lo <- paste0("dev_", lo); d_hi <- paste0("dev_", hi)
      ok <- if (all(c(g_lo, g_hi) %in% names(pl))) !is.na(pl[[g_lo]]) & !is.na(pl[[g_hi]]) else FALSE
      tt <- function(a, b) if (sum(ok) > 2) t.test(pl[[a]][ok], pl[[b]][ok], paired = TRUE) else NULL
      tg <- tt(g_hi, g_lo); td <- tt(d_hi, d_lo)
      tests[[paste(v, b)]] <- tibble::tibble(version = v, band = b, comparison = paste(hi, "vs", lo), n_participants = sum(ok),
        gap_diff = if (is.null(tg)) NA_real_ else unname(tg$estimate), gap_p = if (is.null(tg)) NA_real_ else tg$p.value,
        same_report_diff = if (is.null(td)) NA_real_ else unname(td$estimate), same_report_p = if (is.null(td)) NA_real_ else td$p.value)
    }
  }
  tabs <- bind_rows(tabs) %>% mutate(experiment = exp$id, .before = 1)
  tests <- bind_rows(tests) %>% mutate(experiment = exp$id, .before = 1)
  write.csv(tabs, file.path(out, "report_bands.csv"), row.names = FALSE)
  write.csv(tests, file.path(out, "report_bands_tests.csv"), row.names = FALSE)

  con <- file(file.path(out, "report_bands.md"), open = "wt"); on.exit(close(con))
  w <- function(...) log_line(con, ...)
  w("# ", exp$label, " - extreme reports in four bands (EXPLORATORY)")
  w("")
  w("_Not part of the main write-up. Generated by analysis/run_all.R on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "._")
  w("")
  w("Bands: extreme low 1-9, low 10-14, high 36-40, extreme high 41-50. `gap` = how much less extreme the objective ",
    "value was than the report. Tests compare ", hi, " with ", lo, " within each band, participant-level and paired; ",
    "the same-report test compares only trials with the same report value. Positive = the stimulus behind the report ",
    "was less extreme at the noisier level (more exaggeration).")
  w("")
  for (v in c("raw", "shift_removed")) {
    w("## ", if (v == "raw") "Reports as given" else "Each participant's constant shift removed")
    w("")
    w("| band | level | trials | participants | mean report (SD) | mean objective (SD) | gap |")
    w("|---|---|---|---|---|---|---|")
    tv <- tabs %>% filter(version == v)
    for (i in seq_len(nrow(tv))) with(tv[i, ], w(sprintf("| %s | %s | %d | %d | %.2f (%.2f) | %.2f (%.2f) | %.2f |",
      band, level, n_trials, n_participants, mean_report, sd_report, mean_objective, sd_objective, gap)))
    w("")
    w("| band | participants | gap difference | same-report difference |")
    w("|---|---|---|---|")
    tt <- tests %>% filter(version == v)
    for (i in seq_len(nrow(tt))) with(tt[i, ], w(sprintf("| %s | %d | %s | %s |", band, n_participants,
      ifelse(is.na(gap_p), "n/a", sprintf("%+.2f, %s", gap_diff, fmt_p(gap_p))),
      ifelse(is.na(same_report_p), "n/a", sprintf("%+.2f, %s", same_report_diff, fmt_p(same_report_p))))))
    w("")
  }
  w("Objective means only span about 14-35, so the outermost bands are squeezed against the ends of the stimulus range; ",
    "they also rest on fewer participants.")
  invisible(list(table = tabs, tests = tests))
}

# ---------------------------------------------------------
# True value -> report: the forward counterpart of the report-to-true table.
# For every true value (rounded) and noise level: trials, mean report, SD of
# the reports and bias (report - true). Extreme true values (<= TR_LOW,
# >= TR_HIGH) are compared between the lowest and highest noise level at the
# participant level, on trials with the same true value: positive = the report
# was pulled further toward the middle at the noisier level (compression).
# The SD of reports here includes differences between participants.
# ---------------------------------------------------------
TR_LOW  <- 17
TR_HIGH <- 32

epoc_true_to_report <- function(dat, exp, dir) {
  out <- file.path(dir, "exploratory")
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  labs_ <- expl_level_labels(exp); lo <- labs_[1]; hi <- labs_[length(labs_)]
  d <- dat %>% mutate(level = factor(labs_[match(as.character(.data[[exp$noise_var]]), as.character(exp$levels))], levels = labs_),
                      tv = round(meanVal))
  values <- d %>% group_by(level, true = tv) %>%
    summarise(n = n(), mean_report = mean(indexSelected), sd_report = if (n() > 1) sd(indexSelected) else NA_real_, .groups = "drop") %>%
    mutate(bias = mean_report - true, experiment = exp$id, .before = 1)
  zones <- values %>% mutate(zone = case_when(true <= TR_LOW ~ paste0("low (<= ", TR_LOW, ")"), true >= TR_HIGH ~ paste0("high (>= ", TR_HIGH, ")"),
                                              TRUE ~ "middle")) %>%
    filter(is.finite(sd_report)) %>% group_by(experiment, zone, level) %>%
    summarise(n_trials = sum(n), sd_report = weighted.mean(sd_report, n), bias = weighted.mean(bias, n), .groups = "drop")
  tests <- bind_rows(lapply(c("low", "high"), function(side) {
    x <- d %>% filter(if (side == "low") tv <= TR_LOW else tv >= TR_HIGH) %>%
      group_by(tv) %>% mutate(dev = (if (side == "low") 1 else -1) * (indexSelected - mean(indexSelected))) %>% ungroup()
    pl <- x %>% group_by(participant_id, level) %>% summarise(dev = mean(dev), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = level, values_from = dev)
    ok <- all(c(lo, hi) %in% names(pl))
    t <- if (ok) t.test(pl[[hi]], pl[[lo]], paired = TRUE) else NULL
    tibble::tibble(experiment = exp$id, extreme_true = side, comparison = paste(hi, "vs", lo),
                   n_participants = if (ok) sum(!is.na(pl[[hi]]) & !is.na(pl[[lo]])) else 0L,
                   pulled_to_middle_more = if (ok) unname(t$estimate) else NA_real_, p = if (ok) t$p.value else NA_real_)
  }))
  write.csv(values, file.path(out, "true_to_report_values.csv"), row.names = FALSE)
  write.csv(zones,  file.path(out, "true_to_report_zones.csv"),  row.names = FALSE)
  write.csv(tests,  file.path(out, "true_to_report_tests.csv"),  row.names = FALSE)

  con <- file(file.path(out, "true_to_report.md"), open = "wt"); on.exit(close(con))
  w <- function(...) log_line(con, ...)
  w("# ", exp$label, " - average report for each true value (EXPLORATORY)")
  w("")
  w("_Not part of the main write-up. Generated by analysis/run_all.R on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "._")
  w("")
  w("Each cell: mean report (SD of reports, trials). The SD of reports includes differences between participants.")
  w("")
  wide <- values %>% mutate(cell = sprintf("%.1f (%.1f, %d)", mean_report, sd_report, n)) %>% select(true, level, cell) %>%
    tidyr::pivot_wider(names_from = level, values_from = cell)
  w("| true value | ", paste(labs_, collapse = " | "), " |")
  w("|---|", paste(rep("---|", length(labs_)), collapse = ""))
  for (i in seq_len(nrow(wide))) w("| ", wide$true[i], " | ", paste(unlist(wide[i, labs_]), collapse = " | "), " |")
  w("")
  w("| zone | ", paste(paste("SD of reports", labs_), collapse = " | "), " | ", paste(paste("bias", labs_), collapse = " | "), " |")
  w("|---|", paste(rep("---|", 2 * length(labs_)), collapse = ""))
  for (z in unique(zones$zone)) { zz <- zones %>% filter(zone == z)
    w("| ", z, " | ", paste(sprintf("%.2f", zz$sd_report), collapse = " | "), " | ", paste(sprintf("%+.2f", zz$bias), collapse = " | "), " |") }
  w("")
  for (i in seq_len(nrow(tests))) with(tests[i, ], w(sprintf("- %s true values: at %s the report was pulled toward the middle by %+.2f more than at %s (%d participants, %s).",
    extreme_true, hi, pulled_to_middle_more, lo, n_participants, ifelse(is.na(p), "n/a", fmt_p(p)))))
  invisible(list(values = values, zones = zones, tests = tests))
}
