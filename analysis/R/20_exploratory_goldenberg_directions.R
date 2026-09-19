# =========================================================
# 20_exploratory_goldenberg_directions.R -- EXPLORATORY, not part of the write-up
#
# Goldenberg et al. (2021), Exp. 1 (data/amit_exp1.csv), in both directions:
#
#   forward  rating ~ true mean: for the same group, is the rating pulled
#            toward the middle more when more faces are shown?
#   reverse  true mean ~ rating: for the same rating, is the group behind it
#            less extreme when more faces are shown (exaggeration)?
#
# The two slopes are forward = r x SD ratio and reverse = r / SD ratio
# (SD ratio = SD of ratings / SD of true means), so both are reported next to
# r and the SDs, per number of faces (1-12) and per cluster (1-4, 5-8, 9-12).
#
# Noise is manipulated by the number of faces, with the spread of each 12-face
# crowd held constant by design (realSD about 9.9; each array shows a subset of
# its crowd). A by-product is that the spread of true means *across trials*
# differs by set size: about 12.6 with 1 face, 8.3 with 12 (in EPoC Experiments
# 1-3 it is flat, about 6.5 at every array size). r and the reverse slope
# depend on that spread, the forward slope much less. So every descriptive is
# also given with each set size restricted to the range of true means every
# set size covers and reweighted to the same distribution within it
# ("matched"), which makes the comparison equivalent to Experiments 1-3.
#
# Values centred within participant x valence, as in the other Goldenberg
# analyses. Mixed models are fitted per valence with a random slope:
#   forward  rating    ~ true_c   * faces + (1 + true_c   | id)
#   reverse  meanGroup ~ rating_c * faces + (1 + rating_c | id)
# with faces as a factor (slope at each set size) and as a number (linear test).
#
# Output: output/exp0_goldenberg/exploratory/directions_*.png / .csv and
# directions.md.
# =========================================================

GBD_BIN   <- 2    # bin width (scale units) for binned plots and for matching distributions
GBD_MIN_N <- 15   # binned points with fewer trials are not plotted

gbd_load <- function() {
  path <- file.path(DATA, "amit_exp1.csv")
  if (!file.exists(path)) return(NULL)
  labs_ <- paste0(head(GB_CLUSTERS, -1) + 1, "-", GB_CLUSTERS[-1], " faces")
  read.csv(path, stringsAsFactors = FALSE) %>%
    filter(!is.na(rating), !is.na(meanGroup), !is.na(numberFaces)) %>%
    mutate(faces = numberFaces + 1, cluster = cut(faces, GB_CLUSTERS, labels = labs_),
           id = factor(id), valence = condition, cen = paste(id, condition)) %>%
    group_by(cen) %>%
    mutate(true_c = meanGroup - mean(meanGroup), rating_c = rating - mean(rating)) %>%
    ungroup()
}

# weights that give every level the same distribution of true means: only bins
# that every level covers (>= GBD_MIN_CELL trials) are kept, and within them each
# level is reweighted to the pooled distribution; trials outside get weight 0
GBD_MIN_CELL <- 5
gbd_match_weights <- function(d, level) {
  d <- d %>% mutate(.lev = .data[[level]], .bin = floor(meanGroup / GBD_BIN))
  nlev <- n_distinct(d$.lev)
  common <- d %>% count(.lev, .bin) %>% filter(n >= GBD_MIN_CELL) %>% count(.bin, name = "k") %>%
    filter(k == nlev) %>% pull(.bin)
  dc <- d %>% filter(.bin %in% common)
  target <- dc %>% count(.bin) %>% mutate(p_target = n / sum(n)) %>% select(-n)
  cell <- dc %>% count(.lev, .bin, name = "n_cell") %>% group_by(.lev) %>% mutate(p_level = n_cell / sum(n_cell)) %>% ungroup()
  d %>% left_join(cell, by = c(".lev", ".bin")) %>% left_join(target, by = ".bin") %>%
    mutate(w = ifelse(.bin %in% common, p_target / p_level, 0)) %>% pull(w)
}

gbd_wstats <- function(x, y, w) {
  w <- w / sum(w); mx <- sum(w * x); my <- sum(w * y)
  vx <- sum(w * (x - mx)^2); vy <- sum(w * (y - my)^2); cxy <- sum(w * (x - mx) * (y - my))
  tibble(sd_true = sqrt(vx), sd_rating = sqrt(vy), sd_ratio = sqrt(vy / vx), r = cxy / sqrt(vx * vy),
         forward = cxy / vx, reverse = cxy / vy)
}

gbd_descriptives <- function(d) {
  one <- function(lev_var) {
    wm <- gbd_match_weights(d, lev_var)
    dd <- d %>% mutate(level = as.character(.data[[lev_var]]), w_match = wm)
    bind_rows(
      dd %>% group_by(level) %>% group_modify(~ bind_cols(tibble(n_trials = nrow(.x)), gbd_wstats(.x$true_c, .x$rating_c, rep(1, nrow(.x))))) %>%
        mutate(weighting = "as shown"),
      dd %>% filter(w_match > 0) %>% group_by(level) %>% group_modify(~ bind_cols(tibble(n_trials = nrow(.x)), gbd_wstats(.x$true_c, .x$rating_c, .x$w_match))) %>%
        mutate(weighting = "matched")
    ) %>% ungroup()
  }
  bind_rows(one("faces") %>% mutate(grouping = "number of faces"),
            one("cluster") %>% mutate(grouping = "cluster")) %>%
    relocate(grouping, weighting, level)
}

# per participant, 1-4 vs 9-12 faces, both valences pooled (centred within valence)
gbd_participant_tests <- function(d) {
  labs_ <- levels(d$cluster); lo <- labs_[1]; hi <- labs_[length(labs_)]
  pp <- d %>% filter(cluster %in% c(lo, hi)) %>% group_by(id, cluster) %>%
    summarise(n = n(), r = cor(true_c, rating_c), sd_true = sd(true_c), sd_rating = sd(rating_c), .groups = "drop") %>%
    mutate(z = atanh(r), log_ratio = log(sd_rating / sd_true), forward = r * sd_rating / sd_true, reverse = r * sd_true / sd_rating)
  bind_rows(lapply(c("z", "log_ratio", "forward", "reverse", "sd_true", "sd_rating"), function(v) {
    wd <- pp %>% select(id, cluster, val = all_of(v)) %>% tidyr::pivot_wider(names_from = cluster, values_from = val) %>%
      filter(is.finite(.data[[lo]]), is.finite(.data[[hi]]))
    tt <- t.test(wd[[hi]], wd[[lo]], paired = TRUE)
    tibble(measure = v, low = mean(wd[[lo]]), high = mean(wd[[hi]]), difference = unname(tt$estimate),
           n = nrow(wd), p = tt$p.value)
  })) %>% mutate(comparison = paste(hi, "vs", lo), .before = 1)
}

gbd_models <- function(d) {
  specs <- list(forward = list(y = "rating", x = "true_c"), reverse = list(y = "meanGroup", x = "rating_c"))
  out <- list(); tests <- list()
  for (dir_ in names(specs)) for (v in c("positive", "negative")) {
    s <- specs[[dir_]]; dd <- d %>% filter(valence == v) %>% mutate(faces_f = factor(faces))
    ff <- as.formula(paste0(s$y, " ~ ", s$x, " * faces_f + (1 + ", s$x, " | id)"))
    fl <- as.formula(paste0(s$y, " ~ ", s$x, " * faces + (1 + ", s$x, " | id)"))
    ctl <- lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
    mf <- lmerTest::lmer(ff, data = dd, control = ctl); ml <- lmerTest::lmer(fl, data = dd, control = ctl)
    conv_msg <- function(m) paste(c(m@optinfo$conv$lme4$messages, if (lme4::isSingular(m, tol = 1e-4)) "singular"), collapse = "; ")
    out[[paste(dir_, v)]] <- as.data.frame(summary(
      emtrends(mf, ~ faces_f, var = s$x, lmer.df = "asymptotic"), infer = c(TRUE, TRUE))) %>%
      rename(slope = 2) %>%
      rename_with(~ "ci_low",  any_of(c("lower.CL", "asymp.LCL"))) %>%
      rename_with(~ "ci_high", any_of(c("upper.CL", "asymp.UCL"))) %>%
      transmute(direction = dir_, valence = v, faces = as.numeric(as.character(faces_f)), slope, SE, ci_low, ci_high)
    cf <- coef(summary(ml)); term <- paste0(s$x, ":faces")
    tests[[paste(dir_, v)]] <- tibble(direction = dir_, valence = v, change_per_face = cf[term, "Estimate"],
                                      SE = cf[term, "Std. Error"], df = cf[term, "df"], p = cf[term, "Pr(>|t|)"],
                                      fit_notes_linear = conv_msg(ml), fit_notes_by_set_size = conv_msg(mf))
  }
  list(slopes = bind_rows(out), tests = bind_rows(tests))
}

# forward at the extremes: same true value, is the rating pulled toward the middle more with more faces?
gbd_forward_extremes <- function(d) {
  labs_ <- levels(d$cluster)
  x <- d %>% mutate(side = case_when(meanGroup <= GB_TAIL_LOW ~ "low", meanGroup >= GB_TAIL_HIGH ~ "high")) %>%
    filter(!is.na(side)) %>%
    mutate(pull = ifelse(side == "low", rating - meanGroup, meanGroup - rating), tv = factor(round(meanGroup)))
  cells <- x %>% group_by(side, cluster) %>%
    summarise(n_trials = n(), n_participants = n_distinct(id), mean_true = mean(meanGroup),
              mean_rating = mean(rating), pull = mean(pull), .groups = "drop")
  tests <- bind_rows(lapply(c("low", "high"), function(s) {
    m <- lmerTest::lmer(pull ~ cluster + tv + (1 | id), data = x %>% filter(side == s))
    as.data.frame(summary(pairs(emmeans(m, ~ cluster, lmer.df = "satterthwaite"), reverse = TRUE))) %>%
      mutate(side = s, cluster_effect_p = anova(m)["cluster", "Pr(>F)"], .before = 1)
  }))
  list(cells = cells, tests = tests)
}

gbd_figure <- function(d, slopes, direction) {
  labs_ <- levels(d$cluster)
  cols <- setNames(c("#2a78d6", "#eb6834", "#1baf7a"), labs_)
  fwd <- direction == "forward"
  dd <- d %>% mutate(x = if (fwd) meanGroup else rating, y = if (fwd) rating else meanGroup)
  pts <- dd %>% mutate(xb = round(x / GBD_BIN) * GBD_BIN) %>% group_by(cluster, xb) %>%
    summarise(y = mean(y), n = n(), .groups = "drop") %>% filter(n >= GBD_MIN_N)
  p1 <- ggplot(dd, aes(x, y, colour = cluster)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55", linewidth = .5) +
    geom_point(data = pts, aes(xb, y), size = 1.4, alpha = .6) +
    geom_smooth(method = "lm", se = FALSE, linewidth = .9, formula = y ~ x) +
    scale_colour_manual(values = cols, name = NULL) +
    coord_equal(xlim = c(100, 150), ylim = c(100, 150)) +
    labs(x = if (fwd) "True mean of the faces shown" else "Rating",
         y = if (fwd) "Mean rating" else "Mean true value behind the rating",
         title = if (fwd) "Average rating for each true value" else "Average true value behind each rating") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), legend.position = "top", plot.title = element_text(face = "bold", size = 11.5))
  sl <- slopes %>% filter(direction == !!direction)
  p2 <- ggplot(sl, aes(faces, slope, colour = valence)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey55") +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = .45), size = .3) +
    geom_smooth(method = "lm", se = FALSE, linewidth = .6, formula = y ~ x) +
    scale_colour_manual(values = c(positive = "#c0392b", negative = "#34495e"), name = NULL) +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "Number of faces", y = if (fwd) "Forward slope (rating on true mean)" else "Reverse slope (true mean on rating)",
         title = "Mixed-model slope at each set size (95% CI)") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), legend.position = "top", plot.title = element_text(face = "bold", size = 11.5))
  patchwork::wrap_plots(list(p1, p2), ncol = 2, widths = c(1, 1.1)) +
    patchwork::plot_annotation(
      title = paste0("Goldenberg et al. (2021), Exp. 1: ", if (fwd) "forward" else "reverse", " direction"),
      subtitle = paste0("EXPLORATORY. Left: dots = binned means (", GBD_BIN, "-unit bins, n >= ", GBD_MIN_N,
                        "), lines = least squares per cluster, valences pooled. Right: per valence. Dashed: slope 1"),
      theme = theme(plot.title = element_text(face = "bold", size = 14), plot.subtitle = element_text(colour = "grey40", size = 9.5)))
}

gbd_r_ratio_figure <- function(desc) {
  x <- desc %>% filter(grouping == "number of faces") %>% mutate(faces = as.numeric(level)) %>%
    select(faces, weighting, r, sd_true, sd_rating, sd_ratio, forward, reverse) %>%
    tidyr::pivot_longer(c(r, sd_true, sd_rating, sd_ratio, forward, reverse), names_to = "measure") %>%
    mutate(measure = factor(measure, levels = c("r", "sd_ratio", "sd_true", "sd_rating", "forward", "reverse"),
                            labels = c("Correlation r", "SD ratio (ratings / true)", "SD of true means", "SD of ratings",
                                       "Forward slope = r x SD ratio", "Reverse slope = r / SD ratio")))
  ggplot(x, aes(faces, value, colour = weighting, linetype = weighting)) +
    geom_line(linewidth = .8) + geom_point(size = 1.4) +
    facet_wrap(~ measure, scales = "free_y", ncol = 2) +
    scale_colour_manual(values = c("as shown" = "#2a78d6", matched = "#eb6834"), name = NULL) +
    scale_linetype_manual(values = c("as shown" = "solid", matched = "22"), name = NULL) +
    scale_x_continuous(breaks = 1:12) +
    labs(x = "Number of faces", y = NULL,
         title = "Goldenberg et al. (2021), Exp. 1: r, SDs and slopes by number of faces",
         subtitle = paste("EXPLORATORY. Centred within participant x valence, trials pooled.",
                          "'matched' = common range of true means, each set size reweighted to the same distribution")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), legend.position = "top",
          plot.title = element_text(face = "bold", size = 13), plot.subtitle = element_text(colour = "grey40", size = 9.5))
}

epoc_goldenberg_directions <- function(dir = file.path(OUT, "exp0_goldenberg", "exploratory")) {
  d <- gbd_load()
  if (is.null(d)) { message("amit_exp1.csv not found; skipping the forward/reverse analysis"); return(invisible(NULL)) }
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  desc <- gbd_descriptives(d)
  pt   <- gbd_participant_tests(d)
  mods <- gbd_models(d)
  ext  <- gbd_forward_extremes(d)

  write.csv(desc,        file.path(dir, "directions_descriptives.csv"),       row.names = FALSE)
  write.csv(pt,          file.path(dir, "directions_participant_tests.csv"),  row.names = FALSE)
  write.csv(mods$slopes, file.path(dir, "directions_model_slopes.csv"),       row.names = FALSE)
  write.csv(mods$tests,  file.path(dir, "directions_model_tests.csv"),        row.names = FALSE)
  write.csv(ext$cells,   file.path(dir, "directions_forward_extremes.csv"),   row.names = FALSE)
  write.csv(ext$tests,   file.path(dir, "directions_forward_extremes_tests.csv"), row.names = FALSE)

  epoc_save(gbd_figure(d, mods$slopes, "forward"), file.path(dir, "directions_forward.png"), width = 12, height = 6, dpi = 170)
  epoc_save(gbd_figure(d, mods$slopes, "reverse"), file.path(dir, "directions_reverse.png"), width = 12, height = 6, dpi = 170)
  epoc_save(gbd_r_ratio_figure(desc), file.path(dir, "directions_r_sd_ratio.png"), width = 10, height = 10, dpi = 170)

  con <- file(file.path(dir, "directions.md"), open = "wt"); on.exit(close(con))
  w <- function(...) log_line(con, ...)
  f <- function(x, k = 2) formatC(x, format = "f", digits = k)
  w("# Goldenberg et al. (2021), Exp. 1 - forward and reverse directions (EXPLORATORY)")
  w("")
  w("_Not part of the main write-up. Generated by analysis/run_all.R on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "._")
  w("")
  w("N = ", n_distinct(d$id), " participants, ", nrow(d), " trials. Forward = rating on true mean; reverse = true mean on rating. ",
    "Forward slope = r x SD ratio, reverse slope = r / SD ratio (SD ratio = SD of ratings / SD of true means). ",
    "Values centred within participant x valence.")
  w("")
  w("**Spread of true means across trials.** Noise is the number of faces; each crowd's spread is constant by design. ",
    "Because each array shows a subset of its 12-face crowd, the spread of true means across trials is wider for small set sizes ",
    "(about 12.6 with 1 face, 8.3 with 12), and r and the reverse slope depend on that spread. 'Matched' rows keep only the range of ",
    "true means that every level covers and reweight each level to the same distribution within it (trials = those kept), ",
    "as in Experiments 1-3, where this spread is the same at every array size.")
  w("")
  w("## By cluster")
  w("")
  w("| cluster | weighting | trials | SD of true means | SD of ratings | SD ratio | r | forward slope | reverse slope |")
  w("|---|---|---|---|---|---|---|---|---|")
  cl <- desc %>% filter(grouping == "cluster") %>% arrange(weighting, level)
  for (i in seq_len(nrow(cl))) with(cl[i, ], w("| ", level, " | ", weighting, " | ", n_trials, " | ", f(sd_true), " | ", f(sd_rating), " | ",
                                               f(sd_ratio, 3), " | ", f(r, 3), " | ", f(forward, 3), " | ", f(reverse, 3), " |"))
  w("")
  w("## Per participant, ", pt$comparison[1])
  w("")
  w("| measure | 1-4 faces | 9-12 faces | difference | n | p |")
  w("|---|---|---|---|---|---|")
  for (i in seq_len(nrow(pt))) with(pt[i, ], w("| ", measure, " | ", f(low, 3), " | ", f(high, 3), " | ", sprintf("%+.3f", difference), " | ", n, " | ", fmt_p(p), " |"))
  w("")
  w("z = Fisher z of r; log_ratio = log(SD ratio).")
  w("")
  w("## Mixed models: change in slope per extra face")
  w("")
  w("| direction | valence | change per face | SE | p | fit notes (linear / by set size) |")
  w("|---|---|---|---|---|---|")
  for (i in seq_len(nrow(mods$tests))) with(mods$tests[i, ], w("| ", direction, " | ", valence, " | ", sprintf("%+.4f", change_per_face), " | ", f(SE, 4), " | ", fmt_p(p), " | ",
                                                                ifelse(nzchar(fit_notes_linear), fit_notes_linear, "ok"), " / ", ifelse(nzchar(fit_notes_by_set_size), fit_notes_by_set_size, "ok"), " |"))
  w("")
  sl <- mods$slopes %>% filter(faces %in% c(1, 4, 8, 12))
  w("Slope at 1 / 4 / 8 / 12 faces (per valence):")
  w("")
  for (dir_ in c("forward", "reverse")) for (v in c("positive", "negative")) {
    s <- sl %>% filter(direction == dir_, valence == v)
    w("- ", dir_, ", ", v, ": ", paste(sprintf("%.2f", s$slope), collapse = " / "), ".")
  }
  w("")
  w("## Forward at the extremes: same true value")
  w("")
  w("Pull toward the middle = rating - true on the low side (true <= ", GB_TAIL_LOW, "), true - rating on the high side (true >= ",
    GB_TAIL_HIGH, "); compared between clusters at the same (rounded) true value, with a random intercept per participant.")
  w("")
  w("| side | cluster | trials | participants | mean true | mean rating | pull toward middle |")
  w("|---|---|---|---|---|---|---|")
  for (i in seq_len(nrow(ext$cells))) with(ext$cells[i, ], w("| ", side, " | ", as.character(cluster), " | ", n_trials, " | ", n_participants, " | ",
                                                             f(mean_true, 1), " | ", f(mean_rating, 1), " | ", sprintf("%+.2f", pull), " |"))
  w("")
  for (s in c("low", "high")) {
    tt <- ext$tests %>% filter(side == s)
    w("- ", s, " side: effect of cluster ", fmt_p(tt$cluster_effect_p[1]), ".")
    for (i in seq_len(nrow(tt))) w("  - ", tt$contrast[i], ": ", sprintf("%+.2f", tt$estimate[i]), ", ", fmt_p(tt$p.value[i]), ".")
  }
  invisible(list(descriptives = desc, participant_tests = pt, models = mods, extremes = ext))
}
