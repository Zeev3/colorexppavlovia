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
# directions.md. The extremes (both directions) are analysed on all trials.
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

# The extremes, on all trials (no matching; matching drops most extreme trials).
#   forward  same extreme TRUE value (<= GB_TAIL_LOW or >= GB_TAIL_HIGH):
#            pull toward the middle = rating - true (low side), true - rating (high side)
#   reverse  same extreme RATING: exaggeration = how much less extreme the true
#            value is than the rating = true - rating (low side), rating - true (high side)
# Clusters are compared at the same rounded value (value as a factor) with a
# random intercept per participant, per side, and for both sides together,
# where a constant shift of the ratings cancels out (the average of the two sides).
gbd_extremes <- function(d) {
  one <- function(direction) {
    x <- d %>% mutate(v = if (direction == "forward") meanGroup else rating,
                      side = case_when(v <= GB_TAIL_LOW ~ "low", v >= GB_TAIL_HIGH ~ "high")) %>%
      filter(!is.na(side)) %>%
      mutate(sgn = ifelse(side == "low", 1, -1),
             effect = if (direction == "forward") sgn * (rating - meanGroup) else sgn * (meanGroup - rating),
             value = factor(paste(side, round(v))))
    cells <- x %>% group_by(side, cluster) %>%
      summarise(n_trials = n(), n_participants = n_distinct(id), mean_true = mean(meanGroup),
                mean_rating = mean(rating), effect = mean(effect), .groups = "drop")
    ps <- lapply(c("low", "high"), function(s) {
      m <- lmerTest::lmer(effect ~ cluster + value + (1 | id), data = x %>% filter(side == s))
      em <- emmeans(m, ~ cluster, lmer.df = "satterthwaite")
      list(contrasts = as.data.frame(summary(pairs(em, reverse = TRUE))) %>%
             mutate(side = s, cluster_effect_p = anova(m)["cluster", "Pr(>F)"], .before = 1),
           means = as.data.frame(summary(em)) %>% mutate(side = s, .before = 1))
    })
    per_side <- list(contrasts = bind_rows(lapply(ps, `[[`, "contrasts")), means = bind_rows(lapply(ps, `[[`, "means")))
    mb <- lmerTest::lmer(effect ~ cluster * side + value + (1 | id), data = x)
    emb <- emmeans(mb, ~ cluster, lmer.df = "satterthwaite")   # averaged over the two sides
    both <- list(contrasts = as.data.frame(summary(pairs(emb, reverse = TRUE))) %>%
                   mutate(side = "both, shift removed", cluster_effect_p = anova(mb)["cluster", "Pr(>F)"], .before = 1),
                 means = as.data.frame(summary(emb)) %>% mutate(side = "both, shift removed", .before = 1))
    rn <- function(z) z %>% rename_with(~ "ci_low", any_of(c("lower.CL", "asymp.LCL"))) %>% rename_with(~ "ci_high", any_of(c("upper.CL", "asymp.UCL")))
    list(cells = cells %>% mutate(direction = direction, .before = 1),
         tests = bind_rows(per_side$contrasts, both$contrasts) %>% mutate(direction = direction, .before = 1),
         means = rn(bind_rows(per_side$means, both$means)) %>% mutate(direction = direction, .before = 1))
  }
  r <- lapply(c("forward", "reverse"), one)
  list(cells = bind_rows(lapply(r, `[[`, "cells")), tests = bind_rows(lapply(r, `[[`, "tests")),
       means = bind_rows(lapply(r, `[[`, "means")))
}

# The extremes with and without the correction, by one method so that the only
# difference is the matching. Within each extreme value (true value for forward,
# rating for reverse) the clusters' mean effect is taken, then averaged over the
# values every cluster has, weighted by how often each value occurs overall.
# "matched" = trials within the range of true means every cluster covers,
# weighted so every cluster has the same distribution of true means (as in the
# descriptives). CIs and p from a participant bootstrap.
GBD_BOOT <- 1000
gbd_extremes_std <- function(d, seed = 1) {
  wm <- gbd_match_weights(d, "cluster")
  est <- function(dd, w) {
    out <- list()
    for (direction in c("forward", "reverse")) {
      v <- if (direction == "forward") dd$meanGroup else dd$rating
      side <- ifelse(v <= GB_TAIL_LOW, "low", ifelse(v >= GB_TAIL_HIGH, "high", NA))
      sgn <- ifelse(side == "low", 1, -1)
      eff <- if (direction == "forward") sgn * (dd$rating - dd$meanGroup) else sgn * (dd$meanGroup - dd$rating)
      val <- round(v)
      res <- c()
      for (s in c("low", "high")) {
        k <- which(side == s & w > 0)
        cl <- dd$cluster[k]; vv <- val[k]; ee <- eff[k]; ww <- w[k]
        sw  <- tapply(ww, list(vv, cl), sum); swe <- tapply(ww * ee, list(vv, cl), sum)
        ok  <- rowSums(is.na(sw)) == 0
        if (!any(ok)) { res[paste(s, levels(dd$cluster))] <- NA; next }
        m <- swe[ok, , drop = FALSE] / sw[ok, , drop = FALSE]
        pw <- rowSums(sw[ok, , drop = FALSE]); pw <- pw / sum(pw)
        res[paste(s, colnames(m))] <- colSums(m * pw)
      }
      for (c_ in levels(dd$cluster)) res[paste("both", c_)] <- mean(res[paste(c("low", "high"), c_)])
      out[[direction]] <- res
    }
    unlist(out)
  }
  ids <- unique(d$id); rows <- split(seq_len(nrow(d)), d$id)
  run <- function(w_all) {
    point <- est(d, w_all)
    set.seed(seed)
    boot <- t(replicate(GBD_BOOT, { s <- unlist(rows[sample(length(ids), replace = TRUE)], use.names = FALSE); est(d[s, ], w_all[s]) }))
    list(point = point, boot = boot)
  }
  labs_ <- levels(d$cluster)
  bind_rows(lapply(c("as shown", "matched"), function(wt) {
    r <- run(if (wt == "matched") wm else rep(1, nrow(d)))
    bind_rows(lapply(c("forward", "reverse"), function(dir_) bind_rows(lapply(c("low", "high", "both"), function(s) {
      key <- function(c_) paste0(dir_, ".", s, " ", c_)
      bind_rows(
        tibble(weighting = wt, direction = dir_, side = s, term = labs_,
               estimate = r$point[key(labs_)],
               ci_low = apply(r$boot[, key(labs_), drop = FALSE], 2, quantile, .025, na.rm = TRUE),
               ci_high = apply(r$boot[, key(labs_), drop = FALSE], 2, quantile, .975, na.rm = TRUE), p = NA_real_),
        bind_rows(lapply(list(c(2, 1), c(3, 1), c(3, 2)), function(ij) {
          db <- r$boot[, key(labs_[ij[1]])] - r$boot[, key(labs_[ij[2]])]
          tibble(weighting = wt, direction = dir_, side = s, term = paste(labs_[ij[1]], "-", labs_[ij[2]]),
                 estimate = r$point[key(labs_[ij[1]])] - r$point[key(labs_[ij[2]])],
                 ci_low = quantile(db, .025, na.rm = TRUE), ci_high = quantile(db, .975, na.rm = TRUE),
                 p = min(1, 2 * min(mean(db <= 0, na.rm = TRUE), mean(db >= 0, na.rm = TRUE))))
        })))
    }))))
  }))
}

gbd_extremes_figure <- function(ext) {
  labs_ <- levels(ext$means$cluster)
  x <- ext$means %>% mutate(side = factor(side, levels = c("low", "high", "both, shift removed")),
                            direction = factor(direction, levels = c("forward", "reverse"),
                                               labels = c("Forward: same extreme true value
(pull toward the middle)",
                                                          "Reverse: same extreme rating
(exaggeration: true value less extreme)")))
  ggplot(x, aes(side, emmean, colour = cluster)) +
    geom_hline(yintercept = 0, colour = "grey55") +
    geom_pointrange(aes(ymin = ci_low, ymax = ci_high), position = position_dodge(width = .5), size = .35) +
    facet_wrap(~ direction) +
    scale_colour_manual(values = setNames(c("#2a78d6", "#eb6834", "#1baf7a"), labs_), name = NULL) +
    labs(x = NULL, y = "Scale units (model mean, 95% CI)",
         title = "Goldenberg et al. (2021), Exp. 1: the extremes, all trials (no matching)",
         subtitle = paste0("EXPLORATORY. Low side <= ", GB_TAIL_LOW, ", high side >= ", GB_TAIL_HIGH,
                           ". Clusters compared at the same value; 'both' averages the two sides, so a constant rating shift cancels")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), legend.position = "top",
          plot.title = element_text(face = "bold", size = 13), plot.subtitle = element_text(colour = "grey40", size = 9.5))
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
  ext  <- gbd_extremes(d)
  exs  <- gbd_extremes_std(d)

  write.csv(desc,        file.path(dir, "directions_descriptives.csv"),       row.names = FALSE)
  write.csv(pt,          file.path(dir, "directions_participant_tests.csv"),  row.names = FALSE)
  write.csv(mods$slopes, file.path(dir, "directions_model_slopes.csv"),       row.names = FALSE)
  write.csv(mods$tests,  file.path(dir, "directions_model_tests.csv"),        row.names = FALSE)
  write.csv(ext$cells,   file.path(dir, "directions_extremes.csv"),           row.names = FALSE)
  write.csv(ext$tests,   file.path(dir, "directions_extremes_tests.csv"),     row.names = FALSE)
  write.csv(ext$means,   file.path(dir, "directions_extremes_means.csv"),     row.names = FALSE)
  write.csv(exs,         file.path(dir, "directions_extremes_matched.csv"),   row.names = FALSE)

  epoc_save(gbd_figure(d, mods$slopes, "forward"), file.path(dir, "directions_forward.png"), width = 12, height = 6, dpi = 170)
  epoc_save(gbd_figure(d, mods$slopes, "reverse"), file.path(dir, "directions_reverse.png"), width = 12, height = 6, dpi = 170)
  epoc_save(gbd_r_ratio_figure(desc), file.path(dir, "directions_r_sd_ratio.png"), width = 10, height = 10, dpi = 170)
  epoc_save(gbd_extremes_figure(ext), file.path(dir, "directions_extremes.png"), width = 11, height = 5.5, dpi = 170)

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
  w("## The extremes, all trials (no matching)")
  w("")
  w("Matching drops most extreme trials, so the extremes are analysed on all trials. Forward: at the same extreme true value ",
    "(<= ", GB_TAIL_LOW, " or >= ", GB_TAIL_HIGH, "), pull toward the middle = rating - true on the low side, true - rating on the high side. ",
    "Reverse: at the same extreme rating, exaggeration = true - rating on the low side, rating - true on the high side (positive = the rating ",
    "was more extreme than the group). Clusters are compared at the same rounded value with a random intercept per participant; ",
    "'both, shift removed' averages the two sides, so a constant shift of the ratings cancels.")
  w("")
  for (dir_ in c("forward", "reverse")) {
    w("### ", if (dir_ == "forward") "Forward: same extreme true value" else "Reverse: same extreme rating")
    w("")
    w("| side | cluster | trials | participants | mean true | mean rating | ", if (dir_ == "forward") "pull toward middle" else "exaggeration", " |")
    w("|---|---|---|---|---|---|---|")
    cc <- ext$cells %>% filter(direction == dir_)
    for (i in seq_len(nrow(cc))) with(cc[i, ], w("| ", side, " | ", as.character(cluster), " | ", n_trials, " | ", n_participants, " | ",
                                                 f(mean_true, 1), " | ", f(mean_rating, 1), " | ", sprintf("%+.2f", effect), " |"))
    w("")
    for (s in c("low", "high", "both, shift removed")) {
      tt <- ext$tests %>% filter(direction == dir_, side == s)
      w("- ", s, ": effect of cluster ", fmt_p(tt$cluster_effect_p[1]), ".")
      for (i in seq_len(nrow(tt))) w("  - ", tt$contrast[i], ": ", sprintf("%+.2f", tt$estimate[i]), ", ", fmt_p(tt$p.value[i]), ".")
    }
    w("")
  }
  w("## The extremes with and without the correction")
  w("")
  w("Same quantities as above, by one method so that the matching is the only difference: within each extreme value the clusters' ",
    "mean is taken, then averaged over the values all clusters share. 'Matched' keeps trials in the range of true means every ",
    "cluster covers, weighted to the same distribution of true means. 95% CIs and p from a participant bootstrap (", GBD_BOOT, " resamples).")
  w("")
  for (dir_ in c("forward", "reverse")) {
    w("### ", if (dir_ == "forward") "Forward: pull toward the middle at the same extreme true value" else "Reverse: exaggeration at the same extreme rating")
    w("")
    w("| side | comparison | as shown | matched |")
    w("|---|---|---|---|")
    for (s in c("low", "high", "both")) {
      xs <- exs %>% filter(direction == dir_, side == s)
      for (tm in unique(xs$term)) {
        a <- xs %>% filter(term == tm, weighting == "as shown"); b <- xs %>% filter(term == tm, weighting == "matched")
        cell <- function(z) if (is.na(z$estimate)) "n/a" else paste0(sprintf("%+.2f", z$estimate), " [", sprintf("%.2f", z$ci_low), ", ",
                                                                   sprintf("%.2f", z$ci_high), "]", ifelse(is.na(z$p), "", paste0(", ", fmt_p(z$p))))
        w("| ", ifelse(s == "both", "both, shift removed", s), " | ", tm, " | ", cell(a), " | ", cell(b), " |")
      }
    }
    w("")
  }
  invisible(list(descriptives = desc, participant_tests = pt, models = mods, extremes = ext, extremes_std = exs))
}
