# =========================================================
# 19_exploratory_narrowing.R -- EXPLORATORY, not part of the write-up
#
# Why do reports spread less (SD of reports / SD of true values falls) at
# higher noise? Noise alone would widen them. Two checks between the lowest
# and the highest noise level, each within participant:
#
#   ends     mean(report - true) in the bottom and top band of true means
#            (the lowest / highest EXPL_NARROW_BAND + 1 values of the range).
#            Narrowing = the bottom band moves up and the top band moves down.
#            A compressed stimulus scale at one end (Experiment 4's last ten
#            steps are about half the size of the others) predicts a pull at
#            that end only; a pull toward the middle of the means or of the
#            response bar predicts it at both ends.
#   session  change in log SD ratio (low -> high noise), in the first vs the
#            second half of each colour block and in the first vs the second
#            colour block. A pull learned from the range of means should grow
#            over the session; hedging toward the middle of the bar should not.
#            Levels are compared within each half, because Experiment 4's
#            shuffle placed SD 3 trials earlier and SD 7 trials later.
#
# Output: output/<exp>/exploratory/narrowing_*.csv and narrowing.md, and the
# tests for output/exploratory/all_narrowing_tests.csv.
# =========================================================

EXPL_NARROW_BAND <- 4   # band width in steps above the lowest / below the highest true mean
EXPL_NARROW_MIN  <- 3   # minimum trials in a participant x colour x level x band cell

expl_paired <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(tibble(n = length(x), mean = NA_real_, se = NA_real_, t = NA_real_, p = NA_real_))
  tt <- t.test(x)
  tibble(n = length(x), mean = mean(x), se = sd(x) / sqrt(length(x)), t = unname(tt$statistic), p = tt$p.value)
}

expl_narrow_prep <- function(dat, exp) {
  d <- dat %>%
    mutate(level = as.numeric(as.character(.data[[exp$noise_var]])),
           colour = if ("color" %in% names(dat)) as.character(color) else "all")
  lv <- sort(unique(d$level))
  d %>% filter(level %in% range(lv)) %>%
    mutate(noise = ifelse(level == min(lv), "low", "high"), err = indexSelected - meanVal) %>%
    group_by(participant_id, colour) %>%
    mutate(pos = rank(trial, ties.method = "first"),
           half = ifelse(pos <= n() / 2, "early", "late"),
           block_start = min(trial)) %>%
    group_by(participant_id) %>%
    mutate(block = ifelse(block_start == min(block_start), "first block", "second block")) %>%
    ungroup()
}

expl_narrow_ends <- function(d) {
  lo <- min(d$meanVal); hi <- max(d$meanVal)
  d <- d %>% mutate(band = case_when(meanVal <= lo + EXPL_NARROW_BAND ~ "bottom",
                                     meanVal >= hi - EXPL_NARROW_BAND ~ "top",
                                     TRUE ~ NA_character_)) %>% filter(!is.na(band))
  one <- function(dd, colour_lab) {
    ch <- dd %>% group_by(participant_id, band, noise) %>%
      summarise(err = if (n() >= EXPL_NARROW_MIN) mean(err) else NA_real_, .groups = "drop") %>%
      tidyr::pivot_wider(names_from = noise, values_from = err) %>%
      mutate(change = high - low)
    # (bottom change - top change) / 2: the pull toward the middle, free of a
    # shift that moves both ends the same way (2-square arrays in Experiments 1-3)
    pull <- ch %>% select(participant_id, band, change) %>%
      tidyr::pivot_wider(names_from = band, values_from = change) %>%
      transmute(band = "both ends, shift removed", change = (bottom - top) / 2)
    bind_rows(ch %>% select(band, change), pull) %>%
      group_by(band) %>% group_modify(~ expl_paired(.x$change)) %>% ungroup() %>%
      mutate(colour = colour_lab,
             band_range = case_when(band == "bottom" ~ paste0(lo, "-", lo + EXPL_NARROW_BAND),
                                    band == "top" ~ paste0(hi - EXPL_NARROW_BAND, "-", hi), TRUE ~ ""),
             .before = 1)
  }
  bind_rows(lapply(sort(unique(d$colour)), function(cl) one(d %>% filter(colour == cl), cl)),
            if (n_distinct(d$colour) > 1) one(d, "both") else NULL)
}

expl_narrow_bins <- function(d, nbins = 5) {
  br <- unique(round(quantile(d$meanVal, seq(0, 1, length.out = nbins + 1))))
  d %>% mutate(bin = cut(meanVal, br, include.lowest = TRUE, dig.lab = 3)) %>%
    group_by(bin, noise) %>% summarise(err = mean(err), n = n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = noise, values_from = c(err, n)) %>%
    mutate(change = err_high - err_low)
}

expl_narrow_session <- function(d) {
  per <- function(dd) dd %>% group_by(participant_id, noise) %>%
    summarise(lr = if (n() >= 8 && sd(meanVal) > 0) log(sd(indexSelected) / sd(meanVal)) else NA_real_, .groups = "drop") %>%
    tidyr::pivot_wider(names_from = noise, values_from = lr) %>% transmute(participant_id, change = high - low)
  splits <- list(c("half", "early", "late"))
  if (n_distinct(d$block) > 1) splits <- c(splits, list(c("block", "first block", "second block")))
  bind_rows(lapply(splits, function(s) {
    a <- per(d %>% filter(.data[[s[1]]] == s[2])); b <- per(d %>% filter(.data[[s[1]]] == s[3]))
    j <- inner_join(a, b, by = "participant_id", suffix = c("_a", "_b"))
    expl_paired(j$change_b - j$change_a) %>%
      mutate(split = s[1], earlier = s[2], later = s[3],
             change_earlier = mean(j$change_a, na.rm = TRUE), change_later = mean(j$change_b, na.rm = TRUE), .before = 1)
  }))
}

epoc_narrowing <- function(dat, exp, dir) {
  out <- file.path(dir, "exploratory")
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  d <- expl_narrow_prep(dat, exp)
  lv <- range(as.numeric(exp$levels))
  ends <- expl_narrow_ends(d); bins <- expl_narrow_bins(d); sess <- expl_narrow_session(d)
  write.csv(ends, file.path(out, "narrowing_ends.csv"), row.names = FALSE)
  write.csv(bins, file.path(out, "narrowing_bins.csv"), row.names = FALSE)
  write.csv(sess, file.path(out, "narrowing_session.csv"), row.names = FALSE)

  con <- file(file.path(out, "narrowing.md"), open = "wt"); on.exit(close(con))
  w <- function(...) log_line(con, ...)
  f <- function(x, k = 2) formatC(x, format = "f", digits = k)
  fp <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "< .001", paste("=", formatC(p, format = "f", digits = 3))))
  w("# ", exp$label, " - why reports narrow at higher noise (EXPLORATORY)")
  w("")
  w("_Not part of the main write-up. Generated by analysis/run_all.R on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "._")
  w("")
  w("Lowest vs highest noise level (", lv[1], " vs ", lv[2], "). Noise alone would make reports spread more; ",
    "these checks look at where the pull toward the middle comes from.")
  w("")
  w("## Pull at each end")
  w("")
  w("Mean (report - true) in the bottom and top band of true means, change from low to high noise, per participant. ",
    "Narrowing = bottom band up (+), top band down (-).")
  w("")
  w("| colour | band (true mean) | change (steps) | SE | n | p |")
  w("|---|---|---|---|---|---|")
  for (i in seq_len(nrow(ends))) with(ends[i, ], w("| ", colour, " | ", band, " ", band_range, " | ", sprintf("%+.2f", mean), " | ", f(se), " | ", n, " | ", fp(p), " |"))
  w("")
  w("Across the whole range (pooled trials, mean report - true):")
  w("")
  w("| true mean | low noise | high noise | change |")
  w("|---|---|---|---|")
  for (i in seq_len(nrow(bins))) with(bins[i, ], w("| ", as.character(bin), " | ", sprintf("%+.2f", err_low), " | ", sprintf("%+.2f", err_high), " | ", sprintf("%+.2f", change), " |"))
  w("")
  w("## Over the session")
  w("")
  w("Change in log SD ratio (SD of reports / SD of true values) from low to high noise, per participant; ",
    "negative = narrower reports at high noise.")
  w("")
  w("| split | earlier | later | difference | SE | n | p |")
  w("|---|---|---|---|---|---|---|")
  for (i in seq_len(nrow(sess))) with(sess[i, ], w("| ", earlier, " vs ", later, " | ", f(change_earlier, 3), " | ", f(change_later, 3), " | ",
                                                   sprintf("%+.3f", mean), " | ", f(se, 3), " | ", n, " | ", fp(p), " |"))
  invisible(list(tests = bind_rows(
    ends %>% transmute(experiment = exp$id, check = "ends", what = paste(colour, band), estimate = mean, se, n, p),
    sess %>% transmute(experiment = exp$id, check = "session", what = paste(earlier, "vs", later), estimate = mean, se, n, p))))
}
