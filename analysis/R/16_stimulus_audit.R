# =========================================================
# 16_stimulus_audit.R -- what the arrays actually looked like
#
# Every other module treats the design labels at face value: array size 2/10/20
# is "the noise manipulation" in Experiments 1-3, variance SD 3/5/7 in
# Experiment 4. This module goes back to the raw session logs, reads the
# `array_values` field that the cleaned files drop, and measures the dispersion
# of the squares that were actually displayed on each trial.
#
# It matters because the two are not the same thing. The generator used a fixed
# deviation template per array size, so in Experiments 2 and 3 the arrays of 10
# squares are *more* dispersed than the arrays of 20, and in Experiment 1 sizes
# 8 and 12 are identical. Array size and stimulus variance are therefore
# perfectly confounded in those experiments, and confounded non-monotonically.
#
# What the module produces:
#   the measured item SD per experiment x condition x level, with its
#   trial-to-trial variability;
#   those numbers joined to the model slopes, so the slopes can be plotted
#   against what was on screen rather than against the design label;
#   a meta-regression of cell slopes on item SD with experiment as a factor
#   (the experiments differ in exposure time and trial count, so they are
#   allowed their own intercepts);
#   supplementary.md, a written-up version for the paper.
#
# Parsing the raw logs takes a minute or two, so trial-level results are cached
# in analysis/output/stimulus_audit/. Delete the cache or pass force = TRUE to
# re-read.
# =========================================================

AUDIT_DIR <- file.path(OUT, "stimulus_audit")

# Population SD of the values displayed on one trial
.trial_sd <- function(x) {
  v <- suppressWarnings(as.numeric(strsplit(gsub("\\[|\\]|\"", "", x), ",")[[1]]))
  v <- v[is.finite(v)]
  if (length(v) < 2) return(c(NA_real_, NA_real_))
  c(sqrt(mean((v - mean(v))^2)), length(v))
}

# Read only the four columns we need out of one raw session file
.read_raw_arrays <- function(path) {
  want <- c("participant_id", "trial", "array_length", "array_values")
  hdr <- tryCatch(names(read.csv(path, nrows = 1)), error = function(e) NULL)
  if (is.null(hdr) || !all(want %in% hdr)) return(NULL)
  d <- tryCatch(
    suppressWarnings(read.csv(path, colClasses = ifelse(hdr %in% want, NA, "NULL"),
                              stringsAsFactors = FALSE)),
    error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(NULL)
  d[nzchar(trimws(d$array_values)) & !is.na(d$array_length), , drop = FALSE]
}

# Trial-level item SD for one experiment, all conditions
epoc_item_sd_trials <- function(exp, force = FALSE) {

  dir.create(AUDIT_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(ARRAY_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
  cache <- file.path(ARRAY_CACHE_DIR, paste0("item_sd_trials_", exp$id, ".csv"))
  if (file.exists(cache) && !force) return(read.csv(cache, stringsAsFactors = FALSE))

  out <- list()
  for (cond in names(exp$dirs)) {
    folder <- file.path(DATA, exp$dirs[[cond]])
    files <- list.files(folder, pattern = "PARTICIPANT.*\\.csv$", full.names = TRUE)
    rows <- lapply(files, function(f) {
      d <- .read_raw_arrays(f)
      if (is.null(d)) return(NULL)
      sds <- vapply(d$array_values, .trial_sd, numeric(2))
      data.frame(participant_id = d$participant_id,
                 trial          = d$trial,
                 array_length   = suppressWarnings(as.numeric(d$array_length)),
                 item_sd        = sds[1, ],
                 n_items        = sds[2, ],
                 stringsAsFactors = FALSE)
    })
    rows <- bind_rows(rows)
    if (nrow(rows)) out[[cond]] <- rows %>% mutate(condition = cond)
  }

  res <- bind_rows(out) %>%
    filter(is.finite(item_sd), is.finite(n_items)) %>%
    # main-task trials only: the practice block uses its own array sizes, and in
    # Experiment 4 every main-task array holds 10 squares regardless of the
    # variance level stored in array_length
    filter(if (is.null(exp$recode)) n_items == array_length else n_items == 10) %>%
    mutate(
      level = if (is.null(exp$recode)) array_length
              else unname(exp$recode[as.character(array_length)]),
      experiment = exp$id
    ) %>%
    filter(level %in% exp$levels)

  write.csv(res, cache, row.names = FALSE)
  res
}

# Level summaries for one experiment
epoc_item_sd_levels <- function(exp, force = FALSE) {
  epoc_item_sd_trials(exp, force) %>%
    group_by(experiment, condition, level) %>%
    # the spread first: summarise() evaluates in order, so overwriting item_sd
    # before taking its SD would leave nothing to take the SD of
    summarise(n_trials      = n(),
              item_sd_sd    = sd(item_sd),
              item_sd       = mean(item_sd),
              n_items       = mean(n_items),
              .groups = "drop") %>%
    arrange(condition, level)
}

# --- the audit across experiments ---------------------------------------
epoc_stimulus_audit <- function(ids = REPORTED, force = FALSE) {

  dir.create(AUDIT_DIR, showWarnings = FALSE, recursive = TRUE)

  levels_tab <- bind_rows(lapply(ids, function(id) {
    exp <- EXPERIMENTS[[id]]
    epoc_item_sd_levels(exp, force) %>%
      mutate(label = exp$label, noise_var = exp$noise_var)
  }))

  # join to the model slopes produced by the main pipeline
  slope_path <- file.path(OUT, "all_slopes.csv")
  slopes <- if (file.exists(slope_path)) {
    read.csv(slope_path, stringsAsFactors = FALSE) %>%
      mutate(level = suppressWarnings(as.numeric(noise_level))) %>%
      select(experiment, condition, level, slope, slope_se = SE, ci_low, ci_high)
  } else NULL

  cells <- if (is.null(slopes)) levels_tab else
    left_join(levels_tab, slopes, by = c("experiment", "condition", "level"))

  # Is the slope monotonic in item SD within each experiment x condition?
  ordering <- cells %>%
    filter(is.finite(slope)) %>%
    group_by(experiment, condition) %>%
    arrange(item_sd, .by_group = TRUE) %>%
    summarise(
      levels_by_sd     = paste(level, collapse = " < "),
      slopes_by_sd     = paste(sprintf("%.3f", slope), collapse = " > "),
      monotonic        = all(diff(slope) < 0),
      worst_inversion  = max(c(0, diff(slope))),
      spearman_sd_slope = suppressWarnings(cor(item_sd, slope, method = "spearman")),
      .groups = "drop")

  # Meta-regression: cell slopes on item SD, each experiment its own intercept
  # datasets flagged as a subset of another experiment would double-count
  redundant <- names(EXPERIMENTS)[vapply(EXPERIMENTS,
                    function(e) !is.null(e$subset_of), logical(1))]
  meta <- NULL
  fit <- NULL
  d <- cells %>%
    filter(is.finite(slope), is.finite(item_sd), !experiment %in% redundant)
  if (n_distinct(d$experiment) > 1 && nrow(d) > 5) {
    fit <- lm(slope ~ item_sd + experiment, data = d, weights = 1 / (d$slope_se^2))
    meta <- as.data.frame(coef(summary(fit))) %>%
      tibble::rownames_to_column("term") %>%
      rename(b = Estimate, SE = `Std. Error`, t = `t value`, p = `Pr(>|t|)`)
  }

  res <- list(levels = levels_tab, cells = cells, ordering = ordering,
              meta = meta, meta_fit = fit)

  write.csv(levels_tab, file.path(AUDIT_DIR, "item_sd_by_level.csv"),    row.names = FALSE)
  write.csv(cells,      file.path(AUDIT_DIR, "item_sd_vs_slopes.csv"),   row.names = FALSE)
  write.csv(ordering,   file.path(AUDIT_DIR, "slope_ordering_by_sd.csv"),row.names = FALSE)
  if (!is.null(meta)) write.csv(meta, file.path(AUDIT_DIR, "meta_regression.csv"), row.names = FALSE)

  epoc_save(fig_audit_sd_by_level(res),  file.path(AUDIT_DIR, "fig1_item_sd_by_level.png"),
            width = 8, height = 4.5)
  epoc_save(fig_audit_slope_vs_sd(res),  file.path(AUDIT_DIR, "fig2_slope_vs_item_sd.png"),
            width = 8.5, height = 5)
  epoc_audit_report(res, file.path(AUDIT_DIR, "supplementary.md"))
  res
}

# --- figures -------------------------------------------------------------
fig_audit_sd_by_level <- function(res) {
  d <- res$levels %>% mutate(level = factor(level, levels = sort(unique(level))))
  ggplot(d, aes(x = level, y = item_sd, colour = condition, group = condition)) +
    geom_line(linewidth = .9) +
    geom_point(size = 2.4) +
    facet_wrap(~ label, scales = "free_x") +
    scale_colour_manual(values = COND_COLOURS, drop = TRUE) +
    labs(x = "Design level (array size, or nominal SD in Experiment 4)",
         y = "Measured SD of the displayed squares", colour = "Condition",
         title = "What the arrays actually looked like",
         subtitle = "Dispersion of the squares on screen, from the raw session logs") +
    theme_epoc()
}

fig_audit_slope_vs_sd <- function(res) {
  d <- res$cells %>% filter(is.finite(slope))
  ggplot(d, aes(x = item_sd, y = slope, colour = experiment)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0, alpha = .5) +
    geom_line(aes(group = interaction(experiment, condition)), alpha = .6) +
    geom_point(size = 2.4) +
    geom_text(aes(label = level), size = 2.6, vjust = -1.1, show.legend = FALSE) +
    facet_wrap(~ condition) +
    labs(x = "Measured SD of the displayed squares",
         y = "Slope of the objective mean", colour = NULL,
         title = "Regression to the mean tracks stimulus dispersion, not the design label",
         subtitle = "Each point is one experiment x condition x level; numbers are the design levels") +
    theme_epoc()
}

# --- write-up ------------------------------------------------------------
epoc_audit_report <- function(res, path) {

  con <- file(path, open = "wt"); on.exit(close(con))
  w <- function(...) log_line(con, ...)

  w("# Supplementary: what the arrays actually looked like")
  w("")
  w("_Generated by analysis/run_all.R on ", format(Sys.time(), "%Y-%m-%d %H:%M"), "._")
  w("")
  w("The cleaned data files record the array size and the objective mean of each ",
    "trial, but not the individual square values. Reading `array_values` back out of ",
    "the raw session logs shows that the dispersion of the displayed squares was not ",
    "a by-product of array size: the generator used a fixed deviation template per ",
    "level, so array size and stimulus variance are perfectly confounded in ",
    "Experiments 1-3, and confounded non-monotonically.")
  w("")

  w("## Measured dispersion per level")
  w("")
  w("| Experiment | Condition | Level | Items | Measured item SD | Trial-to-trial SD | Trials |")
  w("|---|---|---|---|---|---|---|")
  lv <- res$levels
  for (i in seq_len(nrow(lv)))
    w("| ", lv$label[i], " | ", lv$condition[i], " | ", lv$level[i], " | ",
      fmt_num(lv$n_items[i], 0), " | ", fmt_num(lv$item_sd[i], 2), " | ",
      fmt_num(lv$item_sd_sd[i], 3), " | ", lv$n_trials[i], " |")
  w("")
  w("Two things to note. In Experiments 2 and 3 the 10-square arrays are *more* ",
    "dispersed than the 20-square arrays, and in Experiment 1 sizes 8 and 12 have ",
    "the same dispersion. Where the trial-to-trial SD is 0, every trial at that ",
    "level used an identical set of deviations.")
  w("")

  w("## Slopes ordered by dispersion")
  w("")
  w("Ordering each experiment's levels by measured item SD rather than by array size:")
  w("")
  w("| Experiment | Condition | Levels, ascending item SD | Slopes | Monotonic? | Spearman |")
  w("|---|---|---|---|---|---|")
  od <- res$ordering
  for (i in seq_len(nrow(od)))
    w("| ", od$experiment[i], " | ", od$condition[i], " | ", od$levels_by_sd[i], " | ",
      od$slopes_by_sd[i], " | ", if (od$monotonic[i]) "yes" else
        paste0("no (largest inversion ", fmt_num(od$worst_inversion[i], 3), ")"),
      " | ", fmt_num(od$spearman_sd_slope[i], 2), " |")
  w("")
  w("The comparison that carries the most weight is array size 10 against array ",
    "size 20 in Experiments 2 and 3: the set size doubles while dispersion falls, ",
    "and the slope rises. A processing-load account predicts the opposite; a ",
    "dispersion account predicts exactly this.")
  w("")

  if (!is.null(res$meta)) {
    w("## Slope as a function of dispersion")
    w("")
    w("Cell slopes regressed on measured item SD, weighted by the inverse of their ",
      "squared standard errors, with an intercept per experiment (the experiments ",
      "differ in exposure time, trial count and set size, so they are not expected ",
      "to share one). Datasets that are a subset of another experiment are excluded ",
      "here so they cannot double-count; they still appear in the tables above.")
    w("")
    m <- res$meta %>% filter(term == "item_sd")
    if (nrow(m))
      w("- item SD: ", fmt_b(m$b[1], m$SE[1], m$t[1], NULL, m$p[1]),
        " - each additional unit of dispersion costs about ",
        fmt_num(abs(m$b[1]), 3), " of slope.")
    w("")
    w("Full coefficients in `meta_regression.csv`.")
    w("")
  }

  w("## Reading")
  w("")
  w("Experiments 1-3 did not fail to manipulate noise. They manipulated dispersion ",
    "over a narrow range and in a scrambled order, which is why the slope pattern ",
    "looked non-monotonic in array size. Experiment 4 manipulated the same variable ",
    "deliberately, over roughly twice the range and in the right order, and produced ",
    "a graded effect. The set is one finding, not three failures and one success.")
  w("")
  w("Files: `item_sd_by_level.csv`, `item_sd_vs_slopes.csv`, `slope_ordering_by_sd.csv`, ",
    "`meta_regression.csv`, `fig1_item_sd_by_level.png`, `fig2_slope_vs_item_sd.png`, ",
    "and the cached trial-level values in `item_sd_trials_<experiment>.csv`.")
  invisible(path)
}
