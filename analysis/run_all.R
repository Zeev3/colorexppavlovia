# =========================================================
# run_all.R -- run the whole analysis set
#
#   Rscript analysis/run_all.R              # the four reported experiments
#   Rscript analysis/run_all.R exp4         # one experiment
#   Rscript analysis/run_all.R all          # including pilots / extra batches
#   Rscript analysis/run_all.R --shown-mean # alternative: true value = mean of the
#                                           # squares shown; output_shown_mean/
#   Rscript analysis/run_all.R --full-random # alternative: maximal random effects,
#                                           # by-participant slope always kept
#   Rscript analysis/run_all.R --cleaned    # use the data_cleaned.csv on disk
#                                           # instead of re-cleaning from raw
#
# Everything lands in analysis/output/<experiment>/ :
#   results.md                    readable results section, all numbers filled in
#   descriptives.csv              N, trials, mean absolute error per condition
#   exclusions.csv                participant flow through the cleaning stages
#   participants.csv              per-participant retention and tracking rho
#   correlations_by_participant.csv / _summary.csv / _anova.csv / _posthoc.csv
#   lmm_anova.csv / _coefficients.csv / _slopes.csv / _contrasts.csv / _model.txt
#   exaggeration_by_participant.csv / _summary.csv / _anova.csv / _posthoc.csv
#   compression_*.csv             centre-directed pull on extreme trials
#   compression_band_*.csv        extreme vs middle trials
#   fig1..fig4 .png
#
# plus cross-experiment summaries in analysis/output/ .
# =========================================================

args <- commandArgs(trailingOnly = TRUE)
use_cleaned <- "--cleaned" %in% args
# --shown-mean: alternative pipeline, objective value = mean of the displayed
# squares; everything goes to analysis/output_shown_mean/ (see 00_setup.R)
if ("--shown-mean" %in% args) {
  if (use_cleaned) stop("--shown-mean re-cleans from the raw logs and cannot be combined with --cleaned")
  Sys.setenv(EPOC_TRUE_VALUE = "shown")
}
# --full-random: alternative random-effects strategy (see 00_setup.R / 05_lmm.R);
# output goes to analysis/output_full_random/ (or _shown_mean_full_random)
if ("--full-random" %in% args) Sys.setenv(EPOC_RANDOM = "full")
args <- setdiff(args, c("--cleaned", "--shown-mean", "--full-random"))

# locate the project root from this script's own path, so the script can be
# run from anywhere
this_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (length(this_file)) Sys.setenv(EPOC_ROOT = normalizePath(file.path(dirname(this_file), "..")))

R_DIR <- file.path(Sys.getenv("EPOC_ROOT", unset = getwd()), "analysis", "R")
for (f in sort(list.files(R_DIR, pattern = "\\.R$", full.names = TRUE))) source(f)

targets <- (if (!length(args)) REPORTED
            else if (identical(args, "all")) names(EXPERIMENTS)
            else args)
unknown <- setdiff(targets, names(EXPERIMENTS))
if (length(unknown)) stop("unknown experiment(s): ", paste(unknown, collapse = ", "))

all_desc <- list(); all_slopes <- list(); all_corr <- list(); all_exagg <- list()
all_cond_slopes <- list(); all_rev_slopes <- list(); all_tails <- list()
all_compression <- list()
all_anovas <- list()
all_expl_r2t <- list(); all_expl_split <- list(); all_expl_pw <- list(); all_expl_bands <- list(); all_expl_t2r <- list()   # exploratory only
all_expl_narrow <- list()

for (id in targets) {

  exp <- EXPERIMENTS[[id]]
  dir <- exp_out_dir(exp)
  message("\n=== ", exp$label, " ===")

  prep <- epoc_prepare(exp, source = if (use_cleaned) "cleaned" else "recompute")
  dat  <- prep$data
  desc <- epoc_descriptives(dat, exp)
  print(desc)

  message("  single-subject correlations")
  corr <- epoc_correlations(dat, exp)

  message("  mixed-effects model")
  lmm  <- epoc_lmm(dat, exp)

  message("  within-condition models")
  cmods <- epoc_lmm_by_condition(dat, exp)

  message("  reverse-direction models (objective ~ subjective)")
  rev <- epoc_reverse_models(dat, exp)

  message("  two-stage ANOVAs on participant-level measures")
  anovas <- epoc_anovas(dat, exp)

  message("  tail analyses")
  tails <- epoc_tails(dat, exp)

  message("  exaggeration")
  exg  <- epoc_exaggeration(dat, exp)

  message("  centre-directed compression")
  comp <- epoc_compression(dat, exp)

  message("  figures")
  epoc_figures(corr, lmm, exg, exp, dir)

  message("  Galton squeeze diagrams")
  epoc_galton_figures(dat, exp, dir)
  epoc_condition_model_outputs(cmods, exp, dir)
  epoc_reverse_outputs(rev, lmm, exp, dir)
  epoc_tail_outputs(tails, exp, dir)
  anova_vs_lmm <- epoc_anova_outputs(anovas, lmm, exp, dir)
  epoc_compression_outputs(comp, exp, dir)

  # ---- exploratory, kept out of results.md and the main cross-experiment tables ----
  message("  exploratory: what lies behind each report value")
  all_expl_r2t[[id]] <- epoc_report_to_true(dat, exp, dir)$tests
  all_expl_split[[id]] <- epoc_slope_decomposition(dat, exp, dir)
  epoc_regression_panels(dat, exp, dir)
  epoc_array_configurations(dat, exp, dir)
  all_expl_pw[[id]] <- epoc_piecewise_reverse(dat, exp, dir)$tests
  all_expl_bands[[id]] <- epoc_report_bands(dat, exp, dir)$tests
  all_expl_t2r[[id]] <- epoc_true_to_report(dat, exp, dir)$tests
  all_expl_narrow[[id]] <- epoc_narrowing(dat, exp, dir)$tests

  # ---- tables ----
  write.csv(desc,                    file.path(dir, "descriptives.csv"), row.names = FALSE)
  if (!is.null(prep$exclusions))   write.csv(prep$exclusions,   file.path(dir, "exclusions.csv"),   row.names = FALSE)
  if (!is.null(prep$participants)) write.csv(prep$participants, file.path(dir, "participants.csv"), row.names = FALSE)

  write.csv(corr$by_participant, file.path(dir, "correlations_by_participant.csv"), row.names = FALSE)
  write.csv(corr$summary,        file.path(dir, "correlations_summary.csv"),        row.names = FALSE)
  write.csv(corr$anova,          file.path(dir, "correlations_anova.csv"),          row.names = FALSE)
  write.csv(corr$posthoc,        file.path(dir, "correlations_posthoc.csv"),        row.names = FALSE)

  write.csv(lmm$anova,        file.path(dir, "lmm_anova.csv"),        row.names = FALSE)
  write.csv(lmm$coefficients, file.path(dir, "lmm_coefficients.csv"), row.names = FALSE)
  write.csv(lmm$slopes,       file.path(dir, "lmm_slopes.csv"),       row.names = FALSE)
  write.csv(lmm$contrasts,    file.path(dir, "lmm_contrasts.csv"),    row.names = FALSE)
  write.csv(lmm$predictions,  file.path(dir, "lmm_predictions.csv"),  row.names = FALSE)
  capture.output(
    cat("formula:", lmm$formula, "\nrandom structure used:", lmm$rand_used,
        "\nsingular:", lmm$singular, "\n\n"),
    print(summary(lmm$model)),
    file = file.path(dir, "lmm_model.txt")
  )

  write.csv(exg$by_participant, file.path(dir, "exaggeration_by_participant.csv"), row.names = FALSE)
  write.csv(exg$summary,        file.path(dir, "exaggeration_summary.csv"),        row.names = FALSE)
  write.csv(exg$anova,          file.path(dir, "exaggeration_anova.csv"),          row.names = FALSE)
  write.csv(exg$posthoc,        file.path(dir, "exaggeration_posthoc.csv"),        row.names = FALSE)

  epoc_report(list(descriptives = desc, exclusions = prep$exclusions,
                   corr = corr, lmm = lmm, cond_models = cmods,
                   reverse = rev, tails = tails, exagg = exg,
                   anovas = anovas, anova_vs_lmm = anova_vs_lmm),
              exp, file.path(dir, "results.md"))

  # ---- collect for the cross-experiment tables ----
  lvl <- exp$noise_var
  all_desc[[id]]   <- desc
  all_slopes[[id]] <- lmm$slopes %>%
    mutate(noise_level = as.character(.data[[lvl]]), noise_var = lvl,
           label = exp$label) %>%
    select(experiment, label, noise_var, noise_level, any_of("condition"),
           slope, SE, ci_low, ci_high)
  all_corr[[id]]   <- corr$summary %>%
    mutate(noise_level = as.character(.data[[lvl]]), noise_var = lvl) %>%
    select(experiment, noise_var, noise_level, condition, n, mean_r, ci_low_r, ci_high_r)
  all_cond_slopes[[id]] <- cmods$slopes %>%
    mutate(noise_level = as.character(.data[[lvl]]), noise_var = lvl, label = exp$label) %>%
    select(experiment, label, noise_var, noise_level, condition, slope, SE, ci_low, ci_high)
  all_rev_slopes[[id]] <- rev$slopes %>%
    mutate(noise_level = as.character(.data[[lvl]]), noise_var = lvl, label = exp$label) %>%
    select(experiment, label, noise_var, noise_level, model, any_of("condition"),
           slope, SE, ci_low, ci_high)
  all_tails[[id]] <- tails$fixed$means %>%
    mutate(noise_level = as.character(.data[[lvl]]), noise_var = lvl, label = exp$label) %>%
    select(experiment, label, noise_var, noise_level, tail, mean_obj = emmean, SE, ci_low, ci_high)
  all_anovas[[id]] <- anovas$anova %>% mutate(label_exp = exp$label)
  all_compression[[id]] <- comp$summary %>%
    mutate(noise_level = as.character(.data[[lvl]]), noise_var = lvl, label = exp$label) %>%
    select(experiment, label, noise_var, noise_level, condition, n,
           inward, se, inward_low, inward_high)
  all_exagg[[id]]  <- exg$summary %>%
    mutate(noise_level = as.character(.data[[lvl]]), noise_var = lvl) %>%
    select(experiment, noise_var, noise_level, condition, mean_prop, se)
}

# =========================================================
# Cross-experiment summaries
# =========================================================
slopes_all <- bind_rows(all_slopes)
write.csv(bind_rows(all_desc),  file.path(OUT, "all_descriptives.csv"), row.names = FALSE)
write.csv(slopes_all,           file.path(OUT, "all_slopes.csv"),       row.names = FALSE)
write.csv(bind_rows(all_corr),  file.path(OUT, "all_correlations.csv"), row.names = FALSE)
write.csv(bind_rows(all_exagg), file.path(OUT, "all_exaggeration.csv"), row.names = FALSE)
write.csv(bind_rows(all_cond_slopes), file.path(OUT, "all_slopes_by_condition_model.csv"),
          row.names = FALSE)
write.csv(bind_rows(all_rev_slopes), file.path(OUT, "all_slopes_reverse.csv"),
          row.names = FALSE)
write.csv(bind_rows(all_tails), file.path(OUT, "all_tails_fixed.csv"), row.names = FALSE)
write.csv(bind_rows(all_compression), file.path(OUT, "all_compression.csv"), row.names = FALSE)
write.csv(bind_rows(all_anovas), file.path(OUT, "all_anova_tests.csv"), row.names = FALSE)

# exploratory summaries live in their own folder so they are not mistaken for
# main-analysis output
dir.create(file.path(OUT, "exploratory"), showWarnings = FALSE)
write.csv(bind_rows(all_expl_r2t), file.path(OUT, "exploratory", "all_report_to_true_tests.csv"),
          row.names = FALSE)
write.csv(bind_rows(all_expl_split), file.path(OUT, "exploratory", "all_slope_decomposition.csv"),
          row.names = FALSE)
write.csv(bind_rows(all_expl_pw), file.path(OUT, "exploratory", "all_piecewise_reverse_tests.csv"),
          row.names = FALSE)
write.csv(bind_rows(all_expl_bands), file.path(OUT, "exploratory", "all_report_bands_tests.csv"),
          row.names = FALSE)
write.csv(bind_rows(all_expl_t2r), file.path(OUT, "exploratory", "all_true_to_report_tests.csv"),
          row.names = FALSE)
write.csv(bind_rows(all_expl_narrow), file.path(OUT, "exploratory", "all_narrowing_tests.csv"),
          row.names = FALSE)
# the Goldenberg split is computed further down; the combined table is written there

if (nrow(slopes_all)) {
  slopes_all <- slopes_all %>%
    mutate(noise_level = factor(noise_level, levels = unique(noise_level[order(as.numeric(noise_level))])))
  fig <- ggplot(slopes_all, aes(x = noise_level, y = slope,
                                colour = condition, group = condition)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
    geom_line(linewidth = .9) +
    geom_point(size = 2.4) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = .12) +
    facet_wrap(~ label, scales = "free_x") +
    scale_colour_manual(values = COND_COLOURS, drop = TRUE) +
    labs(x = "Noise level (array size, or SD in Experiment 4)",
         y = "Slope of the objective mean", colour = "Condition",
         title = "Regression to the mean across experiments",
         subtitle = "Slopes below the dashed line = reports compressed towards the participant's mean") +
    theme_epoc()
  epoc_save(fig, file.path(OUT, "all_slopes.png"), width = 10, height = 7)
}

# =========================================================
# Stimulus audit: what the arrays actually looked like
# (reads the raw session logs; trial-level values are cached)
# =========================================================
message("\n=== Stimulus audit ===")
audit <- epoc_stimulus_audit(intersect(targets, names(EXPERIMENTS)))
print(audit$ordering)

# =========================================================
# Cross-experiment comparisons (only those whose two experiments both ran)
# =========================================================
for (cmp in COMPARISONS) {
  if (all(c(cmp$a, cmp$b) %in% targets)) {
    epoc_compare(cmp, source = if (use_cleaned) "cleaned" else "recompute")
  }
}

# =========================================================
# Exploratory analysis of the Goldenberg open data
# =========================================================
if (!length(args) || identical(args, "all") || "exp0" %in% args) {
  message("\n=== Exploratory analysis (Goldenberg et al., 2021) ===")
  epoc_goldenberg()
  message("  exploratory: what lies behind each rating")
  epoc_goldenberg_report_to_true()
  gb_split <- epoc_goldenberg_slope_decomposition()
  epoc_goldenberg_regression_panels()
}

# combined exploratory slope table: every experiment run here, plus Goldenberg by cluster
split_all <- bind_rows(
  bind_rows(all_expl_split) %>% filter(group == "all") %>% mutate(level = as.character(level)),
  if (exists("gb_split") && !is.null(gb_split)) gb_split %>% filter(grouping == "clusters") else NULL
)
if (nrow(split_all)) expl_slope_split_all_md(split_all, file.path(OUT, "exploratory", "all_slope_decomposition.md"))

message("\nDone. Output in ", OUT)
