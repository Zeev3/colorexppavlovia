# =====================================================================================
# EPoC analysis, October 2026 ("analysis_october_26"): runs every step, in order, from the raw data.
# Usage (from this folder):  Rscript run_all.R            all steps
#                            Rscript run_all.R 10        start at step 10 (re-uses the cleaned data in output/)
# Each step's console output goes to output/logs/<step>.log. Results: output/ (see README.md).
# Main analyses = experience instruction only (R/main). Average instruction and comparisons = R/appendix.
# =====================================================================================
args <- commandArgs(trailingOnly = TRUE); START <- if (length(args)) as.numeric(args[1]) else 0
here <- normalizePath(".")
OUT <- file.path(here, "output"); FIG <- file.path(OUT, "figures")
for (d in c(OUT, FIG, file.path(OUT, "logs"))) dir.create(d, showWarnings = FALSE, recursive = TRUE)
steps <- list(
  list(1,  "R/01_clean_conservative.R",                         c("exp1,exp2,exp3,exp4", OUT)),
  list(2,  "R/02_per_person_cells.R",                           OUT),
  list(3,  "R/03_export_clean_data.R",                          OUT),
  list(4,  "R/04_linked_models.R",                              OUT),
  list(10, "R/main/10_experience_only.R",                       OUT),
  list(11, "R/main/11_slope_vs_one.R",                          OUT),
  list(12, "R/main/12_same_report_by_instruction.R",            OUT),
  list(13, "R/main/13_levels_and_serial_dependence.R",          OUT),
  list(14, "R/main/14_r_vs_one.R",                               OUT),
  list(20, "R/checks/20_maximal_model_diagnostics.R",           OUT),
  list(21, "R/checks/21_slope_correlation_lrt.R",               OUT),
  list(22, "R/checks/22_value0_arrays.R",                       c(OUT, OUT)),
  list(23, "R/checks/23_value0_sensitivity.R",                  c(OUT, OUT)),
  list(30, "R/appendix/30_same_report_pooled.R",                OUT),
  list(31, "R/appendix/31_posthocs_both_instructions.R",        OUT),
  list(32, "R/appendix/32_lowest_vs_rest_both_instructions.R",  OUT),
  list(33, "R/appendix/33_exp4_mixed_anova_both_instructions.R", c(OUT, OUT)),
  list(34, "R/appendix/34_numbers_both_instructions.R",         c(OUT, OUT)),
  list(40, "R/figures/40_figures_experience.R",                 c(OUT, FIG)),
  list(41, "R/figures/41_tracking_figures.R",                   c(OUT, FIG)),
  list(42, "R/figures/42_raw_data_views.R",                     c(OUT, FIG)),
  list(43, "R/figures/43_tracking_lines.R",                     c(OUT, FIG)),
  list(44, "R/figures/44_tracking_ellipse_band_drafts.R",       c(OUT, FIG)),
  list(45, "R/figures/45_galton_ellipses.R",                    c(OUT, FIG)),
  list(46, "R/figures/46_slopes_four_ways.R",                   c(OUT, FIG)))
for (s in steps) {
  if (s[[1]] < START) next
  log <- file.path(OUT, "logs", sub("\\.R$", ".log", basename(s[[2]])))
  t0 <- Sys.time(); cat(sprintf("step %02d  %-52s", s[[1]], s[[2]]))
  rc <- system2("Rscript", c(shQuote(file.path(here, s[[2]])), shQuote(s[[3]])), stdout = log, stderr = log)
  cat(sprintf(" %s  (%.1f min)\n", if (rc == 0) "ok" else "FAILED", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  if (rc != 0) stop("step ", s[[1]], " failed; see ", log)
}
cat("done\n")
