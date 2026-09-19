# Render the walkthrough document for each colour experiment, each in a fresh R
# process (the pipeline is sourced per document). Needs pandoc: set
# RSTUDIO_PANDOC to RStudio's bundled copy if pandoc is not on the PATH, and run
# with a UTF-8 locale (LC_ALL=en_US.UTF-8).
#   Rscript render_walkthroughs.R            # all
#   Rscript render_walkthroughs.R exp2 exp4  # some
ids <- commandArgs(trailingOnly = TRUE)
if (!length(ids)) ids <- c("exp1", "exp2", "exp3", "exp4")
titles <- c(exp1 = "Experiment 1 (array size 2/6/8/12): a walkthrough of the results",
            exp2 = "Experiment 2 (array size 2/10/20): a walkthrough of the results",
            exp3 = "Experiment 3 (array size 2/10/20 + Mondrian frame): a walkthrough of the results",
            exp4 = "Experiment 4 (stimulus variance SD 3/5/7): a walkthrough of the results")
for (id in ids) {
  cmd <- sprintf("rmarkdown::render('experiment_walkthrough.Rmd', params = list(exp_id = '%s', title = '%s'), output_file = '%s_walkthrough', output_format = 'all', quiet = TRUE)",
                 id, titles[[id]], id)
  message("rendering ", id)
  status <- system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(cmd)))
  if (status != 0) stop("rendering failed for ", id)
}
