suppressPackageStartupMessages({ library(dplyr) })
SP <- commandArgs(trailingOnly = TRUE)[1]
sink(file.path(SP, "extract.txt"))
cells_all <- list()
for (id in c("exp1", "exp2", "exp3", "exp4")) {
  r <- readRDS(file.path(SP, paste0(id, "_conservative.rds")))
  cat("\n\n==================", id, "\n")
  cat("-- exclusions\n"); print(as.data.frame(r$exclusions))
  cat("-- trial loss\n"); print(as.data.frame(r$trial_loss))
  cat("-- clean trials per condition\n"); print(table(r$data$condition))
  cat("-- tracking summary\n"); print(as.data.frame(r$corr$summary))
  cat("-- tracking anova\n"); print(as.data.frame(r$corr$anova))
  cat("-- two-stage anova\n"); a <- as.data.frame(r$anovas$anova); print(a[a$measure %in% c("slope_fwd", "slope_rev", "r", "sd_response", "bias", "sd_objective"), ])
  cat("-- two-stage means\n"); m <- as.data.frame(r$anovas$means); print(m[m$measure %in% c("slope_fwd", "slope_rev", "r", "sd_response", "bias"), ])
  cat("-- exaggeration rate\n"); print(as.data.frame(r$exg$summary)); print(as.data.frame(r$exg$anova))
  for (t in names(r$tails)) { cat("-- tails", t, "\n"); x <- r$tails[[t]]; for (k in names(x)) if (is.data.frame(x[[k]])) { cat("  .", k, "\n"); print(as.data.frame(x[[k]])) } }
  cat("-- report to true (shift removed)\n"); if (!is.null(r$r2t)) print(unique(as.data.frame(r$r2t)[, c("version", "tail", "n_trials", "level_effect_p")]))
  cells <- as.data.frame(r$anovas$cells); cells$experiment <- id; cells_all[[id]] <- cells
}
sink()
saveRDS(cells_all, file.path(SP, "cells_all.rds"))
