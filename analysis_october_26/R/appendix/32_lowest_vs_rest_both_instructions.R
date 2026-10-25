# Ran's suggestion: post hoc contrast of the lowest noise level vs the mean of the others, on each person's
# own forward slope (stage-1 cells), per experiment and instruction. Paired t-test.
SP <- commandArgs(trailingOnly = TRUE)[1]
suppressPackageStartupMessages(library(dplyr))
cells <- readRDS(file.path(SP, "cells_all.rds")); out <- list()
for (nm in names(cells)) {
  c <- cells[[nm]]; noise <- if ("variance_level" %in% names(c)) "variance_level" else "array_length"
  lv <- sort(unique(as.numeric(as.character(c[[noise]])))); low <- lv[1]
  for (cond in c("average", "experience")) for (meas in c("slope_fwd", "r")) {
    x <- c %>% filter(condition == cond) %>% mutate(L = as.numeric(as.character(.data[[noise]]))) %>% group_by(participant_id) %>%
      summarise(lo = mean(.data[[meas]][L == low]), rest = mean(.data[[meas]][L != low]), .groups = "drop") %>% filter(is.finite(lo), is.finite(rest))
    t <- t.test(x$lo, x$rest, paired = TRUE)
    out[[length(out) + 1]] <- data.frame(experiment = unique(c$experiment)[1], condition = cond, measure = meas, low_level = low,
      n = nrow(x), mean_low = mean(x$lo), mean_rest = mean(x$rest), diff = mean(x$lo - x$rest), t = unname(t$statistic), df = unname(t$parameter), p = t$p.value)
  }
}
res <- bind_rows(out); write.csv(res, file.path(SP, "contrast_low_vs_rest.csv"), row.names = FALSE); print(res, digits = 3)
