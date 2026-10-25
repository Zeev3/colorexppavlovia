# Dump every number the full deck quotes for Exps 1-4 into full_numbers.json (read by the deck builder).
SP <- commandArgs(trailingOnly = TRUE)[1]; OUT <- commandArgs(trailingOnly = TRUE)[2]
suppressPackageStartupMessages({ library(dplyr); library(jsonlite); library(afex) })
df <- function(x) as.data.frame(x) %>% mutate(across(where(is.factor), as.character))
res <- list()
for (id in c("exp1", "exp2", "exp3", "exp4")) {
  r <- readRDS(file.path(SP, paste0(id, "_conservative.rds")))
  res[[id]] <- list(
    exclusions = df(r$exclusions), trial_loss = df(r$trial_loss),
    clean = df(r$data %>% count(condition)),
    corr_summary = df(r$corr$summary), corr_anova = df(r$corr$anova),
    anova = df(r$anovas$anova),
    windows_means = df(r$tails$windows$means), windows_anova = df(r$tails$windows$anova),
    fixed_means = df(r$tails$fixed$means), fixed_anova = df(r$tails$fixed$anova),
    exg_summary = df(r$exg$summary), exg_anova = df(r$exg$anova))
  noise <- if (id == "exp4") "variance_level" else "array_length"; cells <- as.data.frame(r$anovas$cells); rm_out <- list()
  for (cond in c("average", "experience")) for (meas in c("slope_fwd", "slope_rev", "r")) {
    x <- cells %>% filter(condition == cond); if (meas == "r") x$r <- atanh(pmin(x$r, .9999))
    a <- suppressMessages(suppressWarnings(aov_ez("participant_id", meas, x, within = noise, type = 3)))
    t <- as.data.frame(a$anova_table)
    rm_out[[length(rm_out) + 1]] <- data.frame(condition = cond, measure = meas, df1 = t[1, "num Df"], df2 = t[1, "den Df"], F = t[1, "F"], p = t[1, "Pr(>F)"], ges = t[1, "ges"])
  }
  res[[id]]$rm_anova <- bind_rows(rm_out)
}
write_json(res, file.path(OUT, "full_numbers.json"), digits = 6, auto_unbox = TRUE)
