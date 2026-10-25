# Every Exp 4 number used in the analysis guide (ANOVAs with post hocs, model coefficients).
SP <- commandArgs(trailingOnly = TRUE)[1]; HERE <- commandArgs(trailingOnly = TRUE)[2]
suppressPackageStartupMessages({ library(dplyr); library(afex); library(emmeans); library(lme4); library(lmerTest) })
emm_options(lmerTest.limit = 30000, pbkrtest.limit = 30000); afex_options(emmeans_model = "multivariate")
cells <- as.data.frame(readRDS(file.path(SP, "cells_all.rds"))[[4]]); cells$z <- atanh(pmin(cells$r, .9999))
cat("cells:", unique(cells$experiment), nrow(cells), "rows\n")
for (meas in c("slope_fwd", "slope_rev", "z", "sd_response", "sd_objective", "bias")) {
  cat("\n=================", meas, "=================\n")
  a <- suppressMessages(aov_ez("participant_id", meas, cells, within = "variance_level", between = "condition", type = 3))
  print(a$anova_table, digits = 3)
  print(cells %>% group_by(condition, variance_level) %>% summarise(M = mean(.data[[meas]]), SD = sd(.data[[meas]]), .groups = "drop"), digits = 3)
  cat("-- post hoc: levels, pooled over instruction (Holm)\n"); print(pairs(emmeans(a, ~ variance_level), adjust = "holm"), digits = 3)
  cat("-- post hoc: levels within each instruction (Holm)\n"); print(pairs(emmeans(a, ~ variance_level | condition), adjust = "holm"), digits = 3)
}
d <- read.csv(file.path(HERE, "data", "exp4_clean_conservative.csv")) %>% mutate(level = factor(noise_level)) %>%
  group_by(participant_id) %>% mutate(true_c = meanVal - mean(meanVal)) %>% ungroup()
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
for (cond in c("average", "experience")) {
  x <- d %>% filter(condition == cond); contrasts(x$level) <- contr.sum(3)
  f <- if (cond == "average") indexSelected ~ true_c * level + (1 + true_c | participant_id) + (0 + I(true_c * (level == "3")) | participant_id) + (0 + I(true_c * (level == "7")) | participant_id) else
                              indexSelected ~ true_c * level + (1 + true_c | participant_id) + (0 + I(true_c * (level == "3")) | participant_id) + (0 + I(true_c * (level == "7")) | participant_id)
  m <- lmer(f, data = x, control = ctrl)
  cat("\n================= linked forward model,", cond, "=================\n"); print(round(coef(summary(m)), 4))
  print(anova(m))
  cat("-- slopes per level and pairwise (Holm)\n"); tr <- emtrends(m, ~ level, var = "true_c"); print(tr); print(pairs(tr, adjust = "holm"))
}
