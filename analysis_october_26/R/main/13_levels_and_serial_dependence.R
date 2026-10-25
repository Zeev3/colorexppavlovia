# Exp 4: (1) comparing variance levels directly (pairwise, SD 3 as baseline, linear trend);
# (2) tests of the "leaning on expectation" account: serial dependence by level, and where the
# per-level regression lines cross. Data: data/exp4_clean_conservative.csv.
HERE <- commandArgs(trailingOnly = TRUE)[1]
suppressPackageStartupMessages({ library(dplyr); library(lme4); library(lmerTest); library(emmeans); library(afex) })
emm_options(lmerTest.limit = 30000, pbkrtest.limit = 30000)
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
d <- read.csv(file.path(HERE, "data", "exp4_clean_conservative.csv")) %>% mutate(level = factor(noise_level)) %>%
  arrange(participant_id, trial) %>% group_by(participant_id) %>%
  mutate(true_c = meanVal - mean(meanVal), pm = mean(meanVal),
         prev_true = ifelse(lag(trial) == trial - 1, lag(meanVal), NA), prev_rep = ifelse(lag(trial) == trial - 1, lag(indexSelected), NA)) %>%
  mutate(prev_c = prev_true - mean(meanVal), prevr_c = prev_rep - mean(indexSelected)) %>% ungroup()
f_link <- indexSelected ~ true_c * level + (1 + true_c | participant_id) + (0 + I(true_c * (level == "3")) | participant_id) + (0 + I(true_c * (level == "7")) | participant_id)
for (cond in c("average", "experience")) {
  x <- d %>% filter(condition == cond); contrasts(x$level) <- contr.sum(3)
  m <- lmer(f_link, data = x, control = ctrl); tr <- emtrends(m, ~ level, var = "true_c")
  cat("\n=====", cond, ": slope differences between levels =====\n")
  cat("-- SD 3 as the baseline (treatment contrasts): SD 5 - SD 3, SD 7 - SD 3\n"); print(contrast(tr, "trt.vs.ctrl", ref = 1, adjust = "holm"))
  cat("-- all pairs (Holm)\n"); print(pairs(tr, adjust = "holm", reverse = TRUE))
  cat("-- polynomial: linear trend across 3-5-7 and curvature (quadratic)\n"); print(contrast(tr, "poly"))
  # where the SD 3 and SD 7 lines cross (on the true-colour scale)
  em <- summary(emmeans(m, ~ level | true_c, at = list(true_c = c(0, 1))))
  a <- with(em, emmean[true_c == 0]); b <- with(em, emmean[true_c == 1]) - a
  cross <- (a[3] - a[1]) / (b[1] - b[3]) + mean(x$pm)
  cat(sprintf("-- SD 3 and SD 7 lines cross at true colour %.1f (mean colour shown %.1f; middle of the scale 25.5)\n", cross, mean(x$meanVal)))
  # serial dependence: pull toward the previous trial, per level
  y <- x %>% filter(!is.na(prev_c))
  m2 <- lmer(indexSelected ~ true_c * level + prev_c * level + (1 + true_c + prev_c | participant_id), data = y, control = ctrl)
  cat("-- pull toward the PREVIOUS trial's true colour, per level (shades of report per shade of previous colour)\n")
  t2 <- emtrends(m2, ~ level, var = "prev_c"); print(summary(t2, infer = TRUE)); print(contrast(t2, "poly")); print(anova(m2)["level:prev_c", ])
  m3 <- lmer(indexSelected ~ true_c * level + prevr_c * level + (1 + true_c + prevr_c | participant_id), data = y, control = ctrl)
  cat("-- pull toward the PREVIOUS REPORT, per level\n")
  t3 <- emtrends(m3, ~ level, var = "prevr_c"); print(summary(t3, infer = TRUE)); print(contrast(t3, "poly")); print(anova(m3)["level:prevr_c", ])
}
# per-person ANOVA: linear trend contrast on each person's forward slope
cells <- d %>% group_by(participant_id, condition, level) %>% summarise(slope = coef(lm(indexSelected ~ meanVal))[2], .groups = "drop")
a <- suppressMessages(aov_ez("participant_id", "slope", cells, within = "level", between = "condition"))
cat("\n===== per-person ANOVA, polynomial contrasts on the forward slope =====\n")
print(contrast(emmeans(a, ~ level | condition), "poly"))

# per-person check of the serial dependence: each person's weight on the previous trial, per level
pp <- d %>% filter(!is.na(prev_true)) %>% group_by(participant_id, condition, level) %>%
  summarise(w_prev = coef(lm(indexSelected ~ meanVal + prev_true))[3], .groups = "drop")
a2 <- suppressMessages(aov_ez("participant_id", "w_prev", pp, within = "level", between = "condition"))
cat("\n===== per-person weight on the previous trial's colour =====\n"); print(a2$anova_table, digits = 3)
print(pp %>% group_by(condition, level) %>% summarise(M = mean(w_prev), .groups = "drop"), digits = 3)
print(contrast(emmeans(a2, ~ level | condition), "poly")); print(contrast(emmeans(a2, ~ level), "poly"))
