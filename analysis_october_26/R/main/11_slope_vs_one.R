# Is the slope at each level different from 1? Experience instruction only, every experiment.
# (a) per person: one-sample t-test of the people's own OLS slopes against 1, Holm across levels;
# (b) linked model: each level's slope tested against 1 (emtrends, null = 1, Holm across levels).
# Forward (1 = report follows the colour exactly) and reverse. Output: slope_vs_one.csv.
HERE <- commandArgs(trailingOnly = TRUE)[1]
suppressPackageStartupMessages({ library(dplyr); library(emmeans); library(lme4); library(lmerTest) })
emm_options(lmerTest.limit = 30000, pbkrtest.limit = 30000)
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
cells <- read.csv(file.path(HERE, "experience_only_cells.csv"))
link <- read.csv(file.path(HERE, "linked_models.csv")) %>% filter(condition == "experience") %>% distinct(experiment, direction, dropped)
out <- list()
for (id in c("exp1", "exp2", "exp3", "exp4")) {
  for (meas in c("slope_fwd", "slope_rev")) {
    c1 <- cells %>% filter(experiment == id); lv <- sort(unique(c1$level))
    pp <- sapply(lv, function(l) t.test(c1[[meas]][c1$level == l], mu = 1)$p.value)
    for (k in seq_along(lv)) { x <- c1[[meas]][c1$level == lv[k]]; tt <- t.test(x, mu = 1)
      out[[length(out) + 1]] <- data.frame(experiment = id, direction = ifelse(meas == "slope_fwd", "forward", "reverse"), source = "per-person t-test",
        level = lv[k], slope = mean(x), ci_low = tt$conf.int[1], ci_high = tt$conf.int[2], diff_from_1 = mean(x) - 1,
        t = unname(tt$statistic), df = unname(tt$parameter), p_holm = p.adjust(pp, "holm")[k], n_below_1 = sum(x < 1), n = length(x)) }
  }
  d <- read.csv(file.path(HERE, "data", paste0(id, "_clean_conservative.csv"))) %>% filter(condition == "experience") %>%
    mutate(level = factor(noise_level, levels = sort(unique(noise_level)))) %>% group_by(participant_id) %>%
    mutate(true_c = meanVal - mean(meanVal), resp_c = indexSelected - mean(indexSelected)) %>% ungroup()
  contrasts(d$level) <- contr.sum(nlevels(d$level))
  for (dir in c("forward", "reverse")) {
    x <- if (dir == "forward") "true_c" else "resp_c"; y <- if (dir == "forward") "indexSelected" else "meanVal"
    drp <- strsplit(as.character(link$dropped[link$experiment == id & link$direction == dir]), ",")[[1]]; keep <- setdiff(levels(d$level), drp)
    base <- if (dir == "forward") sprintf("(1 + %s | participant_id)", x) else sprintf("(0 + %s | participant_id)", x)
    f <- as.formula(paste(sprintf("%s ~ %s * level +", y, x), paste(c(base, if (length(keep)) sprintf("(0 + I(%s * (level == \"%s\")) | participant_id)", x, keep)), collapse = " + ")))
    m <- suppressWarnings(suppressMessages(lmer(f, data = d, control = ctrl)))
    s <- as.data.frame(summary(emtrends(m, ~ level, var = x), null = 1, infer = TRUE, adjust = "holm"))
    out[[length(out) + 1]] <- data.frame(experiment = id, direction = dir, source = "linked model", level = as.numeric(as.character(s$level)),
      slope = s[[paste0(x, ".trend")]], ci_low = s$lower.CL, ci_high = s$upper.CL, diff_from_1 = s[[paste0(x, ".trend")]] - 1,
      t = s$t.ratio, df = s$df, p_holm = s$p.value, n_below_1 = NA, n = NA)
  }
}
res <- bind_rows(out); write.csv(res, file.path(HERE, "slope_vs_one.csv"), row.names = FALSE)
print(res %>% mutate(across(c(slope, ci_low, ci_high, diff_from_1, t, df), ~ round(.x, 3)), p_holm = signif(p_holm, 2)), row.names = FALSE)
