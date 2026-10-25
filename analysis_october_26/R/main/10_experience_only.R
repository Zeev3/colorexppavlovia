# =====================================================================================
# MAIN ANALYSES, EXPERIENCE INSTRUCTION ONLY (all four slider experiments).
# Nothing here uses the average-instruction participants: no pooling, no between factor.
# Data: data/<exp>_clean_conservative.csv (true colour = displayed average, report + 1, exclusions applied).
# Output: experience_only_results.csv (one row per test) and experience_only_cells.csv (stage-1 numbers).
#
# Stage-1 measures, computed for each participant at each noise level from that person's trials:
#   slope_fwd   OLS slope of report on true colour             lm(report ~ true)
#   slope_rev   OLS slope of true colour on report             lm(true ~ report)
#   r           Pearson correlation of report and true colour  (tested as Fisher z = atanh(r))
#   sd_rep      SD of the person's reports
#   sd_true     SD of the true colours the person saw          (control: should not change)
#   bias        mean of (report - true)
#   extreme     share of trials with report < 14 when true < 20, or report > 36 when true > 30
#   w_prev      weight on the previous trial's true colour     lm(report ~ true + previous true)
# Stage 2: repeated-measures ANOVA, noise level within participants (Greenhouse-Geisser),
# pairwise post hocs (Holm) and a linear trend contrast (levels in order, equal spacing).
# =====================================================================================
HERE <- commandArgs(trailingOnly = TRUE)[1]
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(afex); library(emmeans); library(lme4); library(lmerTest) })
emm_options(lmerTest.limit = 30000, pbkrtest.limit = 30000); afex_options(emmeans_model = "multivariate")
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)); set.seed(1)
res <- list(); cells_out <- list()
row <- function(...) res[[length(res) + 1]] <<- data.frame(..., stringsAsFactors = FALSE)
fmtP <- function(p) ifelse(p < .001, "< .001", sub("^0", "", sprintf(ifelse(p < .1, "%.3f", "%.2f"), p)))

for (id in c("exp1", "exp2", "exp3", "exp4")) {
  d <- read.csv(file.path(HERE, "data", paste0(id, "_clean_conservative.csv"))) %>% filter(condition == "experience") %>%
    mutate(level = factor(noise_level, levels = sort(unique(noise_level)))) %>% arrange(participant_id, trial) %>%
    group_by(participant_id) %>% mutate(true_c = meanVal - mean(meanVal), resp_c = indexSelected - mean(indexSelected),
      prev_true = ifelse(lag(trial) == trial - 1, lag(meanVal), NA)) %>% ungroup()
  lv <- levels(d$level); nP <- n_distinct(d$participant_id)
  row(experiment = id, analysis = "sample", test = "participants / trials", value = sprintf("%d people, %d trials", nP, nrow(d)), p = NA)

  # ---------- stage 1
  cells <- d %>% group_by(participant_id, level) %>% summarise(
    slope_fwd = coef(lm(indexSelected ~ meanVal))[2], slope_rev = coef(lm(meanVal ~ indexSelected))[2],
    r = cor(meanVal, indexSelected), sd_rep = sd(indexSelected), sd_true = sd(meanVal), bias = mean(indexSelected - meanVal),
    extreme = mean((meanVal < 20 & indexSelected < 14) | (meanVal > 30 & indexSelected > 36)),
    n = n(), .groups = "drop") %>% mutate(z = atanh(pmin(r, .9999)), experiment = id) %>%
    left_join(d %>% filter(!is.na(prev_true)) %>% group_by(participant_id, level) %>%
                summarise(w_prev = coef(lm(indexSelected ~ meanVal + prev_true))[3], .groups = "drop"), by = c("participant_id", "level"))
  cells_out[[id]] <- cells

  # ---------- stage 2: RM ANOVA per measure
  for (meas in c("slope_fwd", "slope_rev", "z", "sd_rep", "sd_true", "bias", "extreme", "w_prev")) {
    a <- suppressMessages(aov_ez("participant_id", meas, cells, within = "level")); t <- as.data.frame(a$anova_table)
    m <- cells %>% group_by(level) %>% summarise(M = if (meas == "z") tanh(mean(z)) else mean(.data[[meas]]), .groups = "drop")
    row(experiment = id, analysis = paste0("RM ANOVA: ", meas), test = "means by level",
        value = paste(sprintf(if (meas %in% c("extreme")) "%.3f" else "%.3f", m$M), collapse = " / "), p = NA)
    row(experiment = id, analysis = paste0("RM ANOVA: ", meas), test = "level effect",
        value = sprintf("F(%.2f, %.1f) = %.2f, ges = %.3f", t[1, "num Df"], t[1, "den Df"], t[1, "F"], t[1, "ges"]), p = t[1, "Pr(>F)"])
    em <- emmeans(a, ~ level)
    pr <- as.data.frame(pairs(em, adjust = "holm", reverse = TRUE))
    for (k in seq_len(nrow(pr))) row(experiment = id, analysis = paste0("RM ANOVA: ", meas), test = paste("pair", pr$contrast[k]),
                                     value = sprintf("%+.3f, t(%d) = %.2f", pr$estimate[k], round(pr$df[k]), pr$t.ratio[k]), p = pr$p.value[k])
    ln <- as.data.frame(contrast(em, "poly"))[1, ]
    row(experiment = id, analysis = paste0("RM ANOVA: ", meas), test = "linear trend", value = sprintf("%+.3f, t(%d) = %.2f", ln$estimate, round(ln$df), ln$t.ratio), p = ln$p.value)
  }
  # Ran's contrast: lowest level vs the mean of the others (forward slope)
  lo <- cells %>% group_by(participant_id) %>% summarise(a = slope_fwd[level == lv[1]], b = mean(slope_fwd[level != lv[1]]))
  tt <- t.test(lo$a, lo$b, paired = TRUE)
  row(experiment = id, analysis = "RM ANOVA: slope_fwd", test = "lowest level vs the rest", value = sprintf("%.3f vs %.3f, t(%d) = %.2f", mean(lo$a), mean(lo$b), round(tt$parameter), tt$statistic), p = tt$p.value)

  # ---------- linked-slopes models, forward and reverse
  contrasts(d$level) <- contr.sum(nlevels(d$level))
  for (dir in c("forward", "reverse")) {
    x <- if (dir == "forward") "true_c" else "resp_c"; y <- if (dir == "forward") "indexSelected" else "meanVal"
    base <- if (dir == "forward") sprintf("(1 + %s | participant_id)", x) else sprintf("(0 + %s | participant_id)", x)
    keep <- lv
    repeat {
      f <- as.formula(paste(sprintf("%s ~ %s * level +", y, x), paste(c(base, if (length(keep)) sprintf("(0 + I(%s * (level == \"%s\")) | participant_id)", x, keep)), collapse = " + ")))
      m <- suppressWarnings(suppressMessages(lmer(f, data = d, control = ctrl)))
      vc <- as.data.frame(VarCorr(m)); z0 <- vc$var1[grepl("^I\\(", vc$var1) & is.na(vc$var2) & vc$sdcor < 1e-4]
      if (!length(z0) || !length(keep)) break
      keep <- keep[!sapply(keep, function(k) any(grepl(sprintf("== \"%s\"", k), z0, fixed = TRUE)))]
    }
    tr <- emtrends(m, ~ level, var = x); s <- as.data.frame(summary(tr, infer = TRUE)); a <- anova(m)[paste0(x, ":level"), ]
    lab <- paste("linked model", dir)
    row(experiment = id, analysis = lab, test = "slopes by level [95% CI]",
        value = paste(sprintf("%.3f [%.2f, %.2f]", s[[paste0(x, ".trend")]], s$lower.CL, s$upper.CL), collapse = " / "), p = NA)
    row(experiment = id, analysis = lab, test = "slope x level", value = sprintf("F(%d, %.1f) = %.2f", a$NumDF, a$DenDF, a$`F value`), p = a$`Pr(>F)`)
    row(experiment = id, analysis = lab, test = "clean fit", value = as.character(!isSingular(m, tol = 1e-4) && !length(m@optinfo$conv$lme4$messages)), p = NA)
    pr <- as.data.frame(pairs(tr, adjust = "holm", reverse = TRUE))
    for (k in seq_len(nrow(pr))) row(experiment = id, analysis = lab, test = paste("pair", pr$contrast[k]), value = sprintf("%+.3f", pr$estimate[k]), p = pr$p.value[k])
    ln <- as.data.frame(contrast(tr, "poly"))[1, ]
    row(experiment = id, analysis = lab, test = "linear trend", value = sprintf("%+.3f", ln$estimate), p = ln$p.value)
  }

  # ---------- extremes: report windows (+-2 around 10 and 40), true colour behind them by level
  w <- d %>% mutate(win = case_when(abs(indexSelected - 10) <= 2 ~ "near 10", abs(indexSelected - 40) <= 2 ~ "near 40")) %>% filter(!is.na(win))
  for (wd in c("near 10", "near 40")) {
    x <- w %>% filter(win == wd)
    if (nrow(x) < 20 * length(lv) || any(table(x$level) < 10)) { row(experiment = id, analysis = "report window", test = wd, value = "too few trials", p = NA); next }
    m <- lmer(meanVal ~ level + (1 | participant_id), data = x); e <- as.data.frame(emmeans(m, ~ level)); a <- anova(m)
    row(experiment = id, analysis = "report window", test = paste(wd, ": true colour by level"), value = paste(sprintf("%.1f", e$emmean), collapse = " / "), p = a$`Pr(>F)`[1])
  }
}
res <- bind_rows(res) %>% mutate(p_txt = ifelse(is.na(p), "", fmtP(p)))
write.csv(res, file.path(HERE, "experience_only_results.csv"), row.names = FALSE)
write.csv(bind_rows(cells_out), file.path(HERE, "experience_only_cells.csv"), row.names = FALSE)
cat("done:", nrow(res), "rows\n")
