# Do a person's slopes at different noise levels really correlate?
#  (1) Pearson correlation of per-person OLS slopes between each pair of levels, with 95% CI and p
#  (2) the same corrected for measurement error: r_true = r_obs / sqrt(rel_a * rel_b), where the
#      reliability of the slopes at a level = (variance of slopes - mean squared SE) / variance of slopes
#  (3) likelihood-ratio test, linked vs unlinked model (unlinked = linked with the shared slope variance
#      fixed at 0), with the boundary correction (mixture of chi-square 1 and 2 df)
SP <- commandArgs(trailingOnly = TRUE)[1]
suppressPackageStartupMessages({ library(lme4); library(dplyr); library(tidyr) })
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
pairs_out <- list(); lrt_out <- list()
for (id in c("exp1", "exp2", "exp3", "exp4")) {
  dat <- readRDS(file.path(SP, paste0(id, "_conservative.rds")))$data
  noise <- if (id == "exp4") "variance_level" else "array_length"
  for (cond in c("average", "experience")) {
    d <- dat %>% filter(condition == cond) %>% droplevels()
    # per-person, per-level OLS slope and its standard error
    s <- d %>% group_by(participant_id, lv = .data[[noise]]) %>%
      group_modify(~ { m <- lm(indexSelected ~ meanVal, data = .x); cf <- summary(m)$coefficients
        tibble(slope = cf["meanVal", 1], se = cf["meanVal", 2]) }) %>% ungroup()
    rel <- s %>% group_by(lv) %>% summarise(v = var(slope), e = mean(se^2), rel = (v - e) / v, .groups = "drop")
    w <- s %>% select(participant_id, lv, slope) %>% pivot_wider(names_from = lv, values_from = slope)
    lv <- levels(d[[noise]])
    for (i in 1:(length(lv) - 1)) for (j in (i + 1):length(lv)) {
      a <- w[[lv[i]]]; b <- w[[lv[j]]]; ok <- is.finite(a) & is.finite(b); ct <- cor.test(a[ok], b[ok])
      ra <- rel$rel[rel$lv == lv[i]]; rb <- rel$rel[rel$lv == lv[j]]
      pairs_out[[length(pairs_out) + 1]] <- data.frame(experiment = id, condition = cond, pair = paste(lv[i], "vs", lv[j]),
        n = sum(ok), r = unname(ct$estimate), ci_low = ct$conf.int[1], ci_high = ct$conf.int[2], p = ct$p.value,
        rel_a = ra, rel_b = rb, r_corrected = unname(ct$estimate) / sqrt(ra * rb))
    }
    # LRT: linked vs unlinked (ML fits)
    contrasts(d[[noise]]) <- contr.sum(nlevels(d[[noise]]))
    per <- paste(sprintf("(0 + I(meanVal_c * (%s == \"%s\")) | participant_id)", noise, lv), collapse = " + ")
    fu <- as.formula(sprintf("indexSelected ~ meanVal_c * %s + (1 | participant_id) + %s", noise, per))
    fl <- as.formula(sprintf("indexSelected ~ meanVal_c * %s + (1 + meanVal_c | participant_id) + %s", noise, per))
    mu <- suppressWarnings(suppressMessages(lmer(fu, d, REML = FALSE, control = ctrl)))
    ml <- suppressWarnings(suppressMessages(lmer(fl, d, REML = FALSE, control = ctrl)))
    chi <- 2 * (as.numeric(logLik(ml)) - as.numeric(logLik(mu)))
    p_mix <- 0.5 * pchisq(chi, 1, lower.tail = FALSE) + 0.5 * pchisq(chi, 2, lower.tail = FALSE)
    vc <- as.data.frame(VarCorr(ml)); sh <- vc$sdcor[vc$grp == "participant_id" & vc$var1 == "meanVal_c" & is.na(vc$var2)]
    devs <- vc$sdcor[grepl("^participant_id\\.", vc$grp) & is.na(vc$var2)]
    implied <- sh^2 / sqrt(outer(sh^2 + devs^2, sh^2 + devs^2))
    lrt_out[[length(lrt_out) + 1]] <- data.frame(experiment = id, condition = cond, chisq = chi, p_boundary_corrected = p_mix,
      AIC_unlinked = AIC(mu), AIC_linked = AIC(ml), shared_slope_sd = sh,
      implied_cor_range = sprintf("%.2f–%.2f", min(implied[upper.tri(implied)]), max(implied[upper.tri(implied)])))
    message(id, " ", cond, " done")
  }
}
write.csv(do.call(rbind, pairs_out), file.path(SP, "slope_cor_pairs.csv"), row.names = FALSE)
write.csv(do.call(rbind, lrt_out), file.path(SP, "slope_cor_lrt.csv"), row.names = FALSE)
