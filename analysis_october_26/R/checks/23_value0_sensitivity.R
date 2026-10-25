# Exp 4 sensitivity to the 12 clean trials whose array contained a value-0 square (drawn as shade 50):
# main tests as analysed / with those trials dropped / with their true colour corrected (0 counted as 50).
SP <- commandArgs(trailingOnly = TRUE)[1]; OUT <- commandArgs(trailingOnly = TRUE)[2]
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(lme4); library(lmerTest); library(afex) })
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)); set.seed(1)
d0 <- readRDS(file.path(SP, "exp4_conservative.rds"))$data %>% mutate(participant_id = as.character(participant_id))
z <- read.csv(file.path(OUT, "exp4_zero_arrays.csv")) %>% select(participant_id, trial, corrected_mean)
recentre <- function(d) d %>% group_by(participant_id) %>% mutate(meanVal_c = meanVal - mean(meanVal), resp_c = indexSelected - mean(indexSelected)) %>% ungroup()
V <- list(as_analysed = d0,
          dropped = d0 %>% anti_join(z, by = c("participant_id", "trial")) %>% recentre(),
          corrected = d0 %>% left_join(z, by = c("participant_id", "trial")) %>% mutate(meanVal = ifelse(is.na(corrected_mean), meanVal, corrected_mean)) %>% select(-corrected_mean) %>% recentre())
lk <- read.csv(file.path(OUT, "linked_models.csv")) %>% filter(experiment == "exp4") %>% distinct(condition, direction, dropped)
out <- list()
for (v in names(V)) for (cond in c("average", "experience")) {
  d <- V[[v]] %>% filter(condition == cond) %>% droplevels(); contrasts(d$variance_level) <- contr.sum(3)
  for (dir in c("forward", "reverse")) {
    x <- if (dir == "forward") "meanVal_c" else "resp_c"; y <- if (dir == "forward") "indexSelected" else "meanVal"
    drp <- strsplit(as.character(lk$dropped[lk$condition == cond & lk$direction == dir]), ",")[[1]]; keep <- setdiff(c("3", "5", "7"), drp)
    base <- if (dir == "forward") sprintf("(1 + %s | participant_id)", x) else sprintf("(0 + %s | participant_id)", x)
    f <- as.formula(paste(sprintf("%s ~ %s * variance_level +", y, x), paste(c(base, sprintf("(0 + I(%s * (variance_level == \"%s\")) | participant_id)", x, keep)), collapse = " + ")))
    m <- suppressWarnings(suppressMessages(lmer(f, data = d, control = ctrl))); a <- anova(m)[paste0(x, ":variance_level"), ]
    em <- fixef(m); s3 <- em[x] + em[paste0(x, ":variance_level1")]; s5 <- em[x] + em[paste0(x, ":variance_level2")]; s7 <- em[x] - em[paste0(x, ":variance_level1")] - em[paste0(x, ":variance_level2")]
    out[[length(out) + 1]] <- data.frame(version = v, condition = cond, test = paste("linked", dir), est = sprintf("%.3f / %.3f / %.3f", s3, s5, s7), F = a$`F value`, p = a$`Pr(>F)`)
  }
  cells <- d %>% group_by(participant_id, variance_level) %>% summarise(slope_fwd = coef(lm(indexSelected ~ meanVal))[2], slope_rev = coef(lm(meanVal ~ indexSelected))[2], .groups = "drop")
  for (meas in c("slope_fwd", "slope_rev")) { a <- suppressMessages(aov_ez("participant_id", meas, cells, within = "variance_level")); t <- as.data.frame(a$anova_table)
    out[[length(out) + 1]] <- data.frame(version = v, condition = cond, test = paste("per-person ANOVA", meas), est = "", F = t$F[1], p = t$`Pr(>F)`[1]) }
}
# same-report test, SD 7 vs 3, pooled and per instruction
sr <- function(d, side, B = 2000, MIN = 5) {
  d <- d %>% mutate(tail = case_when(indexSelected <= 14 ~ "low", indexSelected >= 36 ~ "high"), lvl = as.character(variance_level)) %>% filter(tail == side, lvl %in% c("3", "7")) %>% mutate(key = paste(indexSelected, lvl, sep = "|"))
  agg <- d %>% group_by(participant_id, key) %>% summarise(s = sum(meanVal), n = n(), .groups = "drop")
  S <- agg %>% select(-n) %>% pivot_wider(names_from = key, values_from = s, values_fill = 0); N <- agg %>% select(-s) %>% pivot_wider(names_from = key, values_from = n, values_fill = 0)
  Sm <- as.matrix(S[, -1]); Nm <- as.matrix(N[, colnames(S)[-1]]); keys <- colnames(Sm); vals <- sort(unique(as.integer(sub("\\|.*", "", keys))))
  est <- function(w) { s <- colSums(Sm * w); n <- colSums(Nm * w); sapply(vals, function(v) { a <- paste(v, "3", sep = "|"); b <- paste(v, "7", sep = "|")
    if (!(a %in% keys && b %in% keys) || n[a] < MIN || n[b] < MIN) return(NA); dif <- s[b] / n[b] - s[a] / n[a]; if (side == "high") -dif else dif }) }
  l0 <- est(rep(1, nrow(Sm))); ok <- !is.na(l0)
  bt <- replicate(B, { w <- tabulate(sample.int(nrow(Sm), replace = TRUE), nbins = nrow(Sm)); mean(est(w)[ok], na.rm = TRUE) })
  data.frame(est = sprintf("%+.2f [%.2f, %.2f]", mean(l0[ok]), quantile(bt, .025, na.rm = TRUE), quantile(bt, .975, na.rm = TRUE)), F = NA, p = min(1, 2 * min(mean(bt <= 0, na.rm = TRUE), mean(bt >= 0, na.rm = TRUE))))
}
for (v in names(V)) for (cond in c("pooled", "average", "experience")) for (side in c("low", "high")) {
  d <- if (cond == "pooled") V[[v]] else V[[v]] %>% filter(condition == cond)
  out[[length(out) + 1]] <- cbind(data.frame(version = v, condition = cond, test = paste("same report 7 vs 3,", ifelse(side == "low", "pale", "vivid"))), sr(d, side))
}
res <- bind_rows(out); write.csv(res, file.path(OUT, "exp4_zero_sensitivity.csv"), row.names = FALSE)
w <- res %>% mutate(cell = ifelse(is.na(F), sprintf("%s p=%.3f", est, p), sprintf("%s F=%.2f p=%.2g", est, F, p))) %>% select(condition, test, version, cell) %>% pivot_wider(names_from = version, values_from = cell)
print(as.data.frame(w), right = FALSE)
