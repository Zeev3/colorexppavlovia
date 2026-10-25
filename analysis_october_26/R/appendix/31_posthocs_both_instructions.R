# Pairwise post hocs (Holm) and a linear trend for the forward slope, every experiment and instruction,
# from (a) the per-person two-stage ANOVA and (b) the linked-slopes model. Trend contrasts use the
# order of the levels with equal spacing (emmeans "poly"). Output: posthocs_all.csv.
HERE <- commandArgs(trailingOnly = TRUE)[1]
suppressPackageStartupMessages({ library(dplyr); library(afex); library(emmeans); library(lme4); library(lmerTest) })
emm_options(lmerTest.limit = 30000, pbkrtest.limit = 30000); afex_options(emmeans_model = "multivariate")
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
link <- read.csv(file.path(HERE, "linked_models.csv")) %>% filter(direction == "forward") %>% distinct(experiment, condition, dropped)
out <- list()
add <- function(df, exp, cond, source, kind) {
  df <- as.data.frame(df); out[[length(out) + 1]] <<- data.frame(experiment = exp, condition = cond, source = source, kind = kind,
    contrast = as.character(df$contrast), estimate = df$estimate, SE = df$SE, df = df$df, t = df$t.ratio, p = df$p.value) }
for (id in c("exp1", "exp2", "exp3", "exp4")) {
  d <- read.csv(file.path(HERE, "data", paste0(id, "_clean_conservative.csv"))) %>% mutate(level = factor(noise_level, levels = sort(unique(noise_level))))
  cells <- d %>% group_by(participant_id, condition, level) %>% summarise(slope = coef(lm(indexSelected ~ meanVal))[2], .groups = "drop")
  for (cond in c("average", "experience")) {
    a <- suppressMessages(aov_ez("participant_id", "slope", cells %>% filter(condition == cond), within = "level"))
    em <- emmeans(a, ~ level)
    add(pairs(em, adjust = "holm", reverse = TRUE), id, cond, "per-person ANOVA", "pairwise")
    add(contrast(em, "poly")[1], id, cond, "per-person ANOVA", "linear trend")
    x <- d %>% filter(condition == cond) %>% group_by(participant_id) %>% mutate(true_c = meanVal - mean(meanVal)) %>% ungroup()
    contrasts(x$level) <- contr.sum(nlevels(x$level))
    drp <- strsplit(as.character(link$dropped[link$experiment == id & link$condition == cond]), ",")[[1]]
    keep <- setdiff(levels(x$level), drp)
    f <- as.formula(paste("indexSelected ~ true_c * level + (1 + true_c | participant_id)",
                          if (length(keep)) paste("+", paste(sprintf("(0 + I(true_c * (level == \"%s\")) | participant_id)", keep), collapse = " + ")) else ""))
    m <- suppressWarnings(suppressMessages(lmer(f, data = x, control = ctrl)))
    tr <- emtrends(m, ~ level, var = "true_c")
    add(pairs(tr, adjust = "holm", reverse = TRUE), id, cond, "linked model", "pairwise")
    add(contrast(tr, "poly")[1], id, cond, "linked model", "linear trend")
    message(id, " ", cond, " done")
  }
}
res <- bind_rows(out); write.csv(res, file.path(HERE, "posthocs_all.csv"), row.names = FALSE)
print(res %>% mutate(across(c(estimate, SE, t), ~ round(.x, 3)), p = signif(p, 2)), row.names = FALSE)
