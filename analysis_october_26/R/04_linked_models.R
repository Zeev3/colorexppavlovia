# The "linked slopes" model, on the conservative data (displayed mean, reports + 1):
#   report ~ true_c * level + (1 + true_c | person) + per-level slope deviations (uncorrelated) | person
# Each person has one overall slope (correlated with their intercept); each level adds its own
# deviation. Because every level shares the person's overall slope, their slopes at different levels
# are correlated. A deviation whose variance is estimated at exactly 0 is dropped and the model refitted
# (same fit, no boundary). Also fitted: per-level slopes with a free correlation matrix, for comparison.
SP <- commandArgs(trailingOnly = TRUE)[1]
suppressPackageStartupMessages({ library(lme4); library(lmerTest); library(emmeans); library(dplyr) })
emm_options(lmerTest.limit = 30000, pbkrtest.limit = 30000)
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
clean <- function(m) length(m@optinfo$conv$lme4$messages %||% character(0)) == 0 && !isSingular(m, tol = 1e-4)
`%||%` <- function(a, b) if (is.null(a)) b else a
out <- list(); vcs <- list()
for (id in c("exp1", "exp2", "exp3", "exp4")) {
  r <- readRDS(file.path(SP, paste0(id, "_conservative.rds"))); dat <- r$data
  noise <- if (id == "exp4") "variance_level" else "array_length"
  for (cond in c("average", "experience")) for (dir in c("forward", "reverse")) {
    d <- dat %>% filter(condition == cond) %>% droplevels(); contrasts(d[[noise]]) <- contr.sum(nlevels(d[[noise]]))
    x <- if (dir == "forward") "meanVal_c" else "resp_c"; y <- if (dir == "forward") "indexSelected" else "meanVal"
    lv <- levels(d[[noise]]); keep <- lv; dropped <- character()
    base <- if (dir == "forward") sprintf("(1 + %s | participant_id)", x) else sprintf("(0 + %s | participant_id)", x)
    repeat {
      dev <- if (length(keep)) paste(sprintf("(0 + I(%s * (%s == \"%s\")) | participant_id)", x, noise, keep), collapse = " + ") else NULL
      f <- as.formula(paste(sprintf("%s ~ %s * %s +", y, x, noise), paste(c(base, dev), collapse = " + ")))
      m <- suppressWarnings(suppressMessages(lmer(f, data = d, control = ctrl)))
      vc <- as.data.frame(VarCorr(m)); devs <- vc[grepl("^participant_id\\.", vc$grp) & is.na(vc$var2), ]
      zero <- devs$sdcor < 1e-4
      if (!any(zero) || !length(keep)) break
      gone <- keep[which(zero)[1]]; dropped <- c(dropped, gone); keep <- setdiff(keep, gone)
    }
    a <- as.data.frame(anova(m)); key <- paste0(x, ":", noise)
    tr <- as.data.frame(summary(emtrends(m, as.formula(paste("~", noise)), var = x, lmer.df = "satterthwaite"), infer = TRUE))
    out[[length(out) + 1]] <- data.frame(experiment = id, condition = cond, direction = dir, model = "linked",
      formula = deparse1(f), dropped = paste(dropped, collapse = ","), converged_clean = clean(m),
      level = tr[[noise]], slope = tr[[paste0(x, ".trend")]], ci_low = tr$lower.CL, ci_high = tr$upper.CL,
      F = a[key, "F value"], df1 = a[key, "NumDF"], df2 = a[key, "DenDF"], p = a[key, "Pr(>F)"])
    vcs[[length(vcs) + 1]] <- cbind(experiment = id, condition = cond, direction = dir, vc[, c("grp", "var1", "var2", "sdcor")])
    message(id, " ", cond, " ", dir, " clean=", clean(m), " dropped=", paste(dropped, collapse = ","), " p=", signif(a[key, "Pr(>F)"], 3))
  }
}
write.csv(do.call(rbind, out), file.path(SP, "linked_models.csv"), row.names = FALSE)
write.csv(do.call(rbind, vcs), file.path(SP, "linked_varcomp.csv"), row.names = FALSE)
