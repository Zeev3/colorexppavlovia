# Maximal model, forward and reverse, every experiment x instruction, on the conservative data.
# For each fit: convergence/singularity, the random-effect SDs, the correlations, and the
# eigenvalues of the per-person covariance matrix (a zero eigenvalue = the estimated matrix is
# degenerate: one combination of the random effects has no variance at all).
SP <- commandArgs(trailingOnly = TRUE)[1]
suppressPackageStartupMessages({ library(lme4); library(lmerTest); library(emmeans); library(dplyr) })
emm_options(lmerTest.limit = 30000, pbkrtest.limit = 30000)
`%||%` <- function(a, b) if (is.null(a)) b else a
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
out <- list(); vc_out <- list()
for (id in c("exp1", "exp2", "exp3", "exp4")) {
  dat <- readRDS(file.path(SP, paste0(id, "_conservative.rds")))$data
  noise <- if (id == "exp4") "variance_level" else "array_length"
  for (cond in c("average", "experience")) for (dir in c("forward", "reverse")) {
    d <- dat %>% filter(condition == cond) %>% droplevels(); contrasts(d[[noise]]) <- contr.sum(nlevels(d[[noise]]))
    f <- if (dir == "forward") sprintf("indexSelected ~ meanVal_c * %s + (1 + meanVal_c * %s | participant_id)", noise, noise)
         else sprintf("meanVal ~ resp_c * %s + (0 + resp_c + resp_c:%s | participant_id)", noise, noise)
    m <- suppressWarnings(suppressMessages(lmer(as.formula(f), data = d, control = ctrl)))
    msgs <- m@optinfo$conv$lme4$messages %||% character(0)
    S <- VarCorr(m)$participant_id; ev <- eigen(S, only.values = TRUE)$values
    sds <- attr(S, "stddev"); cr <- attr(S, "correlation")
    x <- if (dir == "forward") "meanVal_c" else "resp_c"; key <- paste0(x, ":", noise)
    a <- as.data.frame(anova(m))
    tr <- as.data.frame(summary(emtrends(m, as.formula(paste("~", noise)), var = x, lmer.df = "satterthwaite"), infer = TRUE))
    out[[length(out) + 1]] <- data.frame(experiment = id, condition = cond, direction = dir,
      n_people = n_distinct(d$participant_id), n_re = length(sds), n_params = length(sds) * (length(sds) + 1) / 2,
      singular = isSingular(m, tol = 1e-4), conv_warning = length(msgs) > 0,
      smallest_sd = min(sds), smallest_sd_term = names(sds)[which.min(sds)],
      max_abs_cor = max(abs(cr[upper.tri(cr)])), smallest_eigen_share = min(ev) / sum(ev),
      slopes = paste(sprintf("%.3f", tr[[paste0(x, ".trend")]]), collapse = " / "),
      F = a[key, "F value"], df2 = a[key, "DenDF"], p = a[key, "Pr(>F)"])
    vc_out[[length(vc_out) + 1]] <- data.frame(experiment = id, condition = cond, direction = dir, term = names(sds), sd = sds)
    message(id, " ", cond, " ", dir, " done")
  }
}
write.csv(do.call(rbind, out), file.path(SP, "max_diag.csv"), row.names = FALSE)
write.csv(do.call(rbind, vc_out), file.path(SP, "max_sds.csv"), row.names = FALSE)
