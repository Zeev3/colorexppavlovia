# Conservative re-analysis for the meeting deck:
#   - true colour = mean of the squares actually shown (pipeline's "shown" mode)
#   - report = recorded index + 1 (the response scale showed image i + 1 for slice i)
#   - random effects: closest-to-maximal structure that converges cleanly
# Uses the pipeline's own functions; writes nothing into the pipeline's output folders.
args <- commandArgs(trailingOnly = TRUE); ids <- strsplit(args[1], ",")[[1]]; out_dir <- args[2]
Sys.setenv(EPOC_ROOT = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia", EPOC_TRUE_VALUE = "shown", EPOC_RANDOM = "full")
R_DIR <- file.path(Sys.getenv("EPOC_ROOT"), "analysis", "R")
for (f in sort(list.files(R_DIR, pattern = "\\.R$", full.names = TRUE))) source(f)
suppressPackageStartupMessages({ library(lme4); library(lmerTest); library(emmeans); library(dplyr) })
emm_options(lmerTest.limit = 30000, pbkrtest.limit = 30000)
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# ---- the +1 correction, applied to the raw summary file before cleaning --------
.epoc_clean_orig <- epoc_clean
epoc_clean <- function(summary_path, ...) {
  d <- read.csv(summary_path, stringsAsFactors = FALSE)
  keep <- !is.na(d$indexSelected) & d$indexSelected != NO_RESPONSE   # 0 = slider never moved: still dropped
  d$indexSelected[keep] <- d$indexSelected[keep] + 1
  tmp <- tempfile(fileext = ".csv"); write.csv(d, tmp, row.names = FALSE)
  .epoc_clean_orig(tmp, ...)
}

fit_ladder <- function(d, noise, direction) {
  lv <- levels(d[[noise]]); x <- if (direction == "forward") "meanVal_c" else "resp_c"
  y <- if (direction == "forward") "indexSelected" else "meanVal"
  per_level <- paste(sprintf("(0 + I(%s * (%s == \"%s\")) | participant_id)", x, noise, lv), collapse = " + ")
  specs <- if (direction == "forward") list(
    max             = sprintf("(1 + %s * %s | participant_id)", x, noise),
    by_level_corr   = sprintf("(1 | participant_id) + (0 + %s:%s | participant_id)", x, noise),
    shared_plus_dev = sprintf("(1 + %s | participant_id) + %s", x, per_level),
    by_level_nocorr = paste("(1 | participant_id) +", per_level),
    slope_only      = sprintf("(1 + %s | participant_id)", x))
  else list(   # reverse: no by-person intercept (every person saw the same true colours; that variance is ~0 by design)
    max             = sprintf("(0 + %s + %s:%s | participant_id)", x, x, noise),
    by_level_corr   = sprintf("(0 + %s:%s | participant_id)", x, noise),
    shared_plus_dev = sprintf("(0 + %s | participant_id) + %s", x, per_level),
    by_level_nocorr = per_level,
    slope_only      = sprintf("(0 + %s | participant_id)", x))
  fits <- list(); chosen <- NA
  for (nm in names(specs)) {
    f <- as.formula(sprintf("%s ~ %s * %s + %s", y, x, noise, specs[[nm]]))
    m <- tryCatch(suppressWarnings(suppressMessages(lmer(f, data = d, control = ctrl))), error = function(e) NULL)
    if (is.null(m)) next
    clean <- length(m@optinfo$conv$lme4$messages %||% character(0)) == 0 && !isSingular(m, tol = 1e-4)
    fits[[nm]] <- list(model = m, clean = clean, formula = deparse1(f))
    if (clean && is.na(chosen)) chosen <- nm
  }
  if (is.na(chosen)) chosen <- "slope_only"
  summ <- lapply(names(fits), function(nm) {
    m <- fits[[nm]]$model; a <- as.data.frame(anova(m)); key <- paste0(x, ":", noise)
    tr <- as.data.frame(summary(emtrends(m, as.formula(paste("~", noise)), var = x, lmer.df = "satterthwaite"), infer = TRUE))
    data.frame(structure = nm, clean = fits[[nm]]$clean, chosen = nm == chosen, formula = fits[[nm]]$formula,
               level = tr[[noise]], slope = tr[[paste0(x, ".trend")]], ci_low = tr$lower.CL, ci_high = tr$upper.CL,
               F = a[key, "F value"], df1 = a[key, "NumDF"], df2 = a[key, "DenDF"], p = a[key, "Pr(>F)"])
  })
  list(fits = fits, chosen = chosen, table = do.call(rbind, summ))
}

for (id in ids) {
  exp <- EXPERIMENTS[[id]]; noise <- exp$noise_var
  message("=== ", id)
  prep <- epoc_prepare(exp)
  dat <- add_resp_c(prep$data)
  res <- list(exclusions = prep$exclusions, participants = prep$participants, trial_loss = prep$trial_loss, data = dat)
  res$corr <- epoc_correlations(dat, exp)
  res$anovas <- epoc_anovas(dat, exp)
  res$tails <- epoc_tails(dat, exp)
  res$exg <- epoc_exaggeration(dat, exp)
  mods <- list()
  for (cond in c("average", "experience")) {
    d <- dat %>% filter(condition == cond) %>% droplevels()
    contrasts(d[[noise]]) <- contr.sum(nlevels(d[[noise]]))
    for (dir in c("forward", "reverse")) {
      fl <- fit_ladder(d, noise, dir)
      mods[[paste(cond, dir)]] <- cbind(experiment = id, condition = cond, direction = dir, fl$table)
      message("  ", cond, " ", dir, ": ", fl$chosen)
    }
  }
  res$models <- do.call(rbind, mods)
  rdir <- file.path(out_dir, paste0(id, "_r2t")); dir.create(rdir, showWarnings = FALSE)
  res$r2t <- tryCatch(epoc_report_to_true(dat, exp, rdir)$tests, error = function(e) { message("r2t failed: ", conditionMessage(e)); NULL })
  saveRDS(res, file.path(out_dir, paste0(id, "_conservative.rds")))
  write.csv(res$models, file.path(out_dir, paste0(id, "_models.csv")), row.names = FALSE)
}
