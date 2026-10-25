# =====================================================================================
# Linked-slopes model vs the maximal ("full") model, side by side.
# Open in RStudio, set the three choices below, and run the whole file (Cmd+Shift+Enter).
#
# Data: the conservative clean data (true colour = displayed average, reports + 1, all
# exclusions applied): output/data/<exp>_clean_conservative.csv (made by run_all.R, steps 01-03).
#   meanVal       = true colour (average of the squares shown)
#   indexSelected = the report (slider position + 1)
#   noise_level   = SD 3/5/7 (Exp 4) or number of squares (Exps 1-3)
# =====================================================================================

EXPERIMENT <- "exp4"        # "exp1", "exp2", "exp3", "exp4"
CONDITION  <- "experience"  # "average" or "experience" (models are fitted per instruction)
DIRECTION  <- "forward"     # "forward" (report ~ true) or "reverse" (true ~ report)

library(lme4); library(lmerTest); library(emmeans); library(dplyr)
emm_options(lmerTest.limit = 30000, pbkrtest.limit = 30000)
ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# ---- data ----------------------------------------------------------------------------
here <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) dirname(rstudioapi::getSourceEditorContext()$path) else getwd()
d <- read.csv(file.path(here, "..", "..", "output", "data", paste0(EXPERIMENT, "_clean_conservative.csv"))) %>%
  filter(condition == CONDITION) %>%
  mutate(level = factor(noise_level)) %>%
  group_by(participant_id) %>%                        # centre within each person
  mutate(true_c = meanVal - mean(meanVal), resp_c = indexSelected - mean(indexSelected)) %>%
  ungroup()
contrasts(d$level) <- contr.sum(nlevels(d$level))     # sum coding: the main slope = average over levels
cat(n_distinct(d$participant_id), "participants,", nrow(d), "trials, levels:", levels(d$level), "\n\n")

if (DIRECTION == "forward") { y <- "indexSelected"; x <- "true_c" } else { y <- "meanVal"; x <- "resp_c" }

# ---- 1. maximal model -------------------------------------------------------------------
# Every per-person effect: intercept, overall slope, and a per-level change in both, all
# freely correlated.  (Reverse: no per-person intercept, because everyone saw the same range
# of true colours, so that variance is zero by design.)
f_max <- if (DIRECTION == "forward") sprintf("%s ~ %s * level + (1 + %s * level | participant_id)", y, x, x) else
                                     sprintf("%s ~ %s * level + (0 + %s + %s:level | participant_id)", y, x, x, x)
m_max <- lmer(as.formula(f_max), data = d, control = ctrl)   # watch the console for warnings

# ---- 2. linked-slopes model ----------------------------------------------------------------
# Each person: own intercept + own overall slope (correlated), plus an own deviation at each level.
# I(x * (level == "7")) is a variable equal to x on SD-7 trials and 0 elsewhere, so its random
# slope is each person's EXTRA slope at that level. "0 +" = no extra intercept; separate brackets
# = the deviations are uncorrelated with each other. A person's slope at a level = overall + deviation,
# so their slopes at different levels are linked through the shared overall slope.
base <- if (DIRECTION == "forward") sprintf("(1 + %s | participant_id)", x) else sprintf("(0 + %s | participant_id)", x)
devs <- sprintf("(0 + I(%s * (level == \"%s\")) | participant_id)", x, levels(d$level))
f_link <- paste(sprintf("%s ~ %s * level +", y, x), paste(c(base, devs), collapse = " + "))
m_link <- lmer(as.formula(f_link), data = d, control = ctrl)
# Deviations estimated at exactly 0 add nothing; drop them and refit (identical fit, no boundary warning).
vc <- as.data.frame(VarCorr(m_link)); zero_terms <- vc$var1[grepl("^I\\(", vc$var1) & is.na(vc$var2) & vc$sdcor < 1e-4]
if (length(zero_terms)) {
  cat("Dropped (variance exactly 0):", zero_terms, "\n")
  keep <- devs[!sapply(devs, function(t) any(sapply(zero_terms, function(z) grepl(z, t, fixed = TRUE))))]
  f_link <- paste(sprintf("%s ~ %s * level +", y, x), paste(c(base, keep), collapse = " + "))
  m_link <- lmer(as.formula(f_link), data = d, control = ctrl)
}

# ---- 3. compare ------------------------------------------------------------------------------------
report <- function(m, name, f) {
  msgs <- m@optinfo$conv$lme4$messages
  cat("====", name, "====\n", f, "\n")
  cat("Singular fit:", isSingular(m, tol = 1e-4), " | Convergence warning:", length(msgs) > 0, "\n")
  if (length(msgs)) cat("  ", msgs, sep = "\n  ")
  cat("\nRandom effects (SDs and correlations):\n"); print(VarCorr(m), comp = "Std.Dev.")
  cat("\nSlope at each level (95% CI):\n")
  print(summary(emtrends(m, ~ level, var = x, lmer.df = "satterthwaite"), infer = TRUE))
  cat("\nDoes the slope change with noise? (", x, " x level, Satterthwaite F)\n"); print(anova(m)[paste0(x, ":level"), ])
  cat("\n")
}
report(m_max,  "MAXIMAL", f_max)
report(m_link, "LINKED",  f_link)

# ---- 4. where does the maximal model's extra fit come from? ------------------------------------
# The maximal model adds two kinds of things to the linked one: (a) a per-person shift of the
# intercept at each level (each person's bias may differ between levels), and (b) free correlations
# among the slope terms. Model 2b = linked + (a) only: (1 | participant_id:level) gives every person
# their own intercept at each level. Comparing 2b with the maximal model isolates (b).
f_link_b <- paste(f_link, "+ (1 | participant_id:level)")
m_link_b <- lmer(as.formula(f_link_b), data = d, control = ctrl)
report(m_link_b, "LINKED + per-person level shifts (2b)", f_link_b)

cat("==== Likelihood-ratio tests (ML refit; maximal model is singular, so treat p-values as rough) ====\n")
cat("\nLinked vs linked + level shifts: do people's biases differ between levels?\n"); print(anova(m_link, m_link_b))
cat("\nLinked + level shifts vs maximal: does freeing the slope correlations add anything?\n"); print(anova(m_link_b, m_max))
cat("\nPer-person variance/correlation parameters: maximal", length(getME(m_max, "theta")),
    "| linked + shifts", length(getME(m_link_b, "theta")), "| linked", length(getME(m_link, "theta")), "\n")
