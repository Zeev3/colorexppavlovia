# =========================================================
# 00_setup.R -- packages, paths, constants, small helpers
# Sourced by every other script in analysis/R.
# =========================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(afex)
  library(ggeffects)
})

emm_options(lmerTest.limit = 30000, pbkrtest.limit = 30000)
options(dplyr.summarise.inform = FALSE)

# ---------------------------------------------------------
# Project root
# ---------------------------------------------------------
# Override with Sys.setenv(EPOC_ROOT = "/path/to/colorexppavlovia") if the
# project ever moves; otherwise we walk up from the working directory until
# we find the folder that holds both data/ and analysis/.
epoc_root <- function() {
  env <- Sys.getenv("EPOC_ROOT", unset = "")
  if (nzchar(env)) return(normalizePath(env))
  p <- normalizePath(getwd())
  for (i in 1:6) {
    if (dir.exists(file.path(p, "data")) && dir.exists(file.path(p, "analysis"))) return(p)
    p <- dirname(p)
  }
  stop("Could not locate the project root. Set Sys.setenv(EPOC_ROOT = '...').")
}

ROOT    <- epoc_root()
DATA    <- file.path(ROOT, "data")
# Which value counts as the objective (true) mean of a trial:
#   "recorded" (default) the meanVal the task logged
#   "shown"    the mean of the squares actually displayed, read from
#              array_values. In Experiments 1-3 some array templates do not
#              average to zero (12 squares +0.33 in Exp. 1; 10 squares +0.2 and
#              20 squares -1.5 in Exps. 2-3), so the two differ by a constant at
#              those levels. Set by `run_all.R --shown-mean`, which writes to a
#              separate output folder so the two pipelines never mix.
TRUE_VALUE <- Sys.getenv("EPOC_TRUE_VALUE", unset = "recorded")
stopifnot(TRUE_VALUE %in% c("recorded", "shown"))
# Random-effects strategy for the mixed models (see 05_lmm.R):
#   "ladder" (default) start at the registry's rand_start / rand_force rung and
#                      walk down until a fit is clean
#   "full"             always start at the maximal structure, keep a
#                      by-participant slope on the continuous predictor as the
#                      floor, and ignore rand_start / rand_force. Set by
#                      `run_all.R --full-random`.
RANDOM_MODE <- Sys.getenv("EPOC_RANDOM", unset = "ladder")
stopifnot(RANDOM_MODE %in% c("ladder", "full"))
OUT     <- file.path(ROOT, "analysis", paste0("output",
                                              if (TRUE_VALUE == "shown") "_shown_mean",
                                              if (RANDOM_MODE == "full") "_full_random"))
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
# trial-level caches parsed from the raw logs are the same for both pipelines
ARRAY_CACHE_DIR <- file.path(ROOT, "analysis", "output", "stimulus_audit")

# ---------------------------------------------------------
# Preprocessing constants (identical across all experiments,
# as described in the Results / Data Preprocessing sections)
# ---------------------------------------------------------
NO_RESPONSE   <- 0      # indexSelected the response scale starts on; recorded when the
                        # participant clicks without ever crossing the scale, so a
                        # report of exactly 0 is treated as a missing response
RT_MIN        <- 200    # ms; faster trials are dropped
RT_MAX        <- 8000   # ms; slower trials are dropped
RT_SD_TRIM    <- 2.5    # within-participant +/- SD trimming on RT
MAX_ABS_ERR   <- 30     # |reported - objective| in colour-scale units
MIN_RETENTION <- 0.85   # participant must keep >= 85% of their trials
MIN_SPEARMAN  <- 0.30   # participant-level Spearman r(objective, reported) must exceed this

# Exaggeration: a response is "exaggerated" when it is more extreme than the
# stimulus on the side the stimulus was already on.
EXAGG_LOW_MEAN  <- 20   # objective mean below this counts as a "low" trial
EXAGG_LOW_RESP  <- 14   # ... and a response below this is exaggerated
EXAGG_HIGH_MEAN <- 30   # objective mean above this counts as a "high" trial
EXAGG_HIGH_RESP <- 36   # ... and a response above this is exaggerated

# The colour scale itself
SCALE_MIN <- 1
SCALE_MAX <- 50

# ---------------------------------------------------------
# Plot theme
# ---------------------------------------------------------
theme_epoc <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(colour = "grey30", size = base_size - 1),
      strip.background = element_blank(),
      strip.text    = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

COND_COLOURS <- c(average = "#2C6E9B", experience = "#C0563B")

epoc_save <- function(plot, path, width = 7, height = 4.5, dpi = 300) {
  ggsave(path, plot, width = width, height = height, dpi = dpi, bg = "white")
  invisible(path)
}

# ---------------------------------------------------------
# Reporting helpers (APA-ish formatting)
# ---------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

fmt_p <- function(p) {
  ifelse(is.na(p), "NA",
         ifelse(p < .001, "p < .001",
                paste0("p = ", sub("^0", "", formatC(p, format = "f", digits = 3)))))
}

# coerce first: a logical NA (from cor() on a constant, say) would otherwise
# stop formatC with "unsupported type"
fmt_num <- function(x, d = 2) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = d))
}

# Greenhouse-Geisser correction leaves both df fractional; report them that way
# rather than rounding a corrected 2.91 to a clean-looking 3.
fmt_df <- function(x) fmt_num(x, if (isTRUE(all.equal(x, round(x)))) 0 else 2)

fmt_F <- function(F, df1, df2, p) {
  sprintf("F(%s, %s) = %s, %s",
          fmt_df(df1), fmt_df(df2), fmt_num(F, 2), fmt_p(p))
}

fmt_b <- function(b, se, t = NULL, df = NULL, p = NULL) {
  s <- sprintf("b = %s, SE = %s", fmt_num(b, 3), fmt_num(se, 3))
  if (!is.null(t) && !is.na(t))
    s <- paste0(s, if (is.null(df) || is.na(df)) sprintf(", t = %s", fmt_num(t, 2))
                   else sprintf(", t(%s) = %s", fmt_num(df, 2), fmt_num(t, 2)))
  if (!is.null(p)  && !is.na(p))  s <- paste0(s, ", ", fmt_p(p))
  s
}

# Append a line to a per-experiment results log
log_line <- function(con, ...) cat(..., "\n", sep = "", file = con, append = TRUE)
