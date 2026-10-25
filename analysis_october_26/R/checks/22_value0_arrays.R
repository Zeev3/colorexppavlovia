# Exp 4: arrays containing a square of value 0. The sprite code (background-position p*100%, 50 tiles, repeat)
# draws value p as tile 50 - p, so 0 is drawn as tile 0 = the most vivid shade, i.e. as value 50.
# Lists the affected trials and their corrected displayed mean (each 0 counted as 50).
SP <- commandArgs(trailingOnly = TRUE)[1]; DEST <- commandArgs(trailingOnly = TRUE)[2]
setwd("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/analysis")
suppressPackageStartupMessages({ source("R/00_setup.R"); source("R/01_experiments.R"); source("R/16_stimulus_audit.R") })
exp <- EXPERIMENTS$exp4
raw <- bind_rows(lapply(names(exp$dirs), function(cond) {
  fs <- list.files(file.path(DATA, exp$dirs[[cond]]), pattern = "PARTICIPANT.*\\.csv$", full.names = TRUE)
  bind_rows(lapply(fs, .read_raw_arrays)) %>% mutate(condition = cond) }))
vals <- lapply(raw$array_values, function(x) suppressWarnings(as.numeric(strsplit(gsub("\\[|\\]|\"", "", x), ",")[[1]])))
z <- raw %>% select(participant_id, trial, condition) %>% mutate(n_items = sapply(vals, function(v) sum(is.finite(v))),
  n_zero = sapply(vals, function(v) sum(v == 0, na.rm = TRUE)), n_50 = sapply(vals, function(v) sum(v >= 50, na.rm = TRUE)),
  shown_mean = sapply(vals, function(v) mean(v[is.finite(v)])), vals = sapply(vals, function(v) paste(v, collapse = " "))) %>%
  filter(n_items == 10) %>% distinct(participant_id, trial, condition, .keep_all = TRUE)
cat("arrays with a 0:", sum(z$n_zero > 0), " zeros per array:", paste(table(z$n_zero[z$n_zero > 0]), collapse = ","), " arrays with >= 50:", sum(z$n_50 > 0), "\n")
zz <- z %>% filter(n_zero > 0) %>% mutate(corrected_mean = shown_mean + 50 * n_zero / n_items)
d <- readRDS(file.path(SP, "exp4_conservative.rds"))$data
m <- d %>% mutate(participant_id = as.character(participant_id)) %>% inner_join(zz %>% mutate(participant_id = paste0(condition, ".", participant_id)) %>% select(participant_id, trial, n_zero, corrected_mean, vals), by = c("participant_id", "trial"))
cat("of these in the clean analysed data:", nrow(m), "\n"); print(m %>% select(participant_id, condition, variance_level, trial, meanVal, corrected_mean, indexSelected, vals), width = 200)
write.csv(m %>% select(participant_id, condition, variance_level, trial, meanVal, corrected_mean, indexSelected, n_zero, vals), file.path(DEST, "exp4_zero_arrays.csv"), row.names = FALSE)
