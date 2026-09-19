# =========================================================
# 02_clean.R -- the single cleaning pipeline used by every experiment
#
# Reproduces the "Data Preprocessing" paragraph of the write-up:
#   0. drop a participant's later session when they took part in both
#      conditions (registry field `later_session`), so each person counts once
#   1. keep participants with a complete main task (>= min_trials raw trials)
#   2. drop trials with a report of 0 -- the scale's starting value, saved
#      when the participant clicks without crossing the scale (see
#      sad_sequential_task_functions.js); these are missing responses
#   3. drop trials with RT < 200 ms or RT > 8000 ms
#   4. drop trials beyond +/- 2.5 SD of the participant's own RT
#   5. drop trials where |reported - objective| >= 30 scale units
#   6. drop participants who retained < 85% of their trials
#   7. drop participants whose Spearman r(objective, reported) <= .30
#
# Missing responses count against trial retention like any other lost trial.
#
# Input : <dir>/summary_all_participants.csv  (one row per response trial,
#         produced by the data-organising scripts in funcs/)
# Output: cleaned data frame, a log of every removed trial with the stage
#         that removed it, and a participant-level exclusion table.
# =========================================================

epoc_clean <- function(summary_path, min_trials = 96, drop_ids = character(), shown = NULL) {

  stopifnot(file.exists(summary_path))

  df_all <- read.csv(summary_path, stringsAsFactors = FALSE) %>%
    mutate(row_id = seq_len(n()))

  # alternative pipeline: the objective value is the mean of the squares
  # actually displayed (see TRUE_VALUE in 00_setup.R); every trial must match
  if (!is.null(shown)) {
    df_all <- df_all %>%
      mutate(participant_id = as.character(participant_id)) %>%
      left_join(shown %>% mutate(participant_id = as.character(participant_id)) %>%
                  distinct(participant_id, trial, .keep_all = TRUE),
                by = c("participant_id", "trial"))
    if (anyNA(df_all$shown_mean))
      stop(sum(is.na(df_all$shown_mean)), " trials in ", summary_path, " have no displayed array to take the mean from")
    df_all <- df_all %>% mutate(meanVal_recorded = meanVal, meanVal = shown_mean) %>% select(-shown_mean)
  }

  n_start <- dplyr::n_distinct(df_all$participant_id)

  # -- 0. second sessions of people who already did the other condition -----
  removed_repeat <- df_all %>%
    filter(participant_id %in% drop_ids) %>%
    mutate(stage = "later_session")
  df <- df_all %>% filter(!participant_id %in% drop_ids)

  # -- 1. incomplete sessions ------------------------------------------------
  keep_complete <- df %>%
    count(participant_id, name = "n_total") %>%
    filter(n_total >= min_trials) %>%
    pull(participant_id)

  removed_incomplete <- df %>%
    filter(!participant_id %in% keep_complete) %>%
    mutate(stage = "incomplete_session")

  d0 <- df %>% filter(participant_id %in% keep_complete)

  # -- 2. missing responses (report left at the scale's starting value) -----
  d_resp <- d0 %>% filter(indexSelected != NO_RESPONSE)
  removed_noresp <- d0 %>%
    filter(!row_id %in% d_resp$row_id) %>%
    mutate(stage = "no_response")

  # -- 3. RT bounds ----------------------------------------------------------
  d_rt <- d_resp %>% filter(rt >= RT_MIN, rt <= RT_MAX)
  removed_rt <- d_resp %>%
    filter(!row_id %in% d_rt$row_id) %>%
    mutate(stage = "rt_bounds")

  # -- 4. within-participant RT trimming ------------------------------------
  d_sd <- d_rt %>%
    group_by(participant_id) %>%
    mutate(pt_mean = mean(rt, na.rm = TRUE),
           pt_sd   = sd(rt,   na.rm = TRUE)) %>%
    filter(between(rt, pt_mean - RT_SD_TRIM * pt_sd, pt_mean + RT_SD_TRIM * pt_sd)) %>%
    ungroup() %>%
    select(-pt_mean, -pt_sd)
  removed_sd <- d_rt %>%
    filter(!row_id %in% d_sd$row_id) %>%
    mutate(stage = "rt_sd_trim")

  # -- 5. implausible errors -------------------------------------------------
  d_err <- d_sd %>% filter(abs(indexSelected - meanVal) < MAX_ABS_ERR)
  removed_err <- d_sd %>%
    filter(!row_id %in% d_err$row_id) %>%
    mutate(stage = "large_error")

  # -- 6. trial retention ----------------------------------------------------
  retention <- d0 %>%
    count(participant_id, name = "n_total") %>%
    left_join(count(d_err, participant_id, name = "n_clean"), by = "participant_id") %>%
    mutate(n_clean  = coalesce(n_clean, 0L),
           retained = n_clean / n_total)

  keep_retained <- retention %>% filter(retained >= MIN_RETENTION) %>% pull(participant_id)

  removed_retention <- d_err %>%
    filter(!participant_id %in% keep_retained) %>%
    mutate(stage = "low_retention")

  d_ret <- d_err %>% filter(participant_id %in% keep_retained)

  # -- 7. participants who do not track the stimulus -------------------------
  spearman <- d_ret %>%
    group_by(participant_id) %>%
    summarise(rho = suppressWarnings(
      cor(meanVal, indexSelected, use = "complete.obs", method = "spearman")))

  keep_tracking <- spearman %>% filter(rho > MIN_SPEARMAN) %>% pull(participant_id)

  removed_corr <- d_ret %>%
    filter(!participant_id %in% keep_tracking) %>%
    mutate(stage = "low_tracking")

  d_final <- d_ret %>% filter(participant_id %in% keep_tracking)

  removed <- bind_rows(removed_repeat, removed_incomplete, removed_noresp, removed_rt, removed_sd,
                       removed_err, removed_retention, removed_corr)

  # participant-level bookkeeping, so exclusion counts can be reported directly
  participants <- retention %>%
    left_join(spearman, by = "participant_id") %>%
    mutate(excluded_reason = case_when(
      !participant_id %in% keep_retained ~ "low_retention",
      !participant_id %in% keep_tracking ~ "low_tracking",
      TRUE                               ~ NA_character_
    )) %>%
    bind_rows(
      df %>%
        filter(!participant_id %in% keep_complete) %>%
        count(participant_id, name = "n_total") %>%
        mutate(n_clean = NA_integer_, retained = NA_real_, rho = NA_real_,
               excluded_reason = "incomplete_session"),
      removed_repeat %>%
        count(participant_id, name = "n_total") %>%
        mutate(n_clean = NA_integer_, retained = NA_real_, rho = NA_real_,
               excluded_reason = "later_session")
    ) %>%
    arrange(!is.na(excluded_reason), participant_id)

  exclusions <- tibble::tibble(
    stage = c("started", "later_session", "incomplete_session", "low_retention", "low_tracking", "analysed"),
    n_participants = c(
      n_start,
      n_start - dplyr::n_distinct(df$participant_id),
      dplyr::n_distinct(df$participant_id) - length(keep_complete),
      length(keep_complete) - length(keep_retained),
      length(keep_retained) - length(keep_tracking),
      length(keep_tracking)
    )
  )

  list(
    data         = d_final %>% select(-row_id),
    removed      = removed %>% select(-row_id),
    participants = participants,
    exclusions   = exclusions,
    trial_loss   = tibble::tibble(
      stage = c("no_response", "rt_bounds", "rt_sd_trim", "large_error"),
      n_trials = c(nrow(removed_noresp), nrow(removed_rt), nrow(removed_sd), nrow(removed_err))
    )
  )
}

# Clean one condition of one experiment and (optionally) rewrite the
# data_cleaned.csv / removed_trials.csv files next to the raw data, exactly
# where the older scripts expect to find them.
epoc_clean_condition <- function(exp, condition, write = FALSE) {
  dir  <- file.path(DATA, exp$dirs[[condition]])
  drop <- names(exp$later_session)[exp$later_session == condition]
  shown <- if (TRUE_VALUE == "shown")
    expl_array_trials(exp) %>% filter(condition == !!condition) %>% select(participant_id, trial, shown_mean)
  res  <- epoc_clean(file.path(dir, "summary_all_participants.csv"), exp$min_trials,
                     drop_ids = if (length(drop)) drop else character(), shown = shown)
  if (write) {
    write.csv(res$data,    file.path(dir, "data_cleaned.csv"),   row.names = FALSE)
    write.csv(res$removed, file.path(dir, "removed_trials.csv"), row.names = FALSE)
  }
  res
}
