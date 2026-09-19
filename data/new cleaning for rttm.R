#––––––––––––––––––––––––––––––––––––––––––––
# 0. prep
#––––––––––––––––––––––––––––––––––––––––––––
library(dplyr)

setwd("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/greaternoise/colorblocksavggraternoiselevels")

# 1. read & tag rows
df <- read.csv("summary_all_participants.csv", stringsAsFactors = FALSE) %>%
  mutate(row_id = seq_len(nrow(.)))

#––––––––––––––––––––––––––––––––––––––––––––
# Stage 2: Remove participants with < 200 trials in the raw data
#––––––––––––––––––––––––––––––––––––––––––––
n_trials_initial <- df %>%
  group_by(participant_id) %>%
  summarize(n_total = n(), .groups = "drop")

valid_ids_initial <- n_trials_initial %>%
  filter(n_total >= 96) %>%
  pull(participant_id)

# Keep only participants with at least 96 trials
df0 <- df %>%
  filter(participant_id %in% valid_ids_initial)

# Mark removed participants' trials
removed_too_few <- df %>%
  filter(!participant_id %in% valid_ids_initial) %>%
  mutate(stage = "too_few_trials_initial")

#––––––––––––––––––––––––––––––––––––––––––––
# Stage 3: RT bounds (RT < 200 ms or RT > 8000 ms)
#––––––––––––––––––––––––––––––––––––––––––––
df_rt <- df0 %>% 
  filter(rt >= 200, rt <= 8000)

removed_rt <- df0 %>% 
  filter(!row_id %in% df_rt$row_id) %>% 
  mutate(stage = "rt_bounds")   # criterion 3

#––––––––––––––––––––––––––––––––––––––––––––
# Stage 4: participant-level ±2.5 SD on RT
#––––––––––––––––––––––––––––––––––––––––––––
df_rt_sd <- df_rt %>%
  group_by(participant_id) %>%
  mutate(
    pt_mean = mean(rt, na.rm = TRUE),
    pt_sd   = sd(  rt, na.rm = TRUE)
  ) %>%
  filter(between(rt, pt_mean - 2.5 * pt_sd, pt_mean + 2.5 * pt_sd)) %>%
  ungroup()

removed_rt_sd <- df_rt %>%
  filter(!row_id %in% df_rt_sd$row_id) %>%
  mutate(stage = "participant_rt_sd")  # criterion 4

#––––––––––––––––––––––––––––––––––––––––––––
# Stage 5: Exclude trials where |indexSelected - meanVal| ≥ 30
#––––––––––––––––––––––––––––––––––––––––––––
df_err <- df_rt_sd %>%
  filter(abs(indexSelected - meanVal) < 30)

removed_err <- df_rt_sd %>%
  filter(!row_id %in% df_err$row_id) %>%
  mutate(stage = "large_error")  # criterion 5

#––––––––––––––––––––––––––––––––––––––––––––
# Stage 6: Exclude participants who retain < 85% of trials after cleaning
#         (relative to their original number of trials, among those with ≥96)
#––––––––––––––––––––––––––––––––––––––––––––

# Total trials before any cleaning (per participant), after initial ≥96 filter
n_trials_total <- df0 %>%
  group_by(participant_id) %>%
  summarize(n_total = n(), .groups = "drop")

# Trials retained after trial-level cleaning (RT + SD + error)
n_trials_clean <- df_err %>%
  group_by(participant_id) %>%
  summarize(n_clean = n(), .groups = "drop")

# Merge and calculate removal rate
trial_counts <- left_join(n_trials_total, n_trials_clean, by = "participant_id") %>%
  mutate(
    n_clean      = ifelse(is.na(n_clean), 0, n_clean),
    removal_rate = (n_total - n_clean) / n_total
  )

# Keep only participants with ≤15% of trials removed
valid_ids_retention <- trial_counts %>%
  filter(removal_rate <= 0.15) %>%
  pull(participant_id)

# Data after retention filter
df_ret <- df_err %>%
  filter(participant_id %in% valid_ids_retention)

removed_retention <- df_err %>%
  filter(!participant_id %in% valid_ids_retention) %>%
  mutate(stage = "too_many_trials_removed")   # criterion 6

#––––––––––––––––––––––––––––––––––––––––––––
# Stage 7: low-corr filter (Spearman)
#   drop any participant whose cor(meanVal, indexSelected) ≤ 0.3
#   also drop participants with fewer than 2 usable trials
#––––––––––––––––––––––––––––––––––––––––––––

corr_check <- df_ret %>%
  group_by(participant_id) %>%
  summarize(
    n_complete = sum(complete.cases(meanVal, indexSelected)),
    c = ifelse(
      n_complete >= 2,
      cor(meanVal, indexSelected, use = "complete.obs", method = "spearman"),
      NA_real_
    ),
    .groups = "drop"
  )

good_ids <- corr_check %>%
  filter(!is.na(c), c > 0.3) %>%
  pull(participant_id)

df_final <- df_ret %>%
  filter(participant_id %in% good_ids)

removed_corr <- df_ret %>% 
  filter(!participant_id %in% good_ids) %>% 
  mutate(stage = "low_corr")
#––––––––––––––––––––––––––––––––––––––––––––
# Combine all removed trials & save
#––––––––––––––––––––––––––––––––––––––––––––
removed_all <- bind_rows(
  removed_too_few,
  removed_rt,
  removed_rt_sd,
  removed_err,
  removed_retention,
  removed_corr
)

# final cleaned data
write.csv(
  df_final %>% select(-row_id, -pt_mean, -pt_sd),
  "data_cleaned.csv",
  row.names = FALSE
)

# who got removed & why
write.csv(
  removed_all %>% select(-pt_mean, -pt_sd),
  "removed_trials.csv",
  row.names = FALSE
)

# quick check: how many trials per participant after all filters?
df_final %>%
  count(participant_id) %>%
  arrange(n)

 n_participants_check <- df_final %>%
    distinct(participant_id) %>%
    nrow()
