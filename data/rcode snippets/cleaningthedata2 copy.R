#––––––––––––––––––––––––––––––––––––––––––––
# 0. prep
#––––––––––––––––––––––––––––––––––––––––––––
library(dplyr)

# 1. read & tag rows
setwd("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/colorsquersexpgreaternoise")
df <- read.csv("summary_all_participants.csv", stringsAsFactors = FALSE) %>%
  mutate(row_id = seq_len(nrow(.)))

#––––––––––––––––––––––––––––––––––––––––––––
# Stage 1: RT bounds (under 500 ms or over 8000 ms)
#––––––––––––––––––––––––––––––––––––––––––––
df_rb      <- df %>% filter(rt >= 200, rt <= 8000)
removed_rb <- df %>% 
  filter(!row_id %in% df_rb$row_id) %>% 
  mutate(stage = "rt_bounds")

#––––––––––––––––––––––––––––––––––––––––––––
# Stage 2: participant-level ±2.5 SD on rt
#––––––––––––––––––––––––––––––––––––––––––––
df_p_sd    <- df_rb %>%
  group_by(participant_id) %>%
  mutate(
    pt_mean = mean(rt, na.rm = TRUE),
    pt_sd   = sd(  rt, na.rm = TRUE)
  ) %>%
  filter(between(rt, pt_mean - 3.0 * pt_sd, pt_mean + 3.0 * pt_sd)) %>%
  ungroup()
removed_p_sd <- df_rb %>%
  filter(!row_id %in% df_p_sd$row_id) %>%
  mutate(stage = "participant_sd")

#––––––––––––––––––––––––––––––––––––––––––––
# Stage 2.5: Remove participants with >15% of their original trials removed during participant SD filtering
#––––––––––––––––––––––––––––––––––––––––––––

# Total trials before any filtering
n_trials_total <- df %>%
  group_by(participant_id) %>%
  summarize(n_total = n())

# Trials retained after participant SD filter
n_trials_p_sd <- df_p_sd %>%
  group_by(participant_id) %>%
  summarize(n_after_p_sd = n())

# Merge and calculate proportion removed
trial_counts <- left_join(n_trials_total, n_trials_p_sd, by = "participant_id") %>%
  mutate(
    n_after_p_sd = ifelse(is.na(n_after_p_sd), 0, n_after_p_sd),
    n_removed = n_total - n_after_p_sd,
    removal_rate = n_removed / n_total
  )

# Keep only participants with ≤15% of trials removed
valid_ids <- trial_counts %>%
  filter(removal_rate <= 0.15) %>%
  pull(participant_id)

# Filter df_p_sd to keep only valid participants
df_p_sd <- df_p_sd %>% filter(participant_id %in% valid_ids)

# Save removed participants (those with >15% trials removed)
removed_over15 <- df_rb %>%
  filter(!participant_id %in% valid_ids) %>%
  mutate(stage = "too_many_trials_removed")
#––––––––––––––––––––––––––––––––––––––––––––
# Stage 4: low-corr filter last
#   drop any participant whose corr(meanVal,indexSelected) < 0.3
#––––––––––––––––––––––––––––––––––––––––––––
good_ids <- df_g_sd %>%
  group_by(participant_id) %>%
  summarize(c = cor(meanVal, indexSelected, use = "complete.obs")) %>%
  filter(c >= 0.3) %>%
  pull(participant_id)

df_final     <- df_g_sd %>% filter(participant_id %in% good_ids)
removed_corr <- df_g_sd %>% 
  filter(!participant_id %in% good_ids) %>% 
  mutate(stage = "low_corr")



#––––––––––––––––––––––––––––––––––––––––––––
# Stage 3: Remove participants with mean RT outside global ±2.5 SD
#––––––––––––––––––––––––––––––––––––––––––––

# Calculate mean RT per participant
participant_means <- df_p_sd %>%
  group_by(participant_id) %>%
  summarize(mean_rt = mean(rt, na.rm = TRUE))

# Calculate global mean and SD of participant means
g_mean <- mean(participant_means$mean_rt, na.rm = TRUE)
g_sd   <- sd(  participant_means$mean_rt, na.rm = TRUE)

# Identify participants within ±2.5 SD
valid_ids <- participant_means %>%
  filter(between(mean_rt, g_mean - 3 * g_sd, g_mean + 3 * g_sd)) %>%
  pull(participant_id)

# Keep only valid participants
df_g_sd <- df_p_sd %>% filter(participant_id %in% valid_ids)

# Mark removed participants' trials
removed_g_sd <- df_p_sd %>%
  filter(!participant_id %in% valid_ids) %>%
  mutate(stage = "global_sd")
#––––––––––––––––––––––––––––––––––––––––––––
# Stage 4: low-corr filter last
#   drop any participant whose corr(meanVal,indexSelected) < 0.3
#––––––––––––––––––––––––––––––––––––––––––––
good_ids <- df_g_sd %>%
  group_by(participant_id) %>%
  summarize(c = cor(meanVal, indexSelected, use = "complete.obs")) %>%
  filter(c >= 0.3) %>%
  pull(participant_id)

df_final     <- df_g_sd %>% filter(participant_id %in% good_ids)
removed_corr <- df_g_sd %>% 
  filter(!participant_id %in% good_ids) %>% 
  mutate(stage = "low_corr")

#––––––––––––––––––––––––––––––––––––––––––––
# combine all removed trials & save
#––––––––––––––––––––––––––––––––––––––––––––
removed_all <- bind_rows(removed_rb, removed_p_sd, removed_over15, removed_g_sd, removed_corr)

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
