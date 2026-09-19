#––––––––––––––––––––––––––––––––––––––––––––
# 0. prep
#––––––––––––––––––––––––––––––––––––––––––––
library(dplyr)
library(readr)

data_folder <- "/Users/zeevbenamos/Documents/GitHub/2facolorexp/data/pilot12fac"
setwd(data_folder)

df <- read_csv("summary_all_participants.csv", show_col_types = FALSE) %>%
  mutate(
    row_id = row_number(),
    chosenSigned = as.numeric(chosenSigned)
  )

#––––––––––––––––––––––––––––––––––––––––––––
# Stage 1: Remove participants with too few trials
# adjust this threshold if needed
#––––––––––––––––––––––––––––––––––––––––––––

min_trials <- 96

n_trials_initial <- df %>%
  group_by(participant_id) %>%
  summarise(n_total = n(), .groups = "drop")

valid_ids_initial <- n_trials_initial %>%
  filter(n_total >= min_trials) %>%
  pull(participant_id)

df0 <- df %>%
  filter(participant_id %in% valid_ids_initial)

removed_too_few <- df %>%
  filter(!participant_id %in% valid_ids_initial) %>%
  mutate(stage = "too_few_trials_initial")

#––––––––––––––––––––––––––––––––––––––––––––
# Stage 2: RT bounds
#––––––––––––––––––––––––––––––––––––––––––––

df_rt <- df0 %>%
  filter(rt >= 200, rt <= 8000)

removed_rt <- df0 %>%
  filter(!row_id %in% df_rt$row_id) %>%
  mutate(stage = "rt_bounds")

#––––––––––––––––––––––––––––––––––––––––––––
# Stage 3: participant-level ±2.5 SD on RT
#––––––––––––––––––––––––––––––––––––––––––––

df_rt_sd <- df_rt %>%
  group_by(participant_id) %>%
  mutate(
    pt_mean = mean(rt, na.rm = TRUE),
    pt_sd = sd(rt, na.rm = TRUE)
  ) %>%
  filter(
    is.na(pt_sd) |
      between(rt, pt_mean - 2.5 * pt_sd, pt_mean + 2.5 * pt_sd)
  ) %>%
  ungroup()

removed_rt_sd <- df_rt %>%
  filter(!row_id %in% df_rt_sd$row_id) %>%
  mutate(stage = "participant_rt_sd")

#––––––––––––––––––––––––––––––––––––––––––––
# Stage 4: valid 2AFC choices only
#––––––––––––––––––––––––––––––––––––––––––––

df_choice <- df_rt_sd %>%
  filter(chosenSigned %in% c(5, -5))

removed_choice <- df_rt_sd %>%
  filter(!row_id %in% df_choice$row_id) %>%
  mutate(stage = "invalid_choice")

#––––––––––––––––––––––––––––––––––––––––––––
# Stage 5: Exclude participants who retain < 85% of trials
#––––––––––––––––––––––––––––––––––––––––––––

n_trials_total <- df0 %>%
  group_by(participant_id) %>%
  summarise(n_total = n(), .groups = "drop")

n_trials_clean <- df_choice %>%
  group_by(participant_id) %>%
  summarise(n_clean = n(), .groups = "drop")

trial_counts <- left_join(n_trials_total, n_trials_clean, by = "participant_id") %>%
  mutate(
    n_clean = ifelse(is.na(n_clean), 0, n_clean),
    removal_rate = (n_total - n_clean) / n_total
  )

valid_ids_retention <- trial_counts %>%
  filter(removal_rate <= 0.15) %>%
  pull(participant_id)

df_ret <- df_choice %>%
  filter(participant_id %in% valid_ids_retention)

removed_retention <- df_choice %>%
  filter(!participant_id %in% valid_ids_retention) %>%
  mutate(stage = "too_many_trials_removed")

#––––––––––––––––––––––––––––––––––––––––––––
# Final cleaned dataset
# (without low-correlation filter)
#––––––––––––––––––––––––––––––––––––––––––––

df_final <- df_ret

removed_corr <- data.frame()

#––––––––––––––––––––––––––––––––––––––––––––
# Combine removed trials and save
#––––––––––––––––––––––––––––––––––––––––––––

removed_all <- bind_rows(
  removed_too_few,
  removed_rt,
  removed_rt_sd,
  removed_choice,
  removed_retention
)

write_csv(
  df_final %>% select(-row_id, -pt_mean, -pt_sd),
  "data_cleaned.csv"
)

write_csv(
  removed_all %>% select(-pt_mean, -pt_sd),
  "removed_trials.csv"
)

# quick checks
df_final %>%
  count(participant_id) %>%
  arrange(n)

n_participants_check <- df_final %>%
  distinct(participant_id) %>%
  nrow()

