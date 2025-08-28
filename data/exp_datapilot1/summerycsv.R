library(tidyverse)

# Load data
setwd("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/exp_data")
df <- read_csv("data_cleaned.csv")  # adjust path if needed

# Compute absolute error
df <- df %>%
  mutate(abs_error = abs(meanVal - indexSelected))

# Overall avg and median RT per participant
overall_rt <- df %>%
  group_by(participant_id) %>%
  summarise(
    avg_rt = mean(rt, na.rm = TRUE),
    median_rt = median(rt, na.rm = TRUE),
    overall_corr = cor(meanVal, indexSelected, use = "complete.obs"),
    .groups = "drop"
  )

# Correlation per array_length per participant, reshaped to wide
corrs_by_length <- df %>%
  group_by(participant_id, array_length) %>%
  summarise(corr = cor(meanVal, indexSelected, use = "complete.obs"), .groups = "drop") %>%
  pivot_wider(names_from = array_length, values_from = corr, names_prefix = "corr_")

# Join into final wide-format summary table
final_summary <- left_join(overall_rt, corrs_by_length, by = "participant_id")

# Save to CSV
write_csv(final_summary, "participant_summary_wide.csv")
