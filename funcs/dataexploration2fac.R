# =========================================================
# 2AFC Choice Analysis + Psychometric Plot
# =========================================================

# Libraries
library(dplyr)
library(ggplot2)
library(readr)

# =========================================================
# Read data
# =========================================================

data_folder <- "/Users/zeevbenamos/Documents/GitHub/2facolorexp/data/pilot12fac"

all_data <- read_csv(
  file.path(data_folder, "data_cleaned.csv"),
  show_col_types = FALSE
)

# =========================================================
# Create summary table
# =========================================================

choice_by_mean <- all_data %>%
  group_by(meanVal, array_length) %>%
  summarise(
    total_trials = n(),
    
    n_choose_5 = sum(chosenSigned == 5, na.rm = TRUE),
    n_choose_minus5 = sum(chosenSigned == -5, na.rm = TRUE),
    
    prop_choose_5 = n_choose_5 / total_trials,
    prop_choose_minus5 = n_choose_minus5 / total_trials,
    
    .groups = "drop"
  )

# =========================================================
# Save summary table
# =========================================================

write_csv(
  choice_by_mean,
  file.path(data_folder, "choice_proportions_by_meanVal.csv")
)

# =========================================================
# Psychometric plot
# =========================================================

ggplot(
  choice_by_mean,
  aes(
    x = meanVal,
    y = prop_choose_5,
    color = factor(array_length)
  )
) +
  
  geom_point(size = 2) +
  
  geom_line(linewidth = 1) +
  
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE,
    linewidth = 1
  ) +
  
  labs(
    x = "Objective mean value",
    y = "Proportion choosing +5",
    color = "Array length",
    title = "Psychometric choice curve"
  ) +
  
  ylim(0, 1) +
  
  theme_classic(base_size = 14)

# Create binary choice variable
all_data <- all_data %>%
  mutate(
    choose_5 = ifelse(chosenSigned == 5, 1, 0)
  )

# Summary for points
choice_by_mean <- all_data %>%
  group_by(meanVal, array_length) %>%
  summarise(
    total_trials = n(),
    n_choose_5 = sum(choose_5, na.rm = TRUE),
    n_choose_minus5 = sum(chosenSigned == -5, na.rm = TRUE),
    prop_choose_5 = n_choose_5 / total_trials,
    prop_choose_minus5 = n_choose_minus5 / total_trials,
    .groups = "drop"
  )

# Plot
ggplot() +
  
  # raw proportion points
  geom_point(
    data = choice_by_mean,
    aes(
      x = meanVal,
      y = prop_choose_5,
      color = factor(array_length)
    ),
    size = 2
  ) +
  
  # logistic smooth from raw binary data
  geom_smooth(
    data = all_data,
    aes(
      x = meanVal,
      y = choose_5,
      color = factor(array_length)
    ),
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE,
    linewidth = 1
  ) +
  
  labs(
    x = "Objective mean value",
    y = "Proportion choosing +5",
    color = "Array length",
    title = "Psychometric choice curve"
  ) +
  
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 14)

# =========================================================
# Mean RT by meanVal
# =========================================================

rt_summary <- all_data %>%
  group_by(meanVal) %>%
  summarise(
    mean_rt = mean(rt, na.rm = TRUE),
    se_rt = sd(rt, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# =========================================================
# Plot
# =========================================================

ggplot(rt_summary, aes(x = meanVal, y = mean_rt)) +
  
  geom_line(linewidth = 1) +
  
  geom_point(size = 2) +
  
  geom_errorbar(
    aes(
      ymin = mean_rt - se_rt,
      ymax = mean_rt + se_rt
    ),
    width = 0.3
  ) +
  
  labs(
    title = "Reaction Time Across Objective Mean Values",
    x = "Objective mean value",
    y = "Mean RT (ms)"
  ) +
  
  theme_classic(base_size = 14)
# =========================================================
# Mean RT by meanVal, split by choice
# =========================================================

rt_summary_choice <- all_data %>%
  filter(chosenSigned %in% c(5, -5)) %>%
  mutate(
    choice_label = case_when(
      chosenSigned == 5  ~ "+5",
      chosenSigned == -5 ~ "-5"
    )
  ) %>%
  group_by(meanVal, choice_label) %>%
  summarise(
    mean_rt = mean(rt, na.rm = TRUE),
    se_rt = sd(rt, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# =========================================================
# Plot
# =========================================================

ggplot(
  rt_summary_choice,
  aes(
    x = meanVal,
    y = mean_rt,
    color = choice_label,
    group = choice_label
  )
) +
  
  geom_line(linewidth = 1) +
  
  geom_point(size = 2) +
  
  geom_errorbar(
    aes(
      ymin = mean_rt - se_rt,
      ymax = mean_rt + se_rt
    ),
    width = 0.3
  ) +
  
  scale_color_manual(
    values = c(
      "+5" = "red",
      "-5" = "blue"
    )
  ) +
  
  labs(
    title = "Reaction Time Across Objective Mean Values by Choice",
    x = "Objective mean value",
    y = "Mean RT (ms)",
    color = "Choice"
  ) +
  
  theme_classic(base_size = 14)

# =========================================================
# Percentage of exaggerated answers for each participant
# =========================================================

participant_exaggeration <- all_data %>%
  group_by(participant_id) %>%
  summarise(
    n_trials = n(),
    percent_exaggerated = mean(exaggerated_choice, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(percent_exaggerated) %>%
  mutate(
    participant_order = factor(
      participant_id,
      levels = participant_id
    )
  )

# =========================================================
# Plot
# =========================================================

ggplot(
  participant_exaggeration,
  aes(
    x = participant_order,
    y = percent_exaggerated
  )
) +
  
  geom_hline(
    yintercept = 50,
    linetype = "dashed"
  ) +
  
  geom_col(width = 0.8) +
  
  geom_text(
    aes(
      label = round(percent_exaggerated, 1)
    ),
    vjust = -0.4,
    size = 3
  ) +
  
  labs(
    title = "Percentage of Exaggerated Responses by Participant",
    x = "Participant",
    y = "Exaggerated responses (%)"
  ) +
  
  coord_cartesian(ylim = c(0, 100)) +
  
  theme_classic(base_size = 14) +
  
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# =========================================================
# Table of participant exaggeration percentages
# =========================================================

participant_exaggeration <- all_data %>%
  group_by(participant_id) %>%
  summarise(
    n_trials = n(),
    exaggerated_percent = round(
      mean(exaggerated_choice, na.rm = TRUE) * 100,
      2
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(exaggerated_percent))

participant_exaggeration


# =========================================================
# Keep only first 15 trials per participant
# =========================================================

first15_data <- all_data %>%
  arrange(participant_id, trial) %>%
  group_by(participant_id) %>%
  slice_head(n = 25) %>%
  ungroup()

# Proportion exaggerated by extremity — first 15 trials only
extremity_choice_summary_15 <- first15_data %>%
  group_by(extremity) %>%
  summarise(
    n = n(),
    prop_exaggerated = mean(exaggerated_choice, na.rm = TRUE),
    se = sqrt((prop_exaggerated * (1 - prop_exaggerated)) / n),
    .groups = "drop"
  )

ggplot(extremity_choice_summary_15, aes(x = extremity, y = prop_exaggerated)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = prop_exaggerated - se, ymax = prop_exaggerated + se), width = 0.3) +
  labs(
    title = "Proportion of Exaggerated Choices by Extremity — First 15 Trials",
    x = "Extremity from midpoint |meanVal - 25|",
    y = "Proportion exaggerated choice"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 14)

# RT by extremity and choice type — first 15 trials only
extremity_rt_summary_15 <- first15_data %>%
  group_by(extremity, choice_type) %>%
  summarise(
    n = n(),
    mean_rt = mean(rt, na.rm = TRUE),
    se_rt = sd(rt, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

ggplot(
  extremity_rt_summary_15,
  aes(x = extremity, y = mean_rt, color = choice_type, group = choice_type)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt), width = 0.3) +
  scale_color_manual(values = c("Exaggerated" = "red", "Regressed" = "blue")) +
  labs(
    title = "Mean RT by Extremity and Choice Type — First 15 Trials",
    x = "Extremity from midpoint |meanVal - 25|",
    y = "Mean RT (ms)",
    color = "Choice type"
  ) +
  theme_classic(base_size = 14)

