# =========================================================
# CLEANED SCRIPT: GALTON + BUCKETED SCATTER PLOTS
# ANALYSIS BY ARRAY LENGTH
# =========================================================

# =========================================================
# 0. PACKAGES
# =========================================================
library(dplyr)
library(ggplot2)
library(tidyr)

# =========================================================
# 1. LOAD AND PREPARE DATA
# =========================================================

# Read datasets
exp_raw <- read.csv("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/greaternoise2/greater_noise_exp2/data_cleaned.csv")
avg_raw <- read.csv("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/greaternoise2/grater_noise_avg2/data_cleaned.csv")


# Keep relevant columns and add condition label
exp_dat <- exp_raw %>%
  select(participant_id, trial, fixationTime, meanVal, indexSelected, array_length, color) %>%
  mutate(condition = "experience")

avg_dat <- avg_raw %>%
  select(participant_id, trial, fixationTime, meanVal, indexSelected, array_length, color) %>%
  mutate(condition = "average")

# Combine datasets
filt_dat <- bind_rows(exp_dat, avg_dat) %>%
  mutate(
    participant_id = interaction(condition, participant_id, drop = TRUE),
    participant_id = factor(participant_id),
    condition = factor(condition, levels = c("average", "experience")),
    array_length = factor(array_length, levels = c(2, 10, 20))
  ) %>%
  filter(!is.na(array_length))

# =========================================================
# 2. HELPER OBJECT FOR SIDE TICKS
# =========================================================

tick_vals <- seq(10, 40, by = 5)

scale_df <- expand.grid(
  condition = levels(filt_dat$condition),
  array_length = levels(filt_dat$array_length),
  tick = tick_vals
)

scale_df$condition <- factor(scale_df$condition, levels = levels(filt_dat$condition))
scale_df$array_length <- factor(scale_df$array_length, levels = levels(filt_dat$array_length))

# =========================================================
# 3. STANDARD GALTON DATA
#    X = objective mean
#    Y = mean response
# =========================================================

galton_data <- filt_dat %>%
  group_by(meanVal, array_length, condition) %>%
  summarise(
    mean_response = mean(indexSelected, na.rm = TRUE),
    .groups = "drop"
  )

# =========================================================
# 4. STANDARD GALTON SQUEEZE PLOT
# =========================================================

ggplot(galton_data) +
  geom_segment(
    aes(x = 1, xend = 2, y = meanVal, yend = mean_response),
    alpha = 0.35,
    linewidth = 0.6,
    color = "grey50"
  ) +
  geom_point(
    aes(x = 1, y = meanVal),
    size = 1.2,
    alpha = 0.8,
    color = "black"
  ) +
  geom_point(
    aes(x = 2, y = mean_response),
    size = 1.6,
    alpha = 0.9,
    color = "firebrick"
  ) +
  geom_vline(xintercept = c(1, 2), linewidth = 0.5) +
  geom_segment(
    data = scale_df,
    aes(x = 0.985, xend = 1, y = tick, yend = tick),
    inherit.aes = FALSE,
    linewidth = 0.35
  ) +
  geom_segment(
    data = scale_df,
    aes(x = 2, xend = 2.015, y = tick, yend = tick),
    inherit.aes = FALSE,
    linewidth = 0.35
  ) +
  geom_text(
    data = scale_df,
    aes(x = 0.96, y = tick, label = tick),
    inherit.aes = FALSE,
    hjust = 1,
    size = 2.7
  ) +
  geom_text(
    data = scale_df,
    aes(x = 2.04, y = tick, label = tick),
    inherit.aes = FALSE,
    hjust = 0,
    size = 2.7
  ) +
  facet_grid(condition ~ array_length) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Objective mean", "Mean response"),
    expand = expansion(mult = c(0.28, 0.28))
  ) +
  coord_cartesian(ylim = c(10, 40), clip = "off") +
  labs(
    title = "Galton squeeze diagrams by array length",
    x = NULL,
    y = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1.2, "lines")
  )

# =========================================================
# 5. REVERSE GALTON DATA
#    X = response given
#    Y = mean true value
# =========================================================

reverse_galton_data <- filt_dat %>%
  group_by(indexSelected, array_length, condition) %>%
  summarise(
    mean_true = mean(meanVal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(response_value = indexSelected)

# =========================================================
# 6. REVERSE GALTON SQUEEZE PLOT
# =========================================================

ggplot(reverse_galton_data) +
  geom_segment(
    aes(x = 1, xend = 2, y = response_value, yend = mean_true),
    alpha = 0.35,
    linewidth = 0.6,
    color = "grey50"
  ) +
  geom_point(
    aes(x = 1, y = response_value),
    size = 1.2,
    alpha = 0.8,
    color = "firebrick"
  ) +
  geom_point(
    aes(x = 2, y = mean_true),
    size = 1.4,
    alpha = 0.9,
    color = "black"
  ) +
  geom_vline(xintercept = c(1, 2), linewidth = 0.5) +
  geom_segment(
    data = scale_df,
    aes(x = 0.985, xend = 1, y = tick, yend = tick),
    inherit.aes = FALSE,
    linewidth = 0.35
  ) +
  geom_segment(
    data = scale_df,
    aes(x = 2, xend = 2.015, y = tick, yend = tick),
    inherit.aes = FALSE,
    linewidth = 0.35
  ) +
  geom_text(
    data = scale_df,
    aes(x = 0.96, y = tick, label = tick),
    inherit.aes = FALSE,
    hjust = 1,
    size = 2.7
  ) +
  geom_text(
    data = scale_df,
    aes(x = 2.04, y = tick, label = tick),
    inherit.aes = FALSE,
    hjust = 0,
    size = 2.7
  ) +
  facet_grid(condition ~ array_length) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Mean response", "Objective mean"),
    expand = expansion(mult = c(0.28, 0.28))
  ) +
  coord_cartesian(ylim = c(0, 50), clip = "off") +
  labs(
    title = "Reverse Galton squeeze diagrams by array length",
    x = NULL,
    y = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1.2, "lines")
  )

# =========================================================
# 7. REVERSE GALTON SEPARATED BY CONDITION
# =========================================================

reverse_galton_data_avg <- reverse_galton_data %>%
  filter(condition == "average")

reverse_galton_data_exp <- reverse_galton_data %>%
  filter(condition == "experience")

scale_df_avg <- scale_df %>%
  filter(condition == "average")

scale_df_exp <- scale_df %>%
  filter(condition == "experience")

# Average condition
ggplot(reverse_galton_data_avg) +
  geom_segment(
    aes(x = 1, xend = 2, y = response_value, yend = mean_true),
    alpha = 0.35,
    linewidth = 0.6,
    color = "grey50"
  ) +
  geom_point(
    aes(x = 1, y = response_value),
    size = 1.2,
    alpha = 0.8,
    color = "firebrick"
  ) +
  geom_point(
    aes(x = 2, y = mean_true),
    size = 1.4,
    alpha = 0.9,
    color = "black"
  ) +
  geom_vline(xintercept = c(1, 2), linewidth = 0.5) +
  geom_segment(
    data = scale_df_avg,
    aes(x = 0.985, xend = 1, y = tick, yend = tick),
    inherit.aes = FALSE,
    linewidth = 0.35
  ) +
  geom_segment(
    data = scale_df_avg,
    aes(x = 2, xend = 2.015, y = tick, yend = tick),
    inherit.aes = FALSE,
    linewidth = 0.35
  ) +
  geom_text(
    data = scale_df_avg,
    aes(x = 0.96, y = tick, label = tick),
    inherit.aes = FALSE,
    hjust = 1,
    size = 2.7
  ) +
  geom_text(
    data = scale_df_avg,
    aes(x = 2.04, y = tick, label = tick),
    inherit.aes = FALSE,
    hjust = 0,
    size = 2.7
  ) +
  facet_wrap(~ array_length, nrow = 1) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Mean response", "Objective mean"),
    expand = expansion(mult = c(0.45, 0.45))
  ) +
  coord_cartesian(ylim = c(0, 50), clip = "off") +
  labs(
    title = "Reverse Galton squeeze diagrams – average condition",
    x = NULL,
    y = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(2, "lines"),
    plot.margin = margin(t = 15, r = 40, b = 25, l = 40)
  )

# Experience condition
ggplot(reverse_galton_data_exp) +
  geom_segment(
    aes(x = 1, xend = 2, y = response_value, yend = mean_true),
    alpha = 0.35,
    linewidth = 0.6,
    color = "grey50"
  ) +
  geom_point(
    aes(x = 1, y = response_value),
    size = 1.2,
    alpha = 0.8,
    color = "firebrick"
  ) +
  geom_point(
    aes(x = 2, y = mean_true),
    size = 1.4,
    alpha = 0.9,
    color = "black"
  ) +
  geom_vline(xintercept = c(1, 2), linewidth = 0.5) +
  geom_segment(
    data = scale_df_exp,
    aes(x = 0.985, xend = 1, y = tick, yend = tick),
    inherit.aes = FALSE,
    linewidth = 0.35
  ) +
  geom_segment(
    data = scale_df_exp,
    aes(x = 2, xend = 2.015, y = tick, yend = tick),
    inherit.aes = FALSE,
    linewidth = 0.35
  ) +
  geom_text(
    data = scale_df_exp,
    aes(x = 0.96, y = tick, label = tick),
    inherit.aes = FALSE,
    hjust = 1,
    size = 2.7
  ) +
  geom_text(
    data = scale_df_exp,
    aes(x = 2.04, y = tick, label = tick),
    inherit.aes = FALSE,
    hjust = 0,
    size = 2.7
  ) +
  facet_wrap(~ array_length, nrow = 1) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Mean response", "Objective mean"),
    expand = expansion(mult = c(0.45, 0.45))
  ) +
  coord_cartesian(ylim = c(0, 50), clip = "off") +
  labs(
    title = "Reverse Galton squeeze diagrams – experience condition",
    x = NULL,
    y = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(2, "lines"),
    plot.margin = margin(t = 15, r = 40, b = 25, l = 40)
  )

# =========================================================
# 8. BUCKETED SCATTER:
#    SUBJECTIVE RESPONSE -> MEAN OBJECTIVE VALUE
# =========================================================

scatter_bucketed <- filt_dat %>%
  mutate(
    bucket_start = floor((indexSelected - 1) / 3) * 3 + 1,
    bucket_end = bucket_start + 2,
    response_bucket = bucket_start + 1,
    bucket_label = paste0(bucket_start, "-", bucket_end)
  ) %>%
  group_by(response_bucket, bucket_label, array_length, condition) %>%
  summarise(
    mean_true = mean(meanVal, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

scatter_avg <- scatter_bucketed %>%
  filter(condition == "average")

scatter_exp <- scatter_bucketed %>%
  filter(condition == "experience")

# Both conditions together
ggplot(scatter_bucketed, aes(x = response_bucket, y = mean_true, color = array_length)) +
  geom_line(aes(group = array_length), linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.85) +
  facet_wrap(~ condition) +
  scale_x_continuous(breaks = seq(2, 50, by = 3)) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 50)) +
  labs(
    title = "Mean objective value by bucketed subjective response",
    x = "Subjective response (buckets of 3)",
    y = "Mean objective value",
    color = "Array length"
  ) +
  theme_classic(base_size = 14)

# Average only
ggplot(scatter_avg, aes(x = response_bucket, y = mean_true, color = array_length)) +
  geom_line(aes(group = array_length), linewidth = 0.9, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.9) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_x_continuous(breaks = seq(2, 50, by = 6)) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 50)) +
  labs(
    title = "Average condition",
    x = "Subjective response (binned)",
    y = "Mean objective value",
    color = "Array length"
  ) +
  theme_classic(base_size = 14)

# Experience only
ggplot(scatter_exp, aes(x = response_bucket, y = mean_true, color = array_length)) +
  geom_line(aes(group = array_length), linewidth = 0.9, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.9) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_x_continuous(breaks = seq(2, 50, by = 6)) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 50)) +
  labs(
    title = "Experience condition",
    x = "Subjective response (binned)",
    y = "Mean objective value",
    color = "Array length"
  ) +
  theme_classic(base_size = 14)

# =========================================================
# 9. FORWARD BUCKETED SCATTER:
#    OBJECTIVE MEAN -> MEAN SUBJECTIVE RESPONSE
# =========================================================

scatter_forward <- filt_dat %>%
  mutate(
    mean_bucket = floor((meanVal - 1) / 3) * 3 + 2
  ) %>%
  group_by(mean_bucket, array_length, condition) %>%
  summarise(
    mean_response = mean(indexSelected, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

scatter_forward_avg <- scatter_forward %>%
  filter(condition == "average")

scatter_forward_exp <- scatter_forward %>%
  filter(condition == "experience")

# Average only
ggplot(scatter_forward_avg, aes(x = mean_bucket, y = mean_response, color = array_length)) +
  geom_line(aes(group = array_length), linewidth = 0.9, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.9) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_x_continuous(breaks = seq(2, 50, by = 6)) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 50)) +
  labs(
    title = "Average condition",
    x = "Objective mean (binned)",
    y = "Mean subjective response",
    color = "Array length"
  ) +
  theme_classic(base_size = 14)

# Experience only
ggplot(scatter_forward_exp, aes(x = mean_bucket, y = mean_response, color = array_length)) +
  geom_line(aes(group = array_length), linewidth = 0.9, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.9) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_x_continuous(breaks = seq(2, 50, by = 6)) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 50)) +
  labs(
    title = "Experience condition",
    x = "Objective mean (binned)",
    y = "Mean subjective response",
    color = "Array length"
  ) +
  theme_classic(base_size = 14)