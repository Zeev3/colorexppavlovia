# =========================================================
# PACKAGES
# =========================================================
library(lme4)
library(lmerTest)
library(ggeffects)
library(ggplot2)
library(dplyr)
library(afex)
library(emmeans)
library(tidyr)

# =========================================================
# 1. LOAD AND PREPARE DATA
# =========================================================

# Read datasets
exp_raw <- read.csv("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/variance_control_exp/data_cleaned.csv")
avg_raw <- read.csv("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/variance_control_avg/data_cleaned.csv")

# Keep relevant columns and add condition
exp_dat <- exp_raw %>%
  select(participant_id, trial, fixationTime, meanVal, indexSelected, array_length, color) %>%
  mutate(condition = "experience")

avg_dat <- avg_raw %>%
  select(participant_id, trial, fixationTime, meanVal, indexSelected, array_length, color) %>%
  mutate(condition = "average")

# Combine datasets
filt_dat <- bind_rows(exp_dat, avg_dat)

# Create unique participant IDs across conditions
filt_dat <- filt_dat %>%
  mutate(
    participant_id = interaction(condition, participant_id, drop = TRUE),
    participant_id = factor(participant_id)
  )

# Recode array length to variance level
# 2 -> 3, 6 -> 5, 8 -> 7
filt_dat <- filt_dat %>%
  mutate(
    variance_level = case_when(
      array_length == 2 ~ 3,
      array_length == 6 ~ 5,
      array_length == 8 ~ 7,
      TRUE ~ NA_real_
    )
  )

# Factor coding
filt_dat$variance_level <- factor(filt_dat$variance_level, levels = c(3, 5, 7))
filt_dat$variance_level <- relevel(filt_dat$variance_level, ref = "3")

filt_dat$condition <- factor(filt_dat$condition, levels = c("average", "experience"))

# Effect coding for condition: average = -1, experience = 1
filt_dat <- filt_dat %>%
  mutate(condition_ec = ifelse(condition == "average", -1, 1))

# Center objective mean within participant
filt_dat <- filt_dat %>%
  group_by(participant_id) %>%
  mutate(meanVal_c = meanVal - mean(meanVal, na.rm = TRUE)) %>%
  ungroup()


galton_data <- filt_dat %>%
  group_by(meanVal, variance_level, condition) %>%
  summarise(
    mean_response = mean(indexSelected),
    .groups = "drop"
  ) %>%
  mutate(id = meanVal)

galton_long <- galton_data %>%
  pivot_longer(
    cols = c(meanVal, mean_response),
    names_to = "side",
    values_to = "value"
  ) %>%
  mutate(
    x = ifelse(side == "meanVal", 1, 2)
  )

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
  facet_grid(condition ~ variance_level) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Objective mean", "Mean response"),
    expand = expansion(mult = c(0.18, 0.18))
  ) +
  coord_cartesian(ylim = c(10, 40), clip = "off") +
  labs(
    title = "Galton squeeze diagrams",
    x = NULL,
    y = "Value"
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1.2, "lines")
  )

ggplot(galton_data_binned) +
  
  # connecting lines
  geom_segment(
    aes(
      x = 1, xend = 2,
      y = meanVal_bin, yend = mean_response
    ),
    linewidth = 0.6,
    alpha = 0.6,
    color = "grey30"
  ) +
  
  #  LEFT SIDE labels (objective mean)
  geom_text(
    aes(
      x = 1,
      y = meanVal_bin,
      label = round(meanVal_bin)
    ),
    hjust = 1.2,          # push left
    size = 2.8
  ) +
  
  #  RIGHT SIDE labels (mean response)
  geom_text(
    aes(
      x = 2,
      y = mean_response,
      label = round(mean_response, 1)
    ),
    hjust = -0.2,         # push right
    size = 2.8
  ) +
  
  #  vertical axis lines
  geom_vline(xintercept = c(1, 2), linewidth = 0.6) +
  
  facet_grid(condition ~ variance_level) +
  
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Objective mean", "Mean response"),
    expand = expansion(mult = c(0.35, 0.35))  # space for labels
  ) +
  
  coord_cartesian(ylim = c(10, 40), clip = "off") +
  
  labs(
    title = "Galton squeeze diagrams",
    x = NULL,
    y = NULL
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    axis.text.y = element_blank(),   
    axis.ticks.y = element_blank(),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1.2, "lines")
  )

# add this with the other packages


# ---------------------------------------------------------
# build custom tick labels for BOTH sides of every facet
# ---------------------------------------------------------
tick_vals <- seq(10, 40, by = 5)

scale_df <- expand.grid(
  condition = levels(galton_data$condition),
  variance_level = levels(galton_data$variance_level),
  tick = tick_vals
)

scale_df$condition <- factor(scale_df$condition, levels = levels(galton_data$condition))
scale_df$variance_level <- factor(scale_df$variance_level, levels = levels(galton_data$variance_level))

# ---------------------------------------------------------
# UNBINNED GALTON SQUEEZE WITH SCALE ON BOTH SIDES
# ---------------------------------------------------------
ggplot(galton_data) +
  
  # connecting lines
  geom_segment(
    aes(x = 1, xend = 2, y = meanVal, yend = mean_response),
    alpha = 0.35,
    linewidth = 0.6,
    color = "grey50"
  ) +
  
  # left side points
  geom_point(
    aes(x = 1, y = meanVal),
    size = 1.2,
    alpha = 0.8,
    color = "black"
  ) +
  
  # right side points
  geom_point(
    aes(x = 2, y = mean_response),
    size = 1.6,
    alpha = 0.9,
    color = "firebrick"
  ) +
  
  # vertical axis lines
  geom_vline(xintercept = c(1, 2), linewidth = 0.5) +
  
  # small tick marks on LEFT side
  geom_segment(
    data = scale_df,
    aes(x = 0.985, xend = 1, y = tick, yend = tick),
    inherit.aes = FALSE,
    linewidth = 0.35
  ) +
  
  # small tick marks on RIGHT side
  geom_segment(
    data = scale_df,
    aes(x = 2, xend = 2.015, y = tick, yend = tick),
    inherit.aes = FALSE,
    linewidth = 0.35
  ) +
  
  # LEFT labels
  geom_text(
    data = scale_df,
    aes(x = 0.96, y = tick, label = tick),
    inherit.aes = FALSE,
    hjust = 1,
    size = 2.7
  ) +
  
  # RIGHT labels
  geom_text(
    data = scale_df,
    aes(x = 2.04, y = tick, label = tick),
    inherit.aes = FALSE,
    hjust = 0,
    size = 2.7
  ) +
  
  facet_grid(condition ~ variance_level) +
  
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Objective mean", "Mean response"),
    expand = expansion(mult = c(0.28, 0.28))
  ) +
  
  coord_cartesian(ylim = c(10, 40), clip = "off") +
  
  labs(
    title = "Galton squeeze diagrams",
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
