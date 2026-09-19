library(lme4)
library(lmerTest)
library(ggeffects)
library(ggplot2)
library(dplyr)

setwd("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/variancecontrol/variance_control_exp")

# read data
Data_raw <- read.csv("data_cleaned.csv")

# keep relevant variables
filt_dat <- Data_raw %>%
  select(participant_id, trial, fixationTime, meanVal, indexSelected, array_length, color) %>%
  na.omit()

# participant id as factor
filt_dat$participant_id <- factor(filt_dat$participant_id)
filt_dat <- filt_dat %>%
  mutate(
    variance_level = case_when(
      array_length == 2 ~ 3,
      array_length == 6 ~ 5,
      array_length == 8 ~ 7,
      TRUE ~ NA_real_
    )
  )
filt_dat$variance_level <- factor(filt_dat$variance_level,
                                  levels = c(3, 5, 7))

filt_dat$variance_level <- relevel(filt_dat$variance_level, ref = "3")


# center objective mean within participant
filt_dat <- filt_dat %>%
  group_by(participant_id) %>%
  mutate(meanVal_c = meanVal - mean(meanVal, na.rm = TRUE)) %>%
  ungroup()

# optional: inspect coding
contrasts(filt_dat$variance_level)

# -----------------------------
# Main preregistered model
# -----------------------------
# DV = trial-level subjective experience (indexSelected)
# Fixed effects:
#   mean-centered objective mean
#   variance level (3 levels, reference = SD=3)
#   interaction
# Random effects:
#   by-subject random intercepts and slopes for meanVal_c, variance_level, and interaction

RttM_model_main <- lmer(
  indexSelected ~ meanVal_c * variance_level +
    (1 + meanVal_c * variance_level | participant_id),
  data = filt_dat
)

summary(RttM_model_main)
anova(RttM_model_main)

# -----------------------------
# If model does not converge:
# Barr et al. style simplification
# remove low-variance random slopes gradually
# -----------------------------

# 1. remove interaction random slopes first
RttM_model_simpler_1 <- lmer(
  indexSelected ~ meanVal_c * variance_level +
    (1 + meanVal_c + variance_level | participant_id),
  data = filt_dat
)

summary(RttM_model_simpler_1)

# 2. if still needed, remove correlation among random effects
RttM_model_simpler_2 <- lmer(
  indexSelected ~ meanVal_c * variance_level +
    (1 + meanVal_c + variance_level || participant_id),
  data = filt_dat
)

summary(RttM_model_simpler_2)

# 3. if still needed, reduce further
RttM_model_simpler_3 <- lmer(
  indexSelected ~ meanVal_c * variance_level +
    (1 + meanVal_c | participant_id),
  data = filt_dat,
)

summary(RttM_model_simpler_3)

# -----------------------------
# Interpretation of fixed effects
# -----------------------------
# With SD = 3 as reference:
# (1) meanVal_c coefficient = slope of objective mean at SD = 3
# (2) meanVal_c:variance_level5 = change in slope when moving from SD = 3 to SD = 5
# (3) meanVal_c:variance_level7 = change in slope when moving from SD = 3 to SD = 7

# To test simple slopes directly:
library(emmeans)

emtrends(RttM_model_main, ~ variance_level, var = "meanVal_c")
pairs(emtrends(RttM_model_main, ~ variance_level, var = "meanVal_c"))

# -----------------------------
# Predictions for plotting
# -----------------------------
predictions_across <- ggpredict(RttM_model_main, terms = c("meanVal_c [all]", "variance_level"))

# sample descriptives for subtitle
n_participants <- filt_dat %>%
  distinct(participant_id) %>%
  nrow()

avg_trials <- filt_dat %>%
  count(participant_id) %>%
  summarise(mean_trials = mean(n)) %>%
  pull(mean_trials) %>%
  round(1)

# Plot 1: standard prediction plot
ggplot(predictions_across, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.20, color = NA) +
  labs(
    x = "Objective mean (centered within participant)",
    y = "Predicted Average estimation",
    color = "Variance level (SD)",
    fill = "Variance level (SD)",
    title = "Average estimation as a function of objective mean and variance",
    subtitle = paste0("N = ", n_participants,
                      ", Mean trials per participant = ", avg_trials)
  ) +
  theme_classic()

# -----------------------------
# Optional: get predictions on original meanVal scale for prettier plot
# -----------------------------
# create grand-mean centered display variable if you prefer plotting raw-looking x-axis
grand_mean <- mean(filt_dat$meanVal, na.rm = TRUE)

predictions_across$meanVal_raw_approx <- predictions_across$x + grand_mean

ggplot(predictions_across,
       aes(x = meanVal_raw_approx, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.20, color = NA) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    x = "Objective mean",
    y = "Predicted Average estimation",
    color = "Variance level (SD)",
    fill = "Variance level (SD)",
    title = "Average condition"
  ) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 50)) +
  theme_classic()
library(patchwork)

ggplot(predictions_across,
       aes(x = predicted,
           y = meanVal_raw_approx,
           color = group,
           fill = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(xmin = conf.low, xmax = conf.high),
    alpha = 0.2,
    color = NA,
    orientation = "y"
  ) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    x = "Predicted Average estimation",
    y = "Objective mean",
    color = "Variance level (SD)",
    fill = "Variance level (SD)",
    title = "Average condition (reversed axes, original scale)"
  ) +
  theme_classic()


ggplot(predictions_across,
       aes(x = predicted,
           y = meanVal_raw_approx,
           color = group,
           fill = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(xmin = conf.low, xmax = conf.high),
    alpha = 0.2,
    color = NA,
    orientation = "y"
  ) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed",
    linewidth = 1
  ) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 50)) +
  labs(
    x = "Predicted Average estimation",
    y = "Objective mean",
    color = "Variance level (SD)",
    fill = "Variance level (SD)",
    title = "Average condition (reversed axes, original scale)"
  ) +
  theme_classic()
print(p)

