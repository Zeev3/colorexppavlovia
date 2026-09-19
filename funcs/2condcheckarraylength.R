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

# =========================================================
# 1. LOAD AND PREPARE DATA
# =========================================================

# Read datasets
exp_raw <- read.csv("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/greaternoise2/greater_noise_exp2/data_cleaned.csv")
avg_raw <- read.csv("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/greaternoise2/grater_noise_avg2/data_cleaned.csv")

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


# Factor coding
filt_dat$array_length <- factor(filt_dat$array_length)
#filt_dat$array_length <- relevel(filt_dat$variance_level, ref = "2")

filt_dat$condition <- factor(filt_dat$condition, levels = c("average", "experience"))

# Effect coding for condition: average = -1, experience = 1
filt_dat <- filt_dat %>%
  mutate(condition_ec = ifelse(condition == "average", -1, 1))

# Center objective mean within participant
filt_dat <- filt_dat %>%
  group_by(participant_id) %>%
  mutate(meanVal_c = meanVal - mean(meanVal, na.rm = TRUE)) %>%
  ungroup()

# Sanity checks
table(filt_dat$condition, useNA = "ifany")
table(filt_dat$array_length, useNA = "ifany")

# =========================================================
# 2. ANALYSIS 1:
# LMM TESTING DIFFERENCES BETWEEN CONDITIONS
# =========================================================

RttM_model_condition <- lmer(
  indexSelected ~ meanVal_c * array_length * condition_ec +
    (1  + meanVal_c * array_length| participant_id),
  data = filt_dat
)

summary(RttM_model_condition)
anova(RttM_model_condition)

# -----------------------------
# If model does not converge:
# Barr et al. style simplification
# remove low-variance random slopes gradually
# -----------------------------

# 1. remove interaction random slopes first
RttM_model_simpler_1 <- lmer(
  indexSelected ~ meanVal_c * array_length * condition_ec +
    (1 + meanVal_c + array_length  | participant_id),
  data = filt_dat
)

summary(RttM_model_simpler_1)
anova(RttM_model_simpler_1)

# 2. if still needed, remove correlation among random effects
RttM_model_simpler_2 <- lmer(
  indexSelected ~ meanVal_c * array_length * condition_ec +
    (1 + meanVal_c + array_length  || participant_id),
  data = filt_dat
)

summary(RttM_model_simpler_2)
anova(RttM_model_simpler_2)

# 3. if still needed, reduce further
RttM_model_simpler_3 <- lmer(
  indexSelected ~ meanVal_c * array_length * condition_ec +
    (1 + meanVal_c | participant_id),
  data = filt_dat,
)

summary(RttM_model_simpler_3)
anova(RttM_model_simpler_3)


# 4. alternative reduction 
RttM_model_simpler_4 <- lmer(
  indexSelected ~ meanVal_c * array_length * condition_ec +
    (1 + array_length | participant_id),
  data = filt_dat,
)

summary(RttM_model_simpler_4)
anova(RttM_model_simpler_4)

working_model <-  RttM_model_simpler_2 #put here the model that converged 

# Estimated slopes of objective mean in each condition x variance cell
emtrends(
  working_model,
  ~ condition_ec * array_length,
  var = "meanVal_c"
)

# All pairwise comparisons of slopes
pairs(
  emtrends(
    working_model,
    ~ condition_ec * array_length,
    var = "meanVal_c"
  )
)

# Condition differences in slope at each variance level
emtrends(
  working_model,
  pairwise ~ condition_ec | array_length,
  var = "meanVal_c"
)

# Variance-level differences in slope within each condition
emtrends(
  working_model,
  pairwise ~ array_length | condition_ec,
  var = "meanVal_c"
)

# Predictions for plotting
predictions_condition <- ggpredict(
  working_model,
  terms = c("meanVal_c [all]", "array_length", "condition_ec")
)

plot(predictions_condition)

# =========================================================
# 3. ANALYSIS 2:
# EXAGGERATION EFFECT
# =========================================================

# Define exaggerated responses
filt_dat <- filt_dat %>%
  mutate(
    exaggerated = case_when(
      meanVal < 20 & indexSelected < 14 ~ 1,
      meanVal > 30 & indexSelected > 36 ~ 1,
      TRUE ~ 0
    )
  )

# Aggregate to participant x variance x condition
exagg_data <- filt_dat %>%
  group_by(participant_id, array_length, condition) %>%
  summarise(
    prop_exagg = mean(exaggerated),
    n_trials = n(),
    .groups = "drop"
  )

# Mixed ANOVA
exagg_aov <- aov_ez(
  id = "participant_id",
  dv = "prop_exagg",
  data = exagg_data,
  within = "array_length",
  between = "condition",
  type = 3
)

summary(exagg_aov)

# Optional post hoc tests
emmeans(exagg_aov, ~ array_length) %>% pairs()
emmeans(exagg_aov, ~ array_length | condition) %>% pairs()

# -------------------------
# Plot: violin + points + mean/SE
# -------------------------
ggplot(exagg_data, aes(x = array_length, y = prop_exagg, fill = array_length)) +
  geom_violin(alpha = 0.35, trim = FALSE) +
  geom_jitter(width = 0.08, alpha = 0.35, size = 1.5) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.12, color = "black") +
  facet_wrap(~ condition) +
  labs(
    x = "Number of squares",
    y = "Proportion of exaggerated responses",
    fill = "Number of squares",
    title = "Exaggeration as a function of Number of squares and condition"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic()

# Optional repeated-measures style plot
ggplot(exagg_data,
       aes(x = array_length, y = prop_exagg, group = participant_id)) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.3, size = 1.2) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(aes(group = 1), fun = mean, geom = "point", size = 3) +
  stat_summary(aes(group = 1), fun.data = mean_se, geom = "errorbar", width = 0.1) +
  facet_wrap(~ condition) +
  labs(
    x = "Number of squares",
    y = "Proportion of exaggerated responses",
    title = "Participant-level exaggeration across Number of squares"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic()

# =========================================================
# 4. ANALYSIS 3:
# SINGLE-SUBJECT CORRELATIONS + FISHER Z
# =========================================================

corr_data <- filt_dat %>%
  group_by(participant_id, array_length, condition) %>%
  summarise(
    r = ifelse(
      n() > 2 && sd(meanVal) > 0 && sd(indexSelected) > 0,
      cor(meanVal, indexSelected),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    z = atanh(r)
  )

# Mixed ANOVA on Fisher z
corr_aov <- aov_ez(
  id = "participant_id",
  dv = "z",
  data = corr_data,
  within = "array_length",
  between = "condition",
  type = 3
)

summary(corr_aov)

# Optional post hoc tests
emmeans(corr_aov, ~ array_length) %>% pairs()
emmeans(corr_aov, ~ array_length | condition) %>% pairs()

# Descriptive summary with back-transformed r
corr_summary <- corr_data %>%
  group_by(array_length, condition) %>%
  summarise(
    mean_z = mean(z, na.rm = TRUE),
    se_z = sd(z, na.rm = TRUE) / sqrt(sum(!is.na(z))),
    ci_low_z = mean_z - 1.96 * se_z,
    ci_high_z = mean_z + 1.96 * se_z,
    mean_r = tanh(mean_z),
    ci_low_r = tanh(ci_low_z),
    ci_high_r = tanh(ci_high_z),
    .groups = "drop"
  )

corr_summary

# -------------------------
# Plot: participant-level Fisher z
# -------------------------
ggplot(corr_data,
       aes(x = array_length, y = z, group = participant_id)) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.3, size = 1.2) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(aes(group = 1), fun = mean, geom = "point", size = 3) +
  stat_summary(aes(group = 1), fun.data = mean_se, geom = "errorbar", width = 0.1) +
  facet_wrap(~ condition) +
  labs(
    x = "Number of squares",
    y = "Fisher z-transformed correlation",
    title = "Tracking accuracy across Number of squares and conditions"
  ) +
  theme_classic()

# -------------------------
# Plot: descriptive back-transformed Pearson r
# -------------------------
ggplot(corr_summary,
       aes(x = array_length, y = mean_r, group = condition, color = condition)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_low_r, ymax = ci_high_r), width = 0.1) +
  labs(
    x = "Number of squares",
    y = "Mean Pearson correlation",
    color = "Condition",
    title = "Descriptive tracking accuracy across Number of squares"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic()

