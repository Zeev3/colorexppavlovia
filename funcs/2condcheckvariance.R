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
exp_raw <- read.csv("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/variancecontrol/variance_control_exp/data_cleaned.csv")
avg_raw <- read.csv("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/variancecontrol/variance_control_avg/data_cleaned.csv")

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

# Sanity checks
table(filt_dat$condition, useNA = "ifany")
table(filt_dat$variance_level, useNA = "ifany")
table(filt_dat$array_length, filt_dat$variance_level, useNA = "ifany")

# =========================================================
# 2. ANALYSIS 1:
# LMM TESTING DIFFERENCES BETWEEN CONDITIONS
# =========================================================

RttM_model_condition_null <- lmer(
  indexSelected ~ meanVal_c * variance_level + condition_ec +
    (1 + meanVal_c * variance_level | participant_id),
  data = filt_dat
)
RttM_model_condition <- lmer(
  indexSelected ~ meanVal_c * variance_level * condition_ec +
    (1 + meanVal_c * variance_level | participant_id),
  data = filt_dat
)

summary(RttM_model_condition)
anova(RttM_model_condition)
BIC(RttM_model_condition,RttM_model_condition_null)
# Estimated slopes of objective mean in each condition x variance cell
emtrends(
  RttM_model_condition,
  ~ condition_ec * variance_level,
  var = "meanVal_c"
)

# All pairwise comparisons of slopes
pairs(
  emtrends(
    RttM_model_condition,
    ~ condition_ec * variance_level,
    var = "meanVal_c"
  )
)

# Condition differences in slope at each variance level
emtrends(
  RttM_model_condition,
  pairwise ~ condition_ec | variance_level,
  var = "meanVal_c"
)

# Variance-level differences in slope within each condition
emtrends(
  RttM_model_condition,
  pairwise ~ variance_level | condition_ec,
  var = "meanVal_c"
)

# Predictions for plotting
predictions_condition <- ggpredict(
  RttM_model_condition,
  terms = c("meanVal_c [all]", "variance_level", "condition_ec")
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
  group_by(participant_id, variance_level, condition) %>%
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
  within = "variance_level",
  between = "condition",
  type = 3
)

summary(exagg_aov)

# Optional post hoc tests
emmeans(exagg_aov, ~ variance_level) %>% pairs()
emmeans(exagg_aov, ~ variance_level | condition) %>% pairs()

# -------------------------
# Plot: violin + points + mean/SE
# -------------------------
ggplot(exagg_data, aes(x = variance_level, y = prop_exagg, fill = variance_level)) +
  geom_violin(alpha = 0.35, trim = FALSE) +
  geom_jitter(width = 0.08, alpha = 0.35, size = 1.5) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.12, color = "black") +
  facet_wrap(~ condition) +
  labs(
    x = "Variance level (SD)",
    y = "Proportion of exaggerated responses",
    fill = "Variance level",
    title = "Exaggeration as a function of variance level and condition"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic()

# Optional repeated-measures style plot
ggplot(exagg_data,
       aes(x = variance_level, y = prop_exagg, group = participant_id)) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.3, size = 1.2) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(aes(group = 1), fun = mean, geom = "point", size = 3) +
  stat_summary(aes(group = 1), fun.data = mean_se, geom = "errorbar", width = 0.1) +
  facet_wrap(~ condition) +
  labs(
    x = "Variance level (SD)",
    y = "Proportion of exaggerated responses",
    title = "Participant-level exaggeration across variance levels"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic()

# =========================================================
# 4. ANALYSIS 3:
# SINGLE-SUBJECT CORRELATIONS + FISHER Z
# =========================================================

corr_data <- filt_dat %>%
  group_by(participant_id, variance_level, condition) %>%
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
  within = "variance_level",
  between = "condition",
  type = 3
)

summary(corr_aov)

# Optional post hoc tests
emmeans(corr_aov, ~ variance_level) %>% pairs()
emmeans(corr_aov, ~ variance_level | condition) %>% pairs()

# Descriptive summary with back-transformed r
corr_summary <- corr_data %>%
  group_by(variance_level, condition) %>%
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
       aes(x = variance_level, y = z, group = participant_id)) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.3, size = 1.2) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(aes(group = 1), fun = mean, geom = "point", size = 3) +
  stat_summary(aes(group = 1), fun.data = mean_se, geom = "errorbar", width = 0.1) +
  facet_wrap(~ condition) +
  labs(
    x = "Variance level (SD)",
    y = "Fisher z-transformed correlation",
    title = "Tracking accuracy across variance levels and conditions"
  ) +
  theme_classic()

  # -------------------------
# Plot: descriptive back-transformed Pearson r
# -------------------------
ggplot(corr_summary,
       aes(x = variance_level, y = mean_r, group = condition, color = condition)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_low_r, ymax = ci_high_r), width = 0.1) +
  labs(
    x = "Variance level (SD)",
    y = "Mean Pearson correlation",
    color = "Condition",
    title = "Descriptive tracking accuracy across variance levels"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic()

library(dplyr)
library(tidyr)
library(gt)

apa_corr_table <- corr_summary %>%
  mutate(
    mean_r_apa = sprintf("%.2f", mean_r),
    mean_r_apa = sub("^0", "", mean_r_apa),
    ci_apa = paste0(
      "[",
      sub("^0", "", sprintf("%.2f", ci_low_r)),
      ", ",
      sub("^0", "", sprintf("%.2f", ci_high_r)),
      "]"
    ),
    condition = recode(condition,
                       "average" = "Average",
                       "experience" = "Experience"),
    variance_level = as.character(variance_level)
  ) %>%
  select(variance_level, condition, mean_r_apa, ci_apa) %>%
  pivot_wider(
    names_from = condition,
    values_from = c(mean_r_apa, ci_apa)
  ) %>%
  transmute(
    `Variance level` = variance_level,
    `Average r` = mean_r_apa_Average,
    `Average 95% CI` = ci_apa_Average,
    `Experience r` = mean_r_apa_Experience,
    `Experience 95% CI` = ci_apa_Experience
  )

apa_corr_table_gt <- apa_corr_table %>%
  gt() %>%
  tab_header(
    title = md("**Table 4**"),
    subtitle = md("*Mean Pearson correlations between objective mean and reported values by variance level and condition*")
  ) %>%
  cols_label(
    `Variance level` = "Variance level",
    `Average r` = md("Average<br>*r*"),
    `Average 95% CI` = "95% CI",
    `Experience r` = md("Experience<br>*r*"),
    `Experience 95% CI` = "95% CI"
  ) %>%
  tab_spanner(
    label = "Average condition",
    columns = c(`Average r`, `Average 95% CI`)
  ) %>%
  tab_spanner(
    label = "Experience condition",
    columns = c(`Experience r`, `Experience 95% CI`)
  ) %>%
  tab_source_note(
    source_note = md("*Note.* Correlations were Fisher *z*-transformed before averaging and then back-transformed to Pearson *r*.")
  ) %>%
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = 12,
    heading.align = "left",
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    column_labels.border.top.width = px(1),
    column_labels.border.bottom.width = px(1),
    data_row.padding = px(4)
  )

apa_corr_table_gt

