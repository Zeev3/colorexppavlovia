# =========================================================
# 2AFC Analysis: Choice Bias + RT Differences
# =========================================================

library(dplyr)
library(readr)
library(ggplot2)
library(lme4)
library(lmerTest)

# =========================================================
# Read cleaned data
# =========================================================

data_folder <- "/Users/zeevbenamos/Documents/GitHub/2facolorexp/data/pilot12fac"

all_data <- read_csv(
  file.path(data_folder, "data_cleaned.csv"),
  show_col_types = FALSE
)

# =========================================================
# Prepare variables
# =========================================================

all_data <- all_data %>%
  mutate(
    chosenSigned = as.numeric(chosenSigned),
    meanVal = as.numeric(meanVal),
    rt = as.numeric(rt),
    array_length = factor(array_length),
    
    scale_side = case_when(
      meanVal < 25 ~ "low",
      meanVal > 25 ~ "high",
      TRUE ~ NA_character_
    ),
    
    exaggerated_choice = case_when(
      meanVal < 25 & chosenSigned == -5 ~ 1,
      meanVal < 25 & chosenSigned ==  5 ~ 0,
      meanVal > 25 & chosenSigned ==  5 ~ 1,
      meanVal > 25 & chosenSigned == -5 ~ 0,
      TRUE ~ NA_real_
    ),
    
    choice_type = case_when(
      exaggerated_choice == 1 ~ "Exaggerated",
      exaggerated_choice == 0 ~ "Regressed",
      TRUE ~ NA_character_
    ),
    
    extremity = abs(meanVal - 25),
    meanVal_c = meanVal - mean(meanVal, na.rm = TRUE),
    log_rt = log10(rt)
  ) %>%
  filter(
    !is.na(exaggerated_choice),
    !is.na(log_rt)
  )

# =========================================================
# Analysis 1: Do participants choose exaggerated answers
# more than regressed answers?
# =========================================================

choice_model <- glmer(
  exaggerated_choice ~ 1 + (1 | participant_id),
  data = all_data,
  family = binomial
)

summary(choice_model)

# Convert intercept to probability
intercept <- fixef(choice_model)[1]
prob_exaggerated <- plogis(intercept)

prob_exaggerated

# More detailed model with array length and extremity
choice_model_full <- glmer(
  exaggerated_choice ~ extremity  +
    (1 | participant_id),
  data = all_data,
  family = binomial
)

summary(choice_model_full)
anova(choice_model)

choice_model_meanVal <- glmer(
  exaggerated_choice ~ meanVal  +
    (1 | participant_id),
  data = all_data,
  family = binomial
)

summary(choice_model_meanVal)
anova(choice_model_meanVal)

# =========================================================
# Choice plot: proportion exaggerated by meanVal
# =========================================================

choice_summary <- all_data %>%
  group_by(meanVal, array_length) %>%
  summarise(
    n = n(),
    prop_exaggerated = mean(exaggerated_choice, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(
  choice_summary,
  aes(
    x = meanVal,
    y = prop_exaggerated,
    color = array_length,
    group = array_length
  )
) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  labs(
    title = "Proportion of Exaggerated Choices Across Mean Values",
    x = "Objective mean value",
    y = "Proportion exaggerated choice",
    color = "Array length"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 14)

# =========================================================
# Analysis 2: Are RTs different for exaggerated vs regressed choices?
# =========================================================

rt_model <- lmer(
  log_rt ~ choice_type +
    (1 | participant_id),
  data = all_data
)

summary(rt_model)
anova(rt_model)

# Optional: control for extremity
rt_model_full <- lmer(
  log_rt ~ choice_type * extremity +
    (1 | participant_id),
  data = all_data
)

summary(rt_model_full)
anova(rt_model_full)

# =========================================================
# RT plot: mean RT by meanVal and choice type
# =========================================================

rt_summary <- all_data %>%
  group_by(meanVal, choice_type) %>%
  summarise(
    n = n(),
    mean_rt = mean(rt, na.rm = TRUE),
    se_rt = sd(rt, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

ggplot(
  rt_summary,
  aes(
    x = meanVal,
    y = mean_rt,
    color = choice_type,
    group = choice_type
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
      "Exaggerated" = "red",
      "Regressed" = "blue"
    )
  ) +
  labs(
    title = "Reaction Time by Mean Value and Choice Type",
    x = "Objective mean value",
    y = "Mean RT (ms)",
    color = "Choice type"
  ) +
  theme_classic(base_size = 14)

# =========================================================
# Plot 1: Proportion of exaggerated choices by extremity
# =========================================================

extremity_choice_summary <- all_data %>%
  group_by(extremity) %>%
  summarise(
    n = n(),
    n_exaggerated = sum(exaggerated_choice == 1, na.rm = TRUE),
    prop_exaggerated = mean(exaggerated_choice, na.rm = TRUE),
    se = sqrt((prop_exaggerated * (1 - prop_exaggerated)) / n),
    .groups = "drop"
  )

ggplot(
  extremity_choice_summary,
  aes(x = extremity, y = prop_exaggerated)
) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(
      ymin = prop_exaggerated - se,
      ymax = prop_exaggerated + se
    ),
    width = 0.3
  ) +
  labs(
    title = "Proportion of Exaggerated Choices by Extremity",
    x = "Extremity from midpoint |meanVal - 25|",
    y = "Proportion exaggerated choice"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 14)


# =========================================================
# Plot 2: Mean RT by extremity
# =========================================================

extremity_rt_summary <- all_data %>%
  group_by(extremity, choice_type) %>%
  summarise(
    n = n(),
    mean_rt = mean(rt, na.rm = TRUE),
    se_rt = sd(rt, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

ggplot(
  extremity_rt_summary,
  aes(
    x = extremity,
    y = mean_rt,
    color = choice_type,
    group = choice_type
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
      "Exaggerated" = "red",
      "Regressed" = "blue"
    )
  ) +
  labs(
    title = "Mean RT by Extremity and Choice Type",
    x = "Extremity from midpoint |meanVal - 25|",
    y = "Mean RT (ms)",
    color = "Choice type"
  ) +
  theme_classic(base_size = 14)
