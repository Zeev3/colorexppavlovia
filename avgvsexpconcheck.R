library(dplyr)
library(readr)
library(lmerTest)
library(ggeffects)
library(emmeans)
library(ggplot2)

# -----------------------------
# Helper: run full analysis for 1 experiment
# -----------------------------
run_rtm_condition_analysis <- function(avg_path, exp_path, out_prefix = "exp") {
  
  # 1) Read both datasets and tag condition
  avg_dat <- read_csv(avg_path, col_types = cols()) %>% mutate(condition = "average")
  exp_dat <- read_csv(exp_path, col_types = cols()) %>% mutate(condition = "experience")
  
  dat <- bind_rows(avg_dat, exp_dat) %>%
    select(participant_id, trial, fixationTime, meanVal, indexSelected, array_length, color, condition) %>%
    filter(!is.na(meanVal), !is.na(indexSelected)) %>%
    mutate(
      participant_id = factor(participant_id),
      condition = factor(condition, levels = c("average", "experience")),
      array_length = factor(array_length),
      color = factor(color)
    )
  
  # Optional: collapse across red/blue (keep both but treated as part of the dataset)
  # If you want to control for color, add "+ color" to the model below.
  
  # 2) Basic descriptives: N and average trials per participant (per condition)
  descriptives <- dat %>%
    count(condition, participant_id, name = "n_trials") %>%
    group_by(condition) %>%
    summarise(
      n_participants = n_distinct(participant_id),
      mean_trials = round(mean(n_trials), 1),
      .groups = "drop"
    )
  
  print(descriptives)
  
  # 3) Main model: does RTM differ between conditions?
  #    indexSelected:condition is the key interaction
  model <- lmer(
    indexSelected ~ meanVal * condition *
      factor(array_length) +
      (1 +  meanVal*
         factor(array_length) | participant_id),
    data = dat
  )
  
  cat("\n\n--- MODEL SUMMARY:", out_prefix, "---\n")
  print(summary(model))
  
  # 4) Condition-specific slopes + test vs slope = 1 (identity line)
  slopes <- emtrends(model, ~ condition, var = "meanVal")
  cat("\n\n--- Condition slopes (emtrends):", out_prefix, "---\n")
  print(slopes)
  
  cat("\n\n--- Test slopes vs 1 (RTM test):", out_prefix, "---\n")
  print(test(slopes, null = 1))
  
  # 5) Predictions plot (by condition & array_length)
  preds <- ggpredict(model, terms = c("meanVal", "condition", "array_length"))
  
  p <- plot(preds, show_ci = TRUE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) +
    labs(
      title = paste0("Regression to the Mean by Condition (", out_prefix, ")"),
      x = "Predicted true mean (meanVal)",
      y = "Reported value (indexSelected)"
    ) +
    theme_minimal()
  
  print(p)
  
  # 6) Return everything (so you can save/compare later)
  list(
    data = dat,
    descriptives = descriptives,
    model = model,
    slopes = slopes,
    predictions = preds,
    plot = p
  )
}

# -----------------------------
# Run for each experiment (EDIT PATHS)
# -----------------------------

# Example paths (change these)
res_exp1 <- run_rtm_condition_analysis(
  avg_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/avg_datapilot1/data_cleaned.csv",
  exp_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/exp_datapilot1/data_cleaned.csv",
  out_prefix = "Experiment 1"
)

res_exp2 <- run_rtm_condition_analysis(
  avg_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/colorblocksavggraternoiselevels/data_cleaned.csv",
  exp_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/colorsquersexpgreaternoise/data_cleaned.csv",
  out_prefix = "Experiment 2"
)

res_exp3 <- run_rtm_condition_analysis(
  avg_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/grater_noise_avg2/data_cleaned.csv",
  exp_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/greater_noise_exp2/data_cleaned.csv",
  out_prefix = "Experiment 3"
)
res_exp4 <- run_rtm_condition_analysis(
  avg_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/mondrian_avg/data_cleaned.csv",
  exp_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/greater_noise_exp2/data_cleaned.csv",
  out_prefix = "Experiment 4"
)
res_exp4 <- run_rtm_condition_analysis(
  avg_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/variance_control_avg/data_cleaned.csv",
  exp_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/variance_control_exp/data_cleaned.csv",
  out_prefix = "Experiment 5"
)
