library(dplyr)
library(readr)
library(lmerTest)
library(emmeans)
library(ggeffects)
library(ggplot2)

run_rtm_condition_only <- function(avg_path, exp_path, out_prefix = "Experiment") {
  
  avg_dat <- read_csv(avg_path, col_types = cols()) %>% mutate(condition = "average")
  exp_dat <- read_csv(exp_path, col_types = cols()) %>% mutate(condition = "experience")
  
  dat <- bind_rows(avg_dat, exp_dat) %>%
    select(participant_id, meanVal, indexSelected, condition) %>%
    filter(!is.na(meanVal), !is.na(indexSelected)) %>%
    mutate(
      participant_id = factor(participant_id),
      condition = factor(condition, levels = c("average", "experience"))
    )
  
  # Descriptives
  descriptives <- dat %>%
    count(condition, participant_id, name = "n_trials") %>%
    group_by(condition) %>%
    summarise(
      n_participants = n_distinct(participant_id),
      mean_trials = round(mean(n_trials), 1),
      .groups = "drop"
    )
  print(descriptives)
  
  # Main model: condition only
  model <- lmer(
    meanVal ~ indexSelected * condition + (1 + indexSelected | participant_id),
    data = dat
  )
  
  cat("\n--- MODEL SUMMARY:", out_prefix, "---\n")
  print(summary(model))
  
  # Condition-specific slopes + RTM test vs slope=1
  slopes <- emtrends(model, ~ condition, var = "indexSelected")
  cat("\n--- Slopes by condition:", out_prefix, "---\n")
  print(slopes)
  
  cat("\n--- Test slopes vs 1 (identity):", out_prefix, "---\n")
  print(test(slopes, null = 1))
  
  # Predicted lines by condition
  preds <- ggpredict(model, terms = c("indexSelected", "condition"))
  
  p <- plot(preds, show_ci = TRUE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) +
    labs(
      title = paste0("RTM by Condition (", out_prefix, ")"),
      subtitle = paste0(
        "Avg condition: N=", descriptives$n_participants[descriptives$condition=="average"],
        ", mean trials=", descriptives$mean_trials[descriptives$condition=="average"],
        " | Experience: N=", descriptives$n_participants[descriptives$condition=="experience"],
        ", mean trials=", descriptives$mean_trials[descriptives$condition=="experience"]
      ),
      x = "Reported value (indexSelected)",
      y = "Predicted true mean (meanVal)"
    ) +
    theme_minimal()
  
  print(p)
  
  list(data = dat, descriptives = descriptives, model = model, slopes = slopes, predictions = preds, plot = p)
}
# -----------------------------
# Run for each experiment (EDIT PATHS)
# -----------------------------

# Example paths (change these)
res_exp1 <- run_rtm_condition_only(
  avg_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/avg_datapilot1/data_cleaned.csv",
  exp_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/exp_datapilot1/data_cleaned.csv",
  out_prefix = "Experiment 1"
)

res_exp2 <- run_rtm_condition_only(
  avg_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/colorblocksavggraternoiselevels/data_cleaned.csv",
  exp_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/colorsquersexpgreaternoise/data_cleaned.csv",
  out_prefix = "Experiment 2"
)

res_exp3 <- run_rtm_condition_only(
  avg_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/grater_noise_avg2/data_cleaned.csv",
  exp_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/greater_noise_exp2/data_cleaned.csv",
  out_prefix = "Experiment 3"
)
res_exp4 <- run_rtm_condition_only(
  avg_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/mondrian_avg/data_cleaned.csv",
  exp_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/greater_noise_exp2/data_cleaned.csv",
  out_prefix = "Experiment 4"
)
library(dplyr)
library(purrr)
library(emmeans)
library(knitr)

# Vectorized APA p formatter
fmt_p <- function(p) {
  out <- rep("", length(p))
  out[is.na(p)] <- ""
  out[!is.na(p) & p < .001] <- "< .001"
  out[!is.na(p) & p >= .001] <- sprintf("%.3f", p[!is.na(p) & p >= .001]) |>
    sub("^0", "", x = _)
  out
}

fmt_ci <- function(lcl, ucl, digits = 3) {
  paste0("[", round(lcl, digits), ", ", round(ucl, digits), "]")
}

extract_rtm_rowwise <- function(res, exp_label = "Exp") {
  # 1) Descriptives
  desc <- res$descriptives %>%
    mutate(
      condition = as.character(condition),
      N = n_participants,
      MeanTrials = mean_trials
    ) %>%
    select(condition, N, MeanTrials)
  
  # 2) Slopes + CI
  slopes_df <- as.data.frame(res$slopes) %>%
    transmute(
      condition = as.character(condition),
      Slope_b = round(indexSelected.trend, 3),
      CI95 = fmt_ci(asymp.LCL, asymp.UCL)
    )
  
  # 3) Test slopes vs 1
  p_vs1_df <- as.data.frame(test(res$slopes, null = 1)) %>%
    transmute(
      condition = as.character(condition),
      p_vs_1 = fmt_p(p.value)
    )
  
  # 4) Interaction p-value
  coef_tab <- summary(res$model)$coefficients
  inter_name <- "indexSelected:conditionexperience"
  inter_p <- if (inter_name %in% rownames(coef_tab)) coef_tab[inter_name, "Pr(>|t|)"] else NA_real_
  inter_p_fmt <- fmt_p(inter_p)
  
  # 5) Merge per condition
  out <- desc %>%
    left_join(slopes_df, by = "condition") %>%
    left_join(p_vs1_df, by = "condition") %>%
    mutate(
      Experiment = exp_label,
      Condition = condition,
      Interaction_p = ifelse(condition == "experience", inter_p_fmt, "")
    ) %>%
    select(Experiment, Condition, N, MeanTrials, Slope_b, CI95, p_vs_1, Interaction_p)
  
  out
}

# ---- Build across experiments (edit names as needed) ----
res_list <- list(
  "Exp. 1" = res_exp1,
  "Exp. 2" = res_exp2,
  "Exp. 3" = res_exp3,
  "Exp. 4" = res_exp4
)

rtm_table <- imap_dfr(res_list, extract_rtm_rowwise)

rtm_table
