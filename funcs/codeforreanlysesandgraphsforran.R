# =========================================================
# SINGLE-CONDITION ANALYSIS RUNNER
# =========================================================

run_single_condition_analysis <- function(
    path,
    experiment_name,
    condition_label,
    grouping_var = c("array_length", "variance_level"),
    variance_mapping = FALSE
) {
  
  grouping_var <- match.arg(grouping_var)
  
  dat <- read.csv(path) %>%
    select(participant_id, trial, fixationTime, meanVal,
           indexSelected, array_length, color) %>%
    na.omit() %>%
    mutate(
      experiment = experiment_name,
      condition = condition_label,
      participant_id = factor(participant_id)
    )
  
  if (variance_mapping) {
    dat <- dat %>%
      mutate(
        variance_level = case_when(
          array_length == 2 ~ 3,
          array_length == 6 ~ 5,
          array_length == 8 ~ 7,
          TRUE ~ NA_real_
        ),
        variance_level = factor(variance_level, levels = c(3, 5, 7))
      ) %>%
      filter(!is.na(variance_level))
  } else {
    dat <- dat %>%
      mutate(
        array_length = factor(
          array_length,
          levels = sort(unique(array_length))
        )
      ) %>%
      filter(!is.na(array_length))
  }
  
  dat <- dat %>%
    group_by(participant_id) %>%
    mutate(meanVal_c = meanVal - mean(meanVal, na.rm = TRUE)) %>%
    ungroup()
  
  group_formula <- grouping_var
  group_sym <- rlang::sym(group_formula)
  
  # =====================================================
  # LMM candidates
  # =====================================================
  
  full_formula <- as.formula(
    paste0(
      "indexSelected ~ meanVal_c * ", group_formula, " + ",
      "(1 + meanVal_c * ", group_formula, " | participant_id)"
    )
  )
  
  simpler_formula_1 <- as.formula(
    paste0(
      "indexSelected ~ meanVal_c * ", group_formula, " + ",
      "(1 + meanVal_c + ", group_formula, " | participant_id)"
    )
  )
  
  simpler_formula_2 <- as.formula(
    paste0(
      "indexSelected ~ meanVal_c * ", group_formula, " + ",
      "(1 + meanVal_c | participant_id)"
    )
  )
  
  simpler_formula_3 <- as.formula(
    paste0(
      "indexSelected ~ meanVal_c * ", group_formula, " + ",
      "(1 + ", group_formula, " | participant_id)"
    )
  )
  
  model_list <- list(
    full = tryCatch(lmer(full_formula, data = dat), error = function(e) NULL),
    simpler_mean_group = tryCatch(lmer(simpler_formula_1, data = dat), error = function(e) NULL),
    simpler_mean = tryCatch(lmer(simpler_formula_2, data = dat), error = function(e) NULL),
    simpler_group = tryCatch(lmer(simpler_formula_3, data = dat), error = function(e) NULL)
  )
  
  convergence_info <- tibble(
    model_name = names(model_list),
    exists = !sapply(model_list, is.null),
    singular = sapply(model_list, function(m) {
      if (is.null(m)) NA else isSingular(m)
    }),
    convergence_code = sapply(model_list, function(m) {
      if (is.null(m)) return(NA)
      m@optinfo$conv$opt
    }),
    convergence_message = sapply(model_list, function(m) {
      if (is.null(m)) return(NA)
      msg <- m@optinfo$conv$lme4$messages
      if (is.null(msg)) NA else paste(msg, collapse = "; ")
    })
  )
  
  valid_models <- model_list[!sapply(model_list, is.null)]
  
  converged <- valid_models[sapply(valid_models, function(m) {
    identical(m@optinfo$conv$opt, 0L) &&
      is.null(m@optinfo$conv$lme4$messages)
  })]
  
  if (length(converged) > 0) {
    working_model <- converged[[1]]
    working_model_name <- names(converged)[1]
  } else {
    working_model <- valid_models[[1]]
    working_model_name <- names(valid_models)[1]
  }
  
  # =====================================================
  # Model summaries
  # =====================================================
  
  model_anova <- anova(working_model)
  
  slopes <- emtrends(
    working_model,
    as.formula(paste0("~ ", group_formula)),
    var = "meanVal_c"
  )
  
  slopes_tests <- pairs(slopes)
  
  model_predictions <- ggpredict(
    working_model,
    terms = c("meanVal_c [all]", group_formula)
  )
  
  # =====================================================
  # Correlation analysis
  # =====================================================
  
  corr_data <- dat %>%
    group_by(participant_id, !!group_sym) %>%
    summarise(
      r = ifelse(
        n() > 2 && sd(meanVal) > 0 && sd(indexSelected) > 0,
        cor(meanVal, indexSelected, use = "complete.obs"),
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    mutate(z = atanh(r))
  
  corr_aov <- aov_ez(
    id = "participant_id",
    dv = "z",
    data = corr_data,
    within = group_formula,
    type = 3
  )
  
  corr_summary <- corr_data %>%
    group_by(!!group_sym) %>%
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
  
  corr_pairs <- emmeans(
    corr_aov,
    as.formula(paste0("~ ", group_formula))
  ) %>%
    pairs()
  
  # =====================================================
  # Exaggeration analysis
  # =====================================================
  
  dat_exagg <- dat %>%
    mutate(
      exaggerated = case_when(
        meanVal < 20 & indexSelected < 14 ~ 1,
        meanVal > 30 & indexSelected > 36 ~ 1,
        TRUE ~ 0
      )
    )
  
  exagg_data <- dat_exagg %>%
    group_by(participant_id, !!group_sym) %>%
    summarise(
      prop_exagg = mean(exaggerated, na.rm = TRUE),
      n_trials = n(),
      .groups = "drop"
    )
  
  exagg_aov <- aov_ez(
    id = "participant_id",
    dv = "prop_exagg",
    data = exagg_data,
    within = group_formula,
    type = 3
  )
  
  exagg_pairs <- emmeans(
    exagg_aov,
    as.formula(paste0("~ ", group_formula))
  ) %>%
    pairs()
  
  # =====================================================
  # Participant-level regression lines
  # =====================================================
  
  participant_regressions <- dat %>%
    group_by(participant_id, !!group_sym) %>%
    summarise(
      intercept = coef(lm(indexSelected ~ meanVal))[1],
      slope = coef(lm(indexSelected ~ meanVal))[2],
      .groups = "drop"
    )
  
  average_regression_lines <- participant_regressions %>%
    group_by(!!group_sym) %>%
    summarise(
      mean_intercept = mean(intercept, na.rm = TRUE),
      mean_slope = mean(slope, na.rm = TRUE),
      se_slope = sd(slope, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  return(list(
    experiment = experiment_name,
    condition = condition_label,
    grouping_var = group_formula,
    data = dat,
    
    models = model_list,
    working_model = working_model,
    working_model_name = working_model_name,
    convergence_info = convergence_info,
    model_anova = model_anova,
    slopes = slopes,
    slopes_tests = slopes_tests,
    model_predictions = model_predictions,
    
    corr_data = corr_data,
    corr_aov = corr_aov,
    corr_summary = corr_summary,
    corr_pairs = corr_pairs,
    
    exagg_data = exagg_data,
    exagg_aov = exagg_aov,
    exagg_pairs = exagg_pairs,
    
    participant_regressions = participant_regressions,
    average_regression_lines = average_regression_lines
  ))
}
run_full_experiment <- function(
    exp_path,
    avg_path,
    experiment_name,
    grouping_var = c("array_length", "variance_level"),
    variance_mapping = FALSE
) {
  
  grouping_var <- match.arg(grouping_var)
  
  combined_results <- run_experiment_analysis(
    exp_path = exp_path,
    avg_path = avg_path,
    experiment_name = experiment_name,
    grouping_var = grouping_var,
    variance_mapping = variance_mapping
  )
  
  average_results <- run_single_condition_analysis(
    path = avg_path,
    experiment_name = experiment_name,
    condition_label = "average",
    grouping_var = grouping_var,
    variance_mapping = variance_mapping
  )
  
  experience_results <- run_single_condition_analysis(
    path = exp_path,
    experiment_name = experiment_name,
    condition_label = "experience",
    grouping_var = grouping_var,
    variance_mapping = variance_mapping
  )
  
  return(list(
    experiment = experiment_name,
    grouping_var = grouping_var,
    combined = combined_results,
    average = average_results,
    experience = experience_results
  ))
}

library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)
library(afex)
library(emmeans)
library(ggeffects)
library(ggplot2)

run_experiment_analysis <- function(
    exp_path,
    avg_path,
    experiment_name,
    grouping_var = c("array_length", "variance_level"),
    variance_mapping = FALSE
) {
  
  grouping_var <- match.arg(grouping_var)
  
  prepare_condition_data <- function(path, condition_label) {
    read.csv(path) %>%
      select(participant_id, trial, fixationTime, meanVal,
             indexSelected, array_length, color) %>%
      mutate(condition = condition_label)
  }
  
  exp_dat <- prepare_condition_data(exp_path, "experience")
  avg_dat <- prepare_condition_data(avg_path, "average")
  
  dat <- bind_rows(avg_dat, exp_dat) %>%
    mutate(
      experiment = experiment_name,
      participant_id = interaction(condition, participant_id, drop = TRUE),
      participant_id = factor(participant_id),
      condition = factor(condition, levels = c("average", "experience")),
      condition_ec = ifelse(condition == "average", -1, 1)
    )
  
  if (variance_mapping) {
    dat <- dat %>%
      mutate(
        variance_level = case_when(
          array_length == 2 ~ 3,
          array_length == 6 ~ 5,
          array_length == 8 ~ 7,
          TRUE ~ NA_real_
        ),
        variance_level = factor(variance_level, levels = c(3, 5, 7))
      ) %>%
      filter(!is.na(variance_level))
  } else {
    dat <- dat %>%
      mutate(array_length = factor(array_length)) %>%
      filter(!is.na(array_length))
  }
  
  dat <- dat %>%
    group_by(participant_id) %>%
    mutate(meanVal_c = meanVal - mean(meanVal, na.rm = TRUE)) %>%
    ungroup()
  
  group_formula <- if (grouping_var == "array_length") {
    "array_length"
  } else {
    "variance_level"
  }
  
  # =====================================================
  # Combined LMM
  # =====================================================
  
  full_formula <- as.formula(
    paste0(
      "indexSelected ~ meanVal_c * ", group_formula, " * condition_ec + ",
      "(1 + meanVal_c * ", group_formula, " | participant_id)"
    )
  )
  
  simpler_formula_1 <- as.formula(
    paste0(
      "indexSelected ~ meanVal_c * ", group_formula, " * condition_ec + ",
      "(1 + meanVal_c + ", group_formula, " | participant_id)"
    )
  )
  
  simpler_formula_2 <- as.formula(
    paste0(
      "indexSelected ~ meanVal_c * ", group_formula, " * condition_ec + ",
      "(1 + meanVal_c | participant_id)"
    )
  )
  
  simpler_formula_3 <- as.formula(
    paste0(
      "indexSelected ~ meanVal_c * ", group_formula, " * condition_ec + ",
      "(1 + ", group_formula, " | participant_id)"
    )
  )
  
  model_list <- list(
    full = tryCatch(lmer(full_formula, data = dat), error = function(e) NULL),
    simpler_mean_group = tryCatch(lmer(simpler_formula_1, data = dat), error = function(e) NULL),
    simpler_mean = tryCatch(lmer(simpler_formula_2, data = dat), error = function(e) NULL),
    simpler_group = tryCatch(lmer(simpler_formula_3, data = dat), error = function(e) NULL)
  )
  
  convergence_info <- tibble(
    model_name = names(model_list),
    exists = !sapply(model_list, is.null),
    singular = sapply(model_list, function(m) if (is.null(m)) NA else isSingular(m)),
    convergence_code = sapply(model_list, function(m) {
      if (is.null(m)) return(NA)
      m@optinfo$conv$opt
    })
  )
  
  # Choose first model with convergence code 0.
  valid_models <- model_list[!sapply(model_list, is.null)]
  converged <- valid_models[sapply(valid_models, function(m) m@optinfo$conv$opt == 0)]
  
  if (length(converged) > 0) {
    working_model <- converged[[1]]
    working_model_name <- names(converged)[1]
  } else {
    working_model <- valid_models[[1]]
    working_model_name <- names(valid_models)[1]
  }
  
  model_anova <- anova(working_model)
  
  slopes <- emtrends(
    working_model,
    as.formula(paste0("~ condition_ec * ", group_formula)),
    var = "meanVal_c"
  )
  
  slopes_condition_tests <- emtrends(
    working_model,
    as.formula(paste0("pairwise ~ condition_ec | ", group_formula)),
    var = "meanVal_c"
  )
  
  slopes_group_tests <- emtrends(
    working_model,
    as.formula(paste0("pairwise ~ ", group_formula, " | condition_ec")),
    var = "meanVal_c"
  )
  
  model_predictions <- ggpredict(
    working_model,
    terms = c("meanVal_c [all]", group_formula, "condition_ec")
  )
  
  # =====================================================
  # Correlation analysis
  # =====================================================
  
  group_sym <- rlang::sym(group_formula)
  
  corr_data <- dat %>%
    group_by(participant_id, condition, !!group_sym) %>%
    summarise(
      r = ifelse(
        n() > 2 && sd(meanVal) > 0 && sd(indexSelected) > 0,
        cor(meanVal, indexSelected, use = "complete.obs"),
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    mutate(z = atanh(r))
  
  corr_aov <- aov_ez(
    id = "participant_id",
    dv = "z",
    data = corr_data,
    within = group_formula,
    between = "condition",
    type = 3
  )
  
  corr_summary <- corr_data %>%
    group_by(condition, !!group_sym) %>%
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
  
  corr_pairs_overall <- emmeans(corr_aov, as.formula(paste0("~ ", group_formula))) %>%
    pairs()
  
  corr_pairs_by_condition <- emmeans(
    corr_aov,
    as.formula(paste0("~ ", group_formula, " | condition"))
  ) %>%
    pairs()
  
  # =====================================================
  # Exaggeration analysis
  # =====================================================
  
  dat_exagg <- dat %>%
    mutate(
      exaggerated = case_when(
        meanVal < 20 & indexSelected < 14 ~ 1,
        meanVal > 30 & indexSelected > 36 ~ 1,
        TRUE ~ 0
      )
    )
  
  exagg_data <- dat_exagg %>%
    group_by(participant_id, condition, !!group_sym) %>%
    summarise(
      prop_exagg = mean(exaggerated, na.rm = TRUE),
      n_trials = n(),
      .groups = "drop"
    )
  
  exagg_aov <- aov_ez(
    id = "participant_id",
    dv = "prop_exagg",
    data = exagg_data,
    within = group_formula,
    between = "condition",
    type = 3
  )
  
  exagg_pairs_overall <- emmeans(
    exagg_aov,
    as.formula(paste0("~ ", group_formula))
  ) %>%
    pairs()
  
  exagg_pairs_by_condition <- emmeans(
    exagg_aov,
    as.formula(paste0("~ ", group_formula, " | condition"))
  ) %>%
    pairs()
  
  # =====================================================
  # Participant-average regression lines
  # =====================================================
  
  participant_regressions <- dat %>%
    group_by(participant_id, condition, !!group_sym) %>%
    summarise(
      intercept = coef(lm(indexSelected ~ meanVal))[1],
      slope = coef(lm(indexSelected ~ meanVal))[2],
      .groups = "drop"
    )
  
  average_regression_lines <- participant_regressions %>%
    group_by(condition, !!group_sym) %>%
    summarise(
      mean_intercept = mean(intercept, na.rm = TRUE),
      mean_slope = mean(slope, na.rm = TRUE),
      se_slope = sd(slope, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  return(list(
    experiment = experiment_name,
    grouping_var = group_formula,
    data = dat,
    
    models = model_list,
    working_model = working_model,
    working_model_name = working_model_name,
    convergence_info = convergence_info,
    model_anova = model_anova,
    slopes = slopes,
    slopes_condition_tests = slopes_condition_tests,
    slopes_group_tests = slopes_group_tests,
    model_predictions = model_predictions,
    
    corr_data = corr_data,
    corr_aov = corr_aov,
    corr_summary = corr_summary,
    corr_pairs_overall = corr_pairs_overall,
    corr_pairs_by_condition = corr_pairs_by_condition,
    
    exagg_data = exagg_data,
    exagg_aov = exagg_aov,
    exagg_pairs_overall = exagg_pairs_overall,
    exagg_pairs_by_condition = exagg_pairs_by_condition,
    
    participant_regressions = participant_regressions,
    average_regression_lines = average_regression_lines
  ))
}
exp2_results <- run_full_experiment(
  exp_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/greaternoise/colorsquersexpgreaternoise/data_cleaned.csv",
  avg_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/greaternoise/colorblocksavggraternoiselevels/data_cleaned.csv",
  experiment_name = "Experiment 2",
  grouping_var = "array_length",
  variance_mapping = FALSE
)

exp3_results <- run_full_experiment(
  exp_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/mondrian/mondrian_exp/data_cleaned.csv",
  avg_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/mondrian/mondrian_avg/data_cleaned.csv",
  experiment_name = "Experiment 3",
  grouping_var = "array_length",
  variance_mapping = FALSE
)

exp4_results <- run_full_experiment(
  exp_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/variancecontrol/variance_control_exp/data_cleaned.csv",
  avg_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/variancecontrol/variance_control_avg/data_cleaned.csv",
  experiment_name = "Experiment 4",
  grouping_var = "variance_level",
  variance_mapping = TRUE
)

exp1_results <- run_full_experiment(
  exp_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/exp_datapilot1/data_cleaned.csv",
  avg_path = "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/avg_datapilot1/data_cleaned.csv",
  experiment_name = "Experiment 1",
  grouping_var = "array_length",
  variance_mapping = FALSE
)
