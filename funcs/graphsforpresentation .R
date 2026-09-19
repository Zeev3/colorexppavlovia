# =========================================================
# PLOTTING SECTION
# =========================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggeffects)
library(patchwork)

# =========================================================
# HELPERS
# =========================================================

condition_title <- function(condition_name) {
  ifelse(
    condition_name == "average",
    "Average condition",
    "Experience condition"
  )
}

get_group_label <- function(results_obj) {
  
  ifelse(
    results_obj$grouping_var == "array_length",
    "Array size",
    "Variance level"
  )
}

make_line_data <- function(regression_lines) {
  
  x_seq <- seq(0, 50, length.out = 200)
  
  regression_lines %>%
    tidyr::crossing(meanVal = x_seq) %>%
    mutate(
      predicted = mean_intercept + mean_slope * meanVal
    )
}
# =========================================================
# LABEL HELPERS
# =========================================================

pretty_group_labels <- function(values, grouping_var) {
  
  if (grouping_var == "array_length") {
    
    paste0(values, " squares")
    
  } else {
    
    paste0("SD of ", values)
  }
}

# =========================================================
# 1. PARTICIPANT-AVERAGE REGRESSION PLOTS
# =========================================================

plot_avg_regression <- function(results_obj,
                                condition_name = "experience") {
  
  grouping_var <- results_obj$grouping_var
  
  line_df <- results_obj[[condition_name]]$average_regression_lines %>%
    make_line_data()
  
  ggplot(
    line_df,
    aes(
      x = meanVal,
      y = predicted,
      color = .data[[grouping_var]]
    )
  ) +
    
    geom_line(linewidth = 1.2) +
    
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    
    coord_cartesian(
      xlim = c(0, 50),
      ylim = c(0, 50)
    ) +
    
    labs(
      title = paste0(
        results_obj$experiment,
        " - ",
        condition_title(condition_name)
      ),
      subtitle = "Average participant regression line",
      x = "Objective mean",
      y = "Subjective response",
      color = get_group_label(results_obj)
    ) +
    scale_color_discrete(
      labels = function(x)
        pretty_group_labels(x, grouping_var)
    ) +
    
    theme_classic(base_size = 13)
}
# =========================================================
# REVERSED PARTICIPANT-AVERAGE REGRESSION PLOTS
# x = subjective response
# y = objective mean
# =========================================================

plot_avg_regression_reversed <- function(results_obj,
                                         condition_name = "experience") {
  
  grouping_var <- results_obj$grouping_var
  
  line_df <- results_obj[[condition_name]]$average_regression_lines
  
  x_seq <- seq(0, 50, length.out = 200)
  
  reversed_df <- line_df %>%
    tidyr::crossing(subjective_response = x_seq) %>%
    mutate(
      objective_mean = (subjective_response - mean_intercept) / mean_slope
    )
  
  ggplot(
    reversed_df,
    aes(
      x = subjective_response,
      y = objective_mean,
      color = .data[[grouping_var]]
    )
  ) +
    geom_line(linewidth = 1.2) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    coord_cartesian(xlim = c(0, 50), ylim = c(0, 50)) +
    labs(
      title = paste0(
        results_obj$experiment,
        " - ",
        condition_title(condition_name)
      ),
      subtitle = "Reversed average participant regression line",
      x = "Subjective response",
      y = "Objective mean",
      color = get_group_label(results_obj)
    ) +
    scale_color_discrete(
      labels = function(x)
        pretty_group_labels(x, grouping_var)
    ) +
    
    scale_fill_discrete(
      labels = function(x)
        pretty_group_labels(x, grouping_var)
    ) +
    theme_classic(base_size = 13)
}
# =========================================================
# 2. MODEL-BASED REGRESSION PLOTS
# =========================================================

plot_model_regression <- function(results_obj,
                                  condition_name = "experience") {
  
  grouping_var <- results_obj$grouping_var
  
  model <- results_obj[[condition_name]]$working_model
  dat <- results_obj[[condition_name]]$data
  
  preds <- ggpredict(
    model,
    terms = c("meanVal_c [all]", grouping_var)
  )
  
  grand_mean <- mean(dat$meanVal, na.rm = TRUE)
  
  preds <- preds %>%
    as.data.frame() %>%
    mutate(
      meanVal_raw = x + grand_mean,
      group = factor(group)
    )
  
  ggplot(
    preds,
    aes(
      x = meanVal_raw,
      y = predicted,
      color = group,
      fill = group
    )
  ) +
    
    geom_ribbon(
      aes(
        ymin = conf.low,
        ymax = conf.high
      ),
      alpha = 0.15,
      color = NA
    ) +
    
    geom_line(linewidth = 1.2) +
    
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    
    coord_cartesian(
      xlim = c(0, 50),
      ylim = c(0, 50)
    ) +
    
    labs(
      title = paste0(
        results_obj$experiment,
        " - ",
        condition_title(condition_name)
      ),
      subtitle = "Model-based regression line",
      x = "Objective mean",
      y = "Subjective response",
      color = get_group_label(results_obj),
      fill = get_group_label(results_obj)
    ) +
    scale_color_discrete(
      labels = function(x)
        pretty_group_labels(x, grouping_var)
    ) +
    
    scale_fill_discrete(
      labels = function(x)
        pretty_group_labels(x, grouping_var)
    ) +
    
    theme_classic(base_size = 13)
}

# =========================================================
# 3. GALTON / REVERSE GALTON SQUEEZE
# =========================================================

make_galton_data <- function(results_obj,
                             condition_name = "experience",
                             reverse = TRUE,
                             collapse_group = TRUE) {
  
  dat <- results_obj[[condition_name]]$data
  grouping_var <- results_obj$grouping_var
  
  if (collapse_group) {
    
    if (reverse) {
      
      dat %>%
        group_by(indexSelected) %>%
        summarise(
          mean_true = mean(meanVal, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        rename(response_value = indexSelected)
      
    } else {
      
      dat %>%
        group_by(meanVal) %>%
        summarise(
          mean_response = mean(indexSelected, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
  } else {
    
    if (reverse) {
      
      dat %>%
        group_by(indexSelected, .data[[grouping_var]]) %>%
        summarise(
          mean_true = mean(meanVal, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        rename(response_value = indexSelected)
      
    } else {
      
      dat %>%
        group_by(meanVal, .data[[grouping_var]]) %>%
        summarise(
          mean_response = mean(indexSelected, na.rm = TRUE),
          .groups = "drop"
        )
    }
  }
}

plot_galton_squeeze <- function(results_obj,
                                condition_name = "experience",
                                reverse = TRUE,
                                collapse_group = TRUE,
                                experiment_title = NULL) {
  
  grouping_var <- results_obj$grouping_var
  
  galton_data <- make_galton_data(
    results_obj,
    condition_name = condition_name,
    reverse = reverse,
    collapse_group = collapse_group
  )
  
  tick_df <- data.frame(
    tick = seq(10, 40, by = 5)
  )
  
  if (reverse) {
    
    p <- ggplot(galton_data) +
      
      geom_segment(
        aes(
          x = 1,
          xend = 2,
          y = response_value,
          yend = mean_true
        ),
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
        size = 1.5,
        alpha = 0.9,
        color = "black"
      ) +
      
      scale_x_continuous(
        breaks = c(1, 2),
        labels = c("Response", "Mean objective"),
        expand = expansion(mult = c(0.45, 0.45))
      )
    
  } else {
    
    p <- ggplot(galton_data) +
      
      geom_segment(
        aes(
          x = 1,
          xend = 2,
          y = meanVal,
          yend = mean_response
        ),
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
        size = 1.5,
        alpha = 0.9,
        color = "firebrick"
      ) +
      
      scale_x_continuous(
        breaks = c(1, 2),
        labels = c("Objective mean", "Mean response"),
        expand = expansion(mult = c(0.45, 0.45))
      )
  }
  
  p <- p +
    
    geom_vline(
      xintercept = c(1, 2),
      linewidth = 0.5
    ) +
    
    geom_segment(
      data = tick_df,
      aes(
        x = 0.985,
        xend = 1,
        y = tick,
        yend = tick
      ),
      inherit.aes = FALSE,
      linewidth = 0.35
    ) +
    
    geom_segment(
      data = tick_df,
      aes(
        x = 2,
        xend = 2.015,
        y = tick,
        yend = tick
      ),
      inherit.aes = FALSE,
      linewidth = 0.35
    ) +
    
    geom_text(
      data = tick_df,
      aes(
        x = 0.96,
        y = tick,
        label = tick
      ),
      inherit.aes = FALSE,
      hjust = 1,
      size = 2.7
    ) +
    
    geom_text(
      data = tick_df,
      aes(
        x = 2.04,
        y = tick,
        label = tick
      ),
      inherit.aes = FALSE,
      hjust = 0,
      size = 2.7
    ) +
    
    coord_cartesian(
      ylim = c(0, 50),
      clip = "off"
    ) +
    
    labs(
      title = paste0(
        experiment_title,
        " - ",
        condition_title(condition_name)
      ),
      subtitle = ifelse(
        reverse,
        "Reverse Galton squeeze",
        "Galton squeeze"
      ),
      x = NULL,
      y = NULL
    ) +
    
    theme_classic(base_size = 14) +
    
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.margin = margin(
        t = 15,
        r = 40,
        b = 25,
        l = 40
      )
    )
  
  if (!collapse_group) {
    
    p <- p +
      facet_wrap(
        as.formula(paste0("~", grouping_var)),
        nrow = 1,
        labeller = labeller(
          .cols = function(x)
            pretty_group_labels(x, grouping_var)
        )
      ) +
      theme(
        strip.background = element_rect(
          fill = "white",
          color = "black"
        ),
        strip.text = element_text(face = "bold"),
        panel.spacing = unit(2, "lines")
      )
  }
  
  p
}

# =========================================================
# FIGURES
# =========================================================

# =========================================================
# EXPERIMENT 1
# =========================================================

# -----------------------------
# Participant-average regression
# -----------------------------

fig_exp1_average_participant <-
  plot_avg_regression(exp1_results, "average")

fig_exp1_experience_participant <-
  plot_avg_regression(exp1_results, "experience")

# -----------------------------
# Model regression
# -----------------------------

fig_exp1_average_model <-
  plot_model_regression(exp1_results, "average")

fig_exp1_experience_model <-
  plot_model_regression(exp1_results, "experience")

# -----------------------------
# Reverse Galton squeeze
# -----------------------------

fig_exp1_average_reverse_galton <-
  plot_galton_squeeze(
    exp1_results,
    condition_name = "average",
    reverse = TRUE,
    collapse_group = TRUE,
    experiment_title = "Experiment 1"
  )

fig_exp1_experience_reverse_galton <-
  plot_galton_squeeze(
    exp1_results,
    condition_name = "experience",
    reverse = TRUE,
    collapse_group = TRUE,
    experiment_title = "Experiment 1"
  )

# =========================================================
# EXPERIMENT 2
# =========================================================

# -----------------------------
# Participant-average regression
# -----------------------------

fig_exp2_average_participant <-
  plot_avg_regression(exp2_results, "average")

fig_exp2_experience_participant <-
  plot_avg_regression(exp2_results, "experience")

# -----------------------------
# Model regression
# -----------------------------

fig_exp2_average_model <-
  plot_model_regression(exp2_results, "average")

fig_exp2_experience_model <-
  plot_model_regression(exp2_results, "experience")

# -----------------------------
# Reverse Galton squeeze
# -----------------------------

fig_exp2_average_reverse_galton <-
  plot_galton_squeeze(
    exp2_results,
    condition_name = "average",
    reverse = TRUE,
    collapse_group = TRUE,
    experiment_title = "Experiment 2"
  )

fig_exp2_experience_reverse_galton <-
  plot_galton_squeeze(
    exp2_results,
    condition_name = "experience",
    reverse = TRUE,
    collapse_group = TRUE,
    experiment_title = "Experiment 2"
  )
# -----------------------------
# EXP 3
# -----------------------------

fig_exp3_average_participant <-
  plot_avg_regression(exp3_results, "average")

fig_exp3_experience_participant <-
  plot_avg_regression(exp3_results, "experience")

fig_exp3_average_model <-
  plot_model_regression(exp3_results, "average")

fig_exp3_experience_model <-
  plot_model_regression(exp3_results, "experience")

fig_exp3_average_reverse_galton <-
  plot_galton_squeeze(
    exp3_results,
    condition_name = "average",
    reverse = TRUE,
    collapse_group = TRUE,
    experiment_title = "Experiment 3"
  )

fig_exp3_experience_reverse_galton <-
  plot_galton_squeeze(
    exp3_results,
    condition_name = "experience",
    reverse = TRUE,
    collapse_group = TRUE,
    experiment_title = "Experiment 3"
  )

# -----------------------------
# EXP 4
# -----------------------------

fig_exp4_average_participant <-
  plot_avg_regression(exp4_results, "average")

fig_exp4_experience_participant <-
  plot_avg_regression(exp4_results, "experience")

fig_exp4_average_model <-
  plot_model_regression(exp4_results, "average")

fig_exp4_experience_model <-
  plot_model_regression(exp4_results, "experience")

fig_exp4_average_reverse_galton <-
  plot_galton_squeeze(
    exp4_results,
    condition_name = "average",
    reverse = TRUE,
    collapse_group = FALSE,
    experiment_title = "Experiment 4"
  )

fig_exp4_experience_reverse_galton <-
  plot_galton_squeeze(
    exp4_results,
    condition_name = "experience",
    reverse = TRUE,
    collapse_group = FALSE,
    experiment_title = "Experiment 4"
  )
# =========================================================
# PRINT PLOTS
# =========================================================

fig_exp1_average_participant
fig_exp1_experience_participant

fig_exp1_average_model
fig_exp1_experience_model

fig_exp1_average_reverse_galton
fig_exp1_experience_reverse_galton

fig_exp2_average_participant
fig_exp2_experience_participant

fig_exp2_average_model
fig_exp2_experience_model

fig_exp2_average_reverse_galton
fig_exp2_experience_reverse_galton

fig_exp3_average_participant
fig_exp3_experience_participant

fig_exp3_average_model
fig_exp3_experience_model

fig_exp3_average_reverse_galton
fig_exp3_experience_reverse_galton

fig_exp4_average_participant
fig_exp4_experience_participant

fig_exp4_average_model
fig_exp4_experience_model

fig_exp4_average_reverse_galton
fig_exp4_experience_reverse_galton

# Experiment 1
fig_exp1_average_participant_reversed <-
  plot_avg_regression_reversed(exp1_results, "average")

fig_exp1_experience_participant_reversed <-
  plot_avg_regression_reversed(exp1_results, "experience")

# Experiment 2
fig_exp2_average_participant_reversed <-
  plot_avg_regression_reversed(exp2_results, "average")

fig_exp2_experience_participant_reversed <-
  plot_avg_regression_reversed(exp2_results, "experience")

# Experiment 3
fig_exp3_average_participant_reversed <-
  plot_avg_regression_reversed(exp3_results, "average")

fig_exp3_experience_participant_reversed <-
  plot_avg_regression_reversed(exp3_results, "experience")

# Experiment 4
fig_exp4_average_participant_reversed <-
  plot_avg_regression_reversed(exp4_results, "average")

fig_exp4_experience_participant_reversed <-
  plot_avg_regression_reversed(exp4_results, "experience")
fig_exp1_average_participant_reversed
fig_exp1_experience_participant_reversed

fig_exp2_average_participant_reversed
fig_exp2_experience_participant_reversed

fig_exp3_average_participant_reversed
fig_exp3_experience_participant_reversed

fig_exp4_average_participant_reversed
fig_exp4_experience_participant_reversed
