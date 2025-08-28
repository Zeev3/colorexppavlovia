# Load necessary libraries
library(tidyverse)

# Load data
setwd("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/avg_data")
df <- read.csv("data_cleaned.csv") # Replace with your actual path


# Create absolute error column
df <- df %>%
  mutate(abs_error = abs(meanVal - indexSelected))

# Create summary table per participant per array_length
summary_by_participant <- df %>%
  group_by(participant_id, array_length) %>%
  summarise(
    avg_rt = mean(rt, na.rm = TRUE),
    median_rt = median(rt, na.rm = TRUE),
    corr = cor(meanVal, indexSelected, use = "complete.obs"),
    .groups = "drop"
  )
overall_corrs <- df %>%
  group_by(participant_id) %>%
  summarise(overall_corr = cor(meanVal, indexSelected, use = "complete.obs"))

# Define unique array lengths
array_levels <- sort(unique(df$array_length))

# Plotting function
plot_and_save <- function(data, xvar, title_prefix, xlab, folder = NULL) {
  for (al in array_levels) {
    p <- ggplot(filter(data, array_length == al), aes_string(x = xvar)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
      labs(
        title = paste0(title_prefix, " (Array Length = ", al, ")"),
        x = xlab,
        y = "Count"
      ) +
      theme_minimal()
    
    print(p)  # Show the plot
    
    if (!is.null(folder)) {
      ggsave(
        filename = paste0(folder, "/", title_prefix, "_array_", al, ".png"),
        plot = p,
        width = 8, height = 5
      )
    }
  }
}

# Create plots
plot_and_save(df, "rt", "RT per Trial", "RT")
plot_and_save(summary_by_participant, "avg_rt", "Average RT per Participant", "Average RT")
plot_and_save(summary_by_participant, "median_rt", "Median RT per Participant", "Median RT")
plot_and_save(summary_by_participant, "corr", "Correlation per Participant", "Correlation (meanVal ~ indexSelected)")
plot_and_save(df, "abs_error", "Absolute Error per Trial", "|meanVal - indexSelected|")
ggplot(overall_corrs, aes(x = overall_corr)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(
    title = "Overall Correlation per Participant",
    x = "Correlation (meanVal vs indexSelected)",
    y = "Participant Count"
  ) +
  theme_minimal()
