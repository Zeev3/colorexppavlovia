library(tidyverse)

# Set working directory if needed
 setwd("/Users/zeevbenamos/Downloads/data-14")

# Read the CSV file - replace with file.choose() if you want to select it through a dialog
 data <- read.csv("colorexppavlovia_PARTICIPANT_SESSION_2025-03-03_14h20.03.147.csv")
 
# Filter for valid trials with meanVal and indexSelected
valid_data <- data %>%
  filter(!is.na(meanVal) & !is.na(indexSelected) & !is.na(array_length))

# Check the relevant array lengths
array_lengths <- unique(valid_data$array_length)
print(paste("Available array lengths:", paste(array_lengths, collapse = ", ")))

# Define colors for each array length in the combined plot
length_colors <- c("2" = "blue", "6" = "green", "8" = "red", "12" = "purple")

# Create separate plots for array lengths with sufficient data (2, 6, 8, 12)
relevant_lengths <- c(2, 6, 8, 12)

# Process each array length individually (collapsing red and blue trials)
for (length_val in relevant_lengths) {
  # Filter data for this array length
  length_data <- valid_data %>% filter(array_length == length_val)
  
  # Only create plot if we have enough data
  if (nrow(length_data) >= 3) {
    # Calculate correlation
    corr <- cor.test(length_data$indexSelected, length_data$meanVal)
    r_value <- round(corr$estimate, 3)
    p_value <- round(corr$p.value, 4)
    
    # Calculate regression line parameters
    model <- lm(meanVal ~ indexSelected, data = length_data)
    slope <- coefficients(model)[2]
    intercept <- coefficients(model)[1]
    
    # Create the plot
    p <- ggplot(length_data, aes(x = indexSelected, y = meanVal, color = color)) +
      # Add reference lines
      geom_hline(yintercept = 0, linetype = "solid", color = "gray70", size = 0.5) +
      geom_vline(xintercept = 0, linetype = "solid", color = "gray70", size = 0.5) +
      # Add diagonal identity line (x=y)
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", size = 0.8) +
      # Add data points
      geom_point(size = 3, alpha = 0.7) +
      # Add regression line (for all points, regardless of color)
      geom_abline(intercept = intercept, slope = slope, color = "black", size = 1) +
      # Add correlation text
      annotate("text", x = min(length_data$indexSelected) + 0.2 * diff(range(length_data$indexSelected)), 
               y = max(length_data$meanVal) - 0.1 * diff(range(length_data$meanVal)), 
               label = paste("r =", r_value, "\np =", p_value), 
               hjust = 0, size = 4.5, fontface = "bold") +
      # Add labels and theme
      labs(title = paste("Array Length:", length_val),
           subtitle = paste("Correlation between indexSelected and meanVal"),
           x = "Index Selected", 
           y = "Mean Value") +
      scale_color_manual(values = c("blue" = "blue", "red" = "red"), name = "Color") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
    
    # Display the plot
    print(p)
    
    # Save individual plot
    ggsave(paste0("correlation_array_length_", length_val, ".png"), p, width = 8, height = 6)
    
    # Print summary statistics
    cat("\nArray Length", length_val, ":\n")
    cat("  Number of trials:", nrow(length_data), "\n")
    cat("  Correlation:", r_value, "\n")
    cat("  P-value:", p_value, "\n")
    cat("  Regression equation: y =", round(intercept, 3), "+", round(slope, 3), "x\n")
  } else {
    cat("Not enough data for array length", length_val, "\n")
  }
}

# Now create a combined plot with just the regression lines for each array length
# First, calculate regression models for each array length
regression_data <- data.frame()

for (length_val in relevant_lengths) {
  # Filter data for this array length
  length_data <- valid_data %>% filter(array_length == length_val)
  
  if (nrow(length_data) >= 3) {
    # Calculate the model
    model <- lm(meanVal ~ indexSelected, data = length_data)
    slope <- coefficients(model)[2]
    intercept <- coefficients(model)[1]
    
    # Get the correlation
    corr <- cor.test(length_data$indexSelected, length_data$meanVal)
    r_value <- round(corr$estimate, 3)
    
    # Get the x range to create the line
    x_min <- min(length_data$indexSelected)
    x_max <- max(length_data$indexSelected)
    
    # Store line data
    line_data <- data.frame(
      array_length = as.factor(length_val),
      x = c(x_min, x_max),
      y = c(intercept + slope * x_min, intercept + slope * x_max),
      equation = paste0("y = ", round(intercept, 2), " + ", round(slope, 2), "x  (r = ", r_value, ")")
    )
    
    regression_data <- rbind(regression_data, line_data)
  }
}

# Create the combined plot with all regression lines
if (nrow(regression_data) > 0) {
  # Find the overall range for x and y with a bit of padding
  all_x <- valid_data$indexSelected
  all_y <- valid_data$meanVal
  x_range <- range(all_x, na.rm = TRUE)
  y_range <- range(all_y, na.rm = TRUE)
  
  # Add padding to ranges to ensure lines don't get cut off
  x_padding <- diff(x_range) * 0.1
  y_padding <- diff(y_range) * 0.1
  x_range <- c(x_range[1] - x_padding, x_range[2] + x_padding)
  y_range <- c(y_range[1] - y_padding, y_range[2] + y_padding)
  
  # Make sure all regression lines fit within these expanded ranges
  regression_min_x <- min(regression_data$x, na.rm = TRUE)
  regression_max_x <- max(regression_data$x, na.rm = TRUE)
  regression_min_y <- min(regression_data$y, na.rm = TRUE)
  regression_max_y <- max(regression_data$y, na.rm = TRUE)
  
  x_range[1] <- min(x_range[1], regression_min_x)
  x_range[2] <- max(x_range[2], regression_max_x)
  y_range[1] <- min(y_range[1], regression_min_y)
  y_range[2] <- max(y_range[2], regression_max_y)
  
  # Create the combined plot
  combined_plot <- ggplot() +
    # Add reference x and y axes
    geom_hline(yintercept = 0, linetype = "solid", color = "gray70", size = 0.5) +
    geom_vline(xintercept = 0, linetype = "solid", color = "gray70", size = 0.5) +
    # Add the identity line (abline)
    geom_abline(intercept = 40, slope = -1, linetype = "dashed", color = "black", size = 1) +
    # Add regression lines for each array length
    geom_line(data = regression_data, 
              aes(x = x, y = y, color = array_length, linetype = array_length),
              size = 1.5) +
    # Add labels and legend
    scale_color_manual(values = length_colors,
                       name = "Array Length",
                       labels = paste(relevant_lengths, "items")) +
    scale_linetype_discrete(name = "Array Length",
                            labels = paste(relevant_lengths, "items")) +
    labs(title = "Comparison of Regression Lines by Array Length",
         subtitle = "Relationship between indexSelected and meanVal",
         x = "Index Selected",
         y = "Mean Value") +
    # Set axis limits based on expanded ranges
    coord_cartesian(xlim = x_range, ylim = y_range) +
   
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  # Display the combined plot
  print(combined_plot)

}