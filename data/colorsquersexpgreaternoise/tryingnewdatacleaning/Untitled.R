# Permutation Test Example with Visualization

library(ggplot2)

set.seed(123)

# Example data
treatment <- c(6, 7, 8, 5, 9)
control   <- c(4, 5, 6, 5, 4)

# Observed statistic
obs_diff <- mean(treatment) - mean(control)

# Combine all data
all_data <- c(treatment, control)
group_sizes <- c(length(treatment), length(control))

# Permutations
n_perm <- 10000
perm_diffs <- numeric(n_perm)

for (i in 1:n_perm) {
  perm_labels <- sample(all_data)
  g1 <- perm_labels[1:group_sizes[1]]
  g2 <- perm_labels[(group_sizes[1]+1):length(all_data)]
  perm_diffs[i] <- mean(g1) - mean(g2)
}

# P-value (two-sided)
p_value <- mean(abs(perm_diffs) >= abs(obs_diff))
cat("Observed diff:", obs_diff, "\n")
cat("Permutation p-value:", p_value, "\n")

# Visualization
ggplot(data.frame(perm_diffs), aes(x = perm_diffs)) +
  geom_histogram(binwidth = 0.2, fill = "lightblue", color = "black") +
  geom_vline(xintercept = obs_diff, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Permutation Test Null Distribution",
       x = "Difference in Means",
       y = "Frequency") +
  annotate("text", x = obs_diff, y = max(table(cut(perm_diffs, breaks=20))),
           label = paste0("Observed diff = ", round(obs_diff,2),
                          "\n p = ", round(p_value,3)),
           hjust = -0.1, vjust = -1, color = "red")
