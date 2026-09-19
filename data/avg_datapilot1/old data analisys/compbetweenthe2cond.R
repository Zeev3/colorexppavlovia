#────────────────────────────────────────────────────────────
# 0.  Libraries
#────────────────────────────────────────────────────────────
# install.packages(c("tidyverse", "broom", "purrr", "patchwork", "effsize", "lme4", "lmerTest"))
library(tidyverse)
library(broom)
library(patchwork)   # nice plot grids
library(effsize)     # Cohen's d
library(lme4); library(lmerTest)  # mixed-model option

#────────────────────────────────────────────────────────────
# 1.  Load & label the two data sets
#────────────────────────────────────────────────────────────
df_avg <- read_csv("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/avg_data/data_cleaned.csv",   col_types = cols()) %>%
  mutate(condition = "Average")

df_exp <- read_csv("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/exp_data/data_cleaned.csv", col_types = cols()) %>%
  mutate(condition = "Experience")

df_all <- bind_rows(df_avg, df_exp)

#────────────────────────────────────────────────────────────
# 2.  Helper to pull slope & intercept from any grouping
#────────────────────────────────────────────────────────────
get_coefs <- function(data, ...) {
  data %>%
    group_by(...) %>%                           # grouping vars supplied by ...
    filter(n() >= 2) %>%                        # ≥2 trials to fit an lm
    nest() %>%
    mutate(
      model = map(data, ~ lm(meanVal ~ indexSelected, data = .x)),
      tidy  = map(model, tidy)
    ) %>%
    unnest(tidy) %>%
    pivot_wider(names_from  = term,
                values_from = estimate,
                names_glue  = "{term}") %>%
    rename(intercept = `(Intercept)`,
           slope     = indexSelected)
}

#────────────────────────────────────────────────────────────
# 3.  One row = participant × array_length × condition
#────────────────────────────────────────────────────────────
coef_tbl <- get_coefs(df_all, participant_id, array_length, condition)

# coef_raw is the table that still has *two* rows per model
#   columns: participant_id, array_length, condition, term, estimate, …

slope_tbl <-coef_tbl %>%                # keep only the slope rows
  select(participant_id, array_length, condition, slope) %>% 
  filter(is.finite(slope))                       # term no longer useful
                # drop NA / Inf

intercept_tbl <- coef_tbl %>%            # keep only the intercept rows
  select(participant_id, array_length, condition, intercept) %>%
  filter(is.finite(intercept))
 
# ── violin + boxplot per length/condition ──────────────────
p_len <- ggplot(slope_tbl,
                aes(x = factor(array_length), y = slope, fill = condition)) +
  geom_violin(trim = FALSE, alpha = .3, na.rm = TRUE) +
  geom_boxplot(width = .15, outlier.shape = NA, na.rm = TRUE) +
  labs(title = "Slope by array length & condition",
       x = "Array length", y = "Slope") +
  theme_minimal()

# ── collapsed across lengths ───────────────────────────────
p_all <- slope_tbl %>%
  group_by(participant_id, condition) %>%
  summarise(avg_slope = mean(slope), .groups = "drop") %>%
  ggplot(aes(x = condition, y = avg_slope, fill = condition)) +
  geom_violin(trim = FALSE, alpha = .3, na.rm = TRUE) +
  geom_boxplot(width = .25, outlier.shape = NA, na.rm = TRUE) +
  labs(title = "Average slope (collapsed across lengths)",
       y = "Average slope", x = NULL) +
  theme_minimal()

(p_len) / (p_all)      # requires library(patchwork)
library(effsize)
array_levels <- sort(unique(slope_tbl$array_length))

paired_design <- FALSE     # because groups are independent

tt_results <- map_dfr(array_levels, function(len) {
  
  tmp <- slope_tbl %>%
    filter(array_length == len)
  
  # split into two vectors
  x <- tmp %>% filter(condition == "Average")    %>% pull(slope)
  y <- tmp %>% filter(condition == "Experience") %>% pull(slope)
  
  if (length(x) < 2 || length(y) < 2) {
    return(tibble(array_length = len,
                  n_Average     = length(x),
                  n_Experience  = length(y),
                  mean_Average  = mean(x, na.rm = TRUE),
                  mean_Experience = mean(y, na.rm = TRUE),
                  t = NA_real_, df = NA_real_, p_val = NA_real_, cohen_d = NA_real_))
  }
  
  test <- t.test(x, y, paired = FALSE)
  eff  <- effsize::cohen.d(x, y, pooled = TRUE)
  
  tibble(array_length   = len,
         n_Average      = length(x),
         n_Experience   = length(y),
         mean_Average   = mean(x),
         mean_Experience= mean(y),
         t      = test$statistic,
         df     = test$parameter,
         p_val  = test$p.value,
         cohen_d= eff$estimate)
})

print(tt_results)

##################
overall_tbl <- slope_tbl %>%
  group_by(participant_id, condition) %>%
  summarise(avg_slope = mean(slope), .groups = "drop")

x <- overall_tbl %>% filter(condition == "Average")    %>% pull(avg_slope)
y <- overall_tbl %>% filter(condition == "Experience") %>% pull(avg_slope)

overall_t <- t.test(x, y, paired = FALSE)           # independent Welch test
overall_d <- effsize::cohen.d(x, y, pooled = TRUE)

overall_results <- tibble(
  n_Average      = length(x),
  n_Experience   = length(y),
  mean_Average   = mean(x),
  mean_Experience= mean(y),
  t      = overall_t$statistic,
  df     = overall_t$parameter,
  p_val  = overall_t$p.value,
  cohen_d= overall_d$estimate
)

print(overall_results)


#────────────────────────────────────────────────────────────
# 4.  Correlation between the two regression lines
#     at each array-length level
#────────────────────────────────────────────────────────────
library(purrr)

line_similarity <- function(len, grid = 50) {
  
  row <- line_means %>% filter(array_length == len)
  x   <- seq(1, len, length.out = grid)
  
  y_A <- row$Average_mean_intercept   + row$Average_mean_slope   * x
  y_E <- row$Experience_mean_intercept + row$Experience_mean_slope * x
  
  tibble(array_length = len,
         r = cor(y_A, y_E))
}

sim_tbl <- map_dfr(sort(unique(line_means$array_length)), line_similarity)


corr_lines

#────────────────────────────────────────────────────────────
# 5.  Plot the two regression lines per array_length level
#────────────────────────────────────────────────────────────
library(ggplot2)      # tidyverse already loads it, but explicit is fine
library(patchwork)    # for easy composition if you want

# Faceted version: everything in a single figure -----------------------
p_lines <- df_all %>%
  ggplot(aes(x = indexSelected,
             y = meanVal,
             colour = condition,
             fill   = condition)) +
  geom_point(alpha = 0.20, size = 1, na.rm = TRUE) +   # faint raw data
  geom_smooth(method = "lm", se = FALSE, size = 1.2, na.rm = TRUE) +
  facet_wrap(~ array_length, scales = "free_x") +       # one panel per length
  labs(title = "Fitted lines by condition at each array length",
       x = "Index selected",
       y = "meanVal") +
  theme_minimal() +
  theme(legend.position = "bottom")

p_lines            # render it

# ----------------------------------------------------------------------
# Alternative: separate plots (returned as a list) ---------------------
plots_by_len <- map(array_levels, function(len) {
  df_all %>%
    filter(array_length == len) %>%
    ggplot(aes(indexSelected, meanVal,
               colour = condition,
               fill   = condition)) +
    geom_point(alpha = 0.20, size = 1, na.rm = TRUE) +
    geom_smooth(method = "lm", se = FALSE, size = 1.2, na.rm = TRUE) +
    labs(title = paste("Array length", len),
         x = "Index selected", y = "meanVal") +
    theme_minimal()
})

# If you prefer them stacked / tiled:
wrap_plots(plots_by_len, ncol = 1)   # or ncol = 2, etc.

#────────────────────────────────────────────────────────────
# 6.  Save each condition-line plot to a separate file
#────────────────────────────────────────────────────────────
library(fs)          # tidy file handling (optional but handy)

out_dir <- "/Users/zeevbenamos/Documents"  # <- change me
dir_create(out_dir)                                         # make folder if missing

# Name each plot file like "array_len_02.png", "array_len_06.png", …
plot_files <- sprintf("array_len_%02d.png", array_levels)

purrr::walk2(
  plots_by_len,      # list of ggplot objects
  plot_files,        # matching file names
  ~ ggsave(
    filename = path(out_dir, .y),  # full path
    plot     = .x,
    width    = 6, height = 4, dpi = 300,   # tweak size/resolution as needed
    units    = "in"                        # or "cm"
  )
)

message("Saved ", length(plot_files), " plots to ", out_dir)
