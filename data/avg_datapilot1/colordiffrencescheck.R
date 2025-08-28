#––––––––––––––––––––––––––––––––––––––––––––
# 0.  Helper – same idea as before
#––––––––––––––––––––––––––––––––––––––––––––
df <- read_csv("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/exp_data/data_cleaned.csv",
               col_types = cols())   # let readr guess column types
get_coefs <- function(data, ...) {
  data %>% 
    dplyr::group_by(...) %>%                     # ... = grouping vars
    dplyr::filter(dplyr::n() >= 2) %>%           # need ≥2 rows to fit lm
    tidyr::nest() %>% 
    dplyr::mutate(
      model = purrr::map(data,
                         ~ stats::lm(meanVal ~ indexSelected, data = .x)),
      tidy  = purrr::map(model, broom::tidy)
    ) %>% 
    tidyr::unnest(tidy) %>% 
    tidyr::pivot_wider(
      names_from  = term,
      values_from = estimate,
      names_glue  = "{term}"
    ) %>% 
    dplyr::rename(
      intercept = `(Intercept)`,
      slope     = indexSelected
    )
}

#––––––––––––––––––––––––––––––––––––––––––––
# 1.  One row per participant × color
#––––––––––––––––––––––––––––––––––––––––––––
coef_color <- get_coefs(df, participant_id, color)
#> columns: participant_id, color, intercept, slope
library(dplyr)
library(tidyr)

# 1. Collapse any duplicates --------------------------------------------------
# If the same participant has >1 slope for the same colour,
# take the *mean* slope (change `mean` to `median` or `first` if you prefer).
coef_color_clean <- coef_color %>%
  group_by(participant_id, color) %>%
  summarise(slope = mean(slope, na.rm = TRUE), .groups = "drop")

# 2. Pivot to wide (one row per participant, one column per colour) -----------
paired_slopes <- coef_color_clean %>%
  pivot_wider(names_from  = color,
              values_from = slope) %>%   # now each colour column is numeric
  drop_na()                              # keep complete pairs only

# Double-check: everything should be numeric, no list columns
str(paired_slopes)

# 3. Identify the two colours you want to compare -----------------------------
# Replace 'red' and 'blue' with the actual column names that appear above.
colnames(paired_slopes)
#> e.g. "participant_id" "red" "blue"

# 4. Paired t-test -------------------------------------------------------------
tt <- t.test(paired_slopes$red,
             paired_slopes$blue,
             paired = TRUE)
print(tt)

# 5. Optional effect size ------------------------------------------------------
# install.packages("effsize")   # once
library(effsize)
d <- effsize::cohen.d(paired_slopes$red,
                      paired_slopes$blue,
                      paired = TRUE)
print(d)
