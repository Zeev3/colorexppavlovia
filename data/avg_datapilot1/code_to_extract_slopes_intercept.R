#––––––––––––––––––––––––––––––––––––––––––––
# 0.  Setup
#––––––––––––––––––––––––––––––––––––––––––––
# install.packages(c("dplyr", "purrr", "readr", "tibble", "broom"))  # if needed
library(dplyr)
library(purrr)
library(readr)
library(broom)   # tidy() makes it easy to pull coefficients

#––––––––––––––––––––––––––––––––––––––––––––
# 1.  Load the cleaned dataset
#––––––––––––––––––––––––––––––––––––––––––––
df <- read_csv("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/exp_data/data_cleaned.csv",
               col_types = cols())   # let readr guess column types

#––––––––––––––––––––––––––––––––––––––––––––
# 2.  Fit regression per participant & array_length
#––––––––––––––––––––––––––––––––––––––––––––
coef_table <- df %>%                                          # start with the raw data
  group_by(participant_id, array_length) %>%                  # one model per group
  filter(n() >= 2) %>%                                        # need ≥2 rows to fit a line
  nest() %>%                                                  # bundle rows into a list column
  mutate(model = map(data, ~ lm(meanVal ~ indexSelected, data = .x)),  # fit lm
         tidy  = map(model, tidy)) %>%                        # extract coefficients
  unnest(tidy) %>%                                            # spread out term-level rows
  select(participant_id, array_length, term, estimate) %>%    # keep what we need
  pivot_wider(names_from = term,
              values_from = estimate,
              names_glue = "{term}") %>%                      # → columns: `(Intercept)` & `indexSelected`
  rename(intercept = `(Intercept)`,
         slope     = indexSelected) %>%                       # nicer column names
  arrange(participant_id, array_length)                       # tidy order

#––––––––––––––––––––––––––––––––––––––––––––
# 3.  Inspect the result
#––––––––––––––––––––––––––––––––––––––––––––
print(coef_table, n = 20)   # show first few rows

#––––––––––––––––––––––––––––––––––––––––––––
# 4.  (Optional) save to CSV
#––––––––––––––––––––––––––––––––––––––––––––
 write_csv(Data_raw,
           "/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/exp_data/slopesandintercepts.csv")
 #––––––––––––––––––––––––––––––––––––––––––––
 # 5.  Plot distributions of slope & intercept
 #––––––––––––––––––––––––––––––––––––––––––––
 # install.packages("ggplot2")   # if not yet installed
 library(ggplot2)
 
 ## (a) histogram of slopes
 ggplot(coef_table, aes(x = slope)) +
   geom_histogram(bins = 30, colour = "white") +
   labs(title = "Distribution of Slopes",
        x = "Slope (indexSelected coefficient)",
        y = "Count of participant × array_length cases") +
   theme_minimal()
 
 ## (b) histogram of intercepts
 ggplot(coef_table, aes(x = intercept)) +
   geom_histogram(bins = 30, colour = "white") +
   labs(title = "Distribution of Intercepts",
        x = "Intercept (regression constant)",
        y = "Count of participant × array_length cases") +
   theme_minimal()
 
