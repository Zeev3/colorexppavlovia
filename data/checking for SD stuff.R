library(ggeffects)
library(ggplot2)
library(dplyr)
setwd("/Users/zeevbenamos/Documents/GitHub/RttM_project/amits_data")

Data_raw <- read.csv("amit_exp1.csv")
Data_raw$row_sd <-apply(Data_raw[, 0:12], 1, function(x) sd(x, na.rm = TRUE))


participants_SD <- Data_raw %>% 
  group_by(id, numberFaces, condition) %>%
  summarise(SD = sd(rating, na.rm = TRUE), .group = 'drop')
Positive_Data_SD <- participants_SD %>% filter(condition == "positive")
Negative_Data_SD <- participants_SD %>% filter(condition == "negative")

Mean_sd_per_factor <- participants_SD %>%
  group_by(numberFaces, condition) %>%
  summarise(Mean_SD = mean(SD, na.rm = TRUE), .groups = 'drop')

ggplot(Mean_sd_per_factor, aes(x = numberFaces, y = Mean_SD, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() +                                
  labs(
    title = "Mean SD per Factor",
    x = "Factor",
    y = "Mean SD"
  ) +
  facet_wrap(~ condition)

scatterplot_positiveSD <- ggplot(Positive_Data_SD, aes(x = numberFaces, y = SD)) +
  geom_jitter(width = 0.2, height = 0, color = "blue", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "SDs for Participants per Factor Level (Positive Condition)",
    x = "Factor Level",
    y = "SD"
  )

scatterplot_negativeSD <- ggplot(Negative_Data_SD, aes(x = numberFaces, y = SD)) +
  geom_jitter(width = 0.2, height = 0, color = "red", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "SDs for Participants per Factor Level (Negative Condition)",
    x = "Factor Level",
    y = "SD"
  )
print(scatterplot_positiveSD)
print(scatterplot_negativeSD)
