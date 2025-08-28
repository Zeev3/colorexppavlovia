library(lmerTest)
library(lsr)
library(ggeffects)
library(ggplot2)
library(dplyr)
setwd("/Users/zeevbenamos/Documents/GitHub/RttM_project/amits_data")

Data_raw <- read.csv("amit_exp1.csv")
Data_raw$row_sd <-apply(Data_raw[, 0:12], 1, function(x) sd(x, na.rm = TRUE))
Filtered_Data <- Data_raw %>% select(id, trial, meanGroup, rating, numberFaces, condition,row_sd)
Filtered_Data$id <- as.factor(Filtered_Data$id)
Positive_Data <- Filtered_Data %>% filter(condition == "positive")
Negative_Data <- Filtered_Data %>% filter(condition == "negative")
df_split <- Negative_Data %>%
  filter(!is.na(row_sd)) %>% 
  group_by(numberFaces) %>%
  mutate(
    Median_sd = median(row_sd),               
    SD_Group = ifelse(row_sd <= Median_sd, "Low", "High") 
  )
low_sd_neg <- df_split %>% filter(SD_Group == "Low")
high_sd_neg <- df_split %>% filter(SD_Group == "High")
RttM_model_pos <- lmer(meanGroup~rating*
                     (numberFaces) + (1 |id), data = Positive_Data)
RttM_model_neg <- lmer(meanGroup~rating*
                         (numberFaces) + (1 |id), data = Negative_Data)
RttM_model_neg_low_sd <- lmer(meanGroup~rating*
                         (numberFaces) + (1 |id), data = low_sd_neg)
RttM_model_neg_high_sd <- lmer(meanGroup~rating*
                         (numberFaces) + (1 |id), data = high_sd_neg)
predictions_pos <- ggpredict(RttM_model_pos , c("rating", "numberFaces[1,5,11]")) 
predictions_neg <- ggpredict(RttM_model_neg , c("rating", "numberFaces[1,5,11]")) 
prediction_lowsd_neg <- ggpredict(RttM_model_neg_low_sd , c("rating", "numberFaces[1,5,11]"))
prediction_highsd_neg <- ggpredict(RttM_model_neg_high_sd , c("rating", "numberFaces[1,5,11]"))
#plotting part
m<-lmer(rating~meanGroup*as.factor(numberFaces)+(1+meanGroup|id), data = Negative_Data)
  True_vauleprediction <-(ggpredict(m,c("meanGroup","numberFaces[1,6,11]")))
blues <- c("#000066", "#000099", "#0000CC", "#0000FF", "#1A1AFF", "#3333FF", 
           "#4D4DFF", "#6666FF", "#8080FF", "#9999FF", "#B2B2FF", "#CCCCFF")
ggplot(Positive_Data, aes(x = rating, y = meanGroup)) +
  geom_point(color = as.factor(Positive_Data$numberFaces), size = 1) +          
  geom_abline(intercept = 0, slope = 1,           
              color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Scatter Plot of Truth Values vs Estimations",  
                                         #
  ) + geom_smooth( color = "green", se = TRUE) +
  theme_minimal()                                      
plot(predictions_neg, show_ci = TRUE, ) + ggtitle("negative ratings") + geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1)
plot(predictions_pos, show_ci = TRUE, ) + ggtitle("postive ratings") + geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1) 
plot(prediction_lowsd_neg, show_ci = TRUE, ) + ggtitle("negative ratings with lower sd") + geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1)
plot(prediction_highsd_neg, show_ci = TRUE, ) + ggtitle("negative ratings with higher sd") + geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1)
plot(True_vauleprediction, show_ci = TRUE) + ggtitle("negative ratings true") + geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1)