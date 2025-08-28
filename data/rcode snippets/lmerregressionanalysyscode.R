library(lmerTest)
library(lsr)
library(ggeffects)
library(ggplot2)
library(dplyr)
setwd("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/avg_data")
Data_raw <- read.csv("data_cleaned.csv")
Data_raw$row_sd <-apply(Data_raw[, 0:12], 1, function(x) sd(x, na.rm = TRUE))
Filtered_Data <- Data_raw %>% select(participant_id, trial,fixationTime, meanVal, indexSelected, array_length, color)
Filtered_Data$id <- as.factor(Filtered_Data$participant_id)
red_Data <- Filtered_Data %>% filter(color == "red")
blue_Data <- Filtered_Data %>% filter(color == "blue")
df_split <- blue_Data %>%
  filter(!is.na(row_sd)) %>% 
  group_by(array_length) %>%
  mutate(
    Median_sd = median(row_sd),               
    SD_Group = ifelse(row_sd <= Median_sd, "Low", "High") 
  )
low_sd_neg <- df_split %>% filter(SD_Group == "Low")
high_sd_neg <- df_split %>% filter(SD_Group == "High")

filt_dat=rbind(red_Data,blue_Data) %>% subset(meanVal<36 & meanVal>14)
a=filt_dat %>%
  na.omit() %>% group_by(participant_id,array_length)%>%
  summarise(corr=cor(meanVal,indexSelected),n=n(),
            slope=coef(lm(meanVal~indexSelected))[2],
  intercept = coef(lm(meanVal ~ indexSelected))[1]
)
a %>% group_by(array_length) %>%
  summarise(mean(corr),mean(slope), mean(intercept), mean(intercept))
 
  summary(RttM_model_all <- lmer(meanVal~indexSelected*
                                 factor(array_length) + (1+indexSelected|participant_id), 
                               data = filt_dat))
summary(RttM_model_pos <- lmer(meanVal~indexSelected*
                                 factor(array_length) + (1+indexSelected |participant_id), data = red_Data))
summary(RttM_model_neg <- lmer(meanVal~indexSelected*
                         factor(array_length) + (1 |participant_id), data = blue_Data))
RttM_model_neg_low_sd <- lmer(meanVal~indexSelected*
                         (array_length) + (1 |participant_id), data = low_sd_neg)
RttM_model_neg_high_sd <- lmer(meanVal~indexSelected*
                         (array_length) + (1 |participant_id), data = high_sd_neg)
predictions_red <- ggpredict(RttM_model_pos , c("indexSelected", "array_length")) 
predictions_blue <- ggpredict(RttM_model_neg , c("indexSelected", "array_length")) 
prediction_lowsd_blue <- ggpredict(RttM_model_neg_low_sd , c("indexSelected", "array_length"))
prediction_highsd_blue <- ggpredict(RttM_model_neg_high_sd , c("indexSelected", "array_length"))
#plotting part
m<-lmer(indexSelected~meanVal*as.factor(array_length)+(1+meanVal|participant_id), data = blue_Data)
  True_vauleprediction <-(ggpredict(m,c("meanVal","array_length")))
blues <- c("#000066", "#000099", "#0000CC", "#0000FF", "#1A1AFF", "#3333FF", 
           "#4D4DFF", "#6666FF", "#8080FF", "#9999FF", "#B2B2FF", "#CCCCFF")
ggplot(red_Data, aes(x = indexSelected, y = meanVal)) +
  geom_point(color = as.factor(red_Data$array_length), size = 1) +          
  geom_abline(intercept = 0, slope = 1,           
              color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Scatter Plot of Truth Values vs Estimations",  
                                         #
  ) + geom_smooth( color = "green", se = TRUE) +
  theme_minimal()                                      
plot(predictions_blue, show_ci = TRUE, ) + ggtitle("negative ratings") + geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1)
plot(predictions_red, show_ci = TRUE, ) + ggtitle("postive ratings") + geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1) 
plot(prediction_lowsd_blue, show_ci = TRUE, ) + ggtitle("negative ratings with lower sd") + geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1)
plot(prediction_highsd_blue, show_ci = TRUE, ) + ggtitle("negative ratings with higher sd") + geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1)
plot(True_vauleprediction, show_ci = TRUE) + ggtitle("negative ratings true") + geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", size = 1)

