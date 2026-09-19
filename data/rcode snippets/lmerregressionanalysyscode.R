library(lmerTest)
library(lsr)
library(ggeffects)
library(ggplot2)
library(dplyr)
setwd("/Users/zeevbenamos/Documents/GitHub/colorexppavlovia/data/variance_control_exp")
Data_raw <- read.csv("data_cleaned.csv")
Filtered_Data <- Data_raw %>% select(participant_id, trial,fixationTime, meanVal, indexSelected, array_length, color)
Filtered_Data$id <- as.factor(Filtered_Data$participant_id)
red_Data <- Filtered_Data %>% filter(color == "red")
blue_Data <- Filtered_Data %>% filter(color == "blue")

filt_dat=rbind(red_Data,blue_Data) #%>% subset(meanVal<36 & meanVal>14)
filt_dat$array_length <- factor(filt_dat$array_length)
#contrasts(filt_dat$array_length) <- contr.sum(nlevels(filt_dat$array_length))

a=filt_dat %>%
  na.omit() %>% group_by(participant_id,array_length)%>%
  summarise(corr=cor(indexSelected,meanVal),n=n(),
            slope=coef(lm(indexSelected~meanVal))[2],
  intercept = coef(lm(indexSelected ~ meanVal))[1]
)
a %>% group_by(array_length) %>%
  summarise(mean(corr),mean(slope), mean(intercept), mean(intercept))
  summary(RttM_model_all <- lmer(indexSelected~meanVal*
                                 factor(array_length) + (1+ meanVal*
                                                           factor(array_length)  |participant_id), 
                               data = filt_dat))
  b=filt_dat %>%
    na.omit() %>% group_by(participant_id,array_length)%>%
    summarise(corr=cor(indexSelected,meanVal),n=n(),
              slope=coef(lm(meanVal~indexSelected))[2],
              intercept = coef(lm(meanVal ~ indexSelected))[1]
    )
  b %>% group_by(array_length) %>%
    summarise(mean(corr),mean(slope), mean(intercept), mean(intercept))
  summary(RttM_model_reverse <- lmer(meanVal~indexSelected*
                                   factor(array_length) + (1+ indexSelected*
                                                             factor(array_length)  |participant_id), 
                                 data = filt_dat))

predictions_across <- ggpredict(RttM_model_all , c("meanVal", "array_length"))
predictions_across_reverse <- ggpredict(RttM_model_reverse , c("indexSelected", "array_length"))
#plotting part
n_participants <- filt_dat %>%
  distinct(participant_id) %>%
  nrow()

avg_trials <- filt_dat %>%
  count(participant_id) %>%
  summarize(mean(n)) %>%
  pull() %>%
  round(1)

ggplot(predictions_across, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "Objective mean",
    y = "Predicted subjective experience",
    color = "Array length",
    fill = "Array length",
    title = "Experience condition"
  ) +
  geom_abline(
    slope = 1, intercept = 0,
    color = "black",
    linetype = "dashed",
    size = 1
  )+
  theme_classic()

  ggplot(predictions_across, aes(x = predicted, y = x, color = group, fill = group)) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(xmin = conf.low, xmax = conf.high), alpha = 0.2, color = NA) +
    labs(
      x = "Predicted subjective experience",
      y = "Objective mean",
      color = "Array length",
      fill = "Array length",
      title = "Experience condition"
    ) +
    geom_abline(
      slope = 1, intercept = 0,
      color = "black",
      linetype = "dashed",
      size = 1
    )+
  theme_classic()
  
   plot(predictions_across_reverse, show_ci = TRUE) +
     geom_abline(
       slope = 1, intercept = 0,
       color = "black",
       linetype = "dashed",
       size = 1
     ) +
     coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) +
     labs(
       title = "exp condition",
       subtitle = paste0(
         "N = ", n_participants,
         ", Mean trials per participant = ", avg_trials
       ),
       
     )

