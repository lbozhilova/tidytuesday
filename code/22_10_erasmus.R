###############################
##### EU Student Mobility #####
###############################

# TidyTuesday entry for 8 March 2022

#----- Packages
library("tidyverse")
library("tidytuesdayR")
library("MetBrewer")

#----- Code
source("plot_setup.R")

#----- Data
tuesdata <- tidytuesdayR::tt_load('2022-03-08')
df <- tuesdata$erasmus

# Obtain gender split for countries sending 500+ students
df <- df[rep(1:nrow(df), times = df$participants), ]
gender_df <- table(df[, c("sending_country_code", "participant_gender")]) %>% 
  data.frame() %>% 
  pivot_wider(names_from = "participant_gender", values_from = "Freq")
gender_df <- cbind(gender_df, "all" = rowSums(gender_df[, -1])) %>% 
  filter(all >= 500)

#----- Plot
p <- ggplot(gender_df, aes(x = reorder(sending_country_code, - Female / all), 
                      y = Female / all, 
                      size = Female)) +
  theme_tt + theme(legend.position = "none") +
  ggtitle("Gender split of Erasmus students") +
  geom_point(colour = "#c2185b") +
  geom_hline(yintercept = sum(gender_df$Female) / sum(gender_df$all),
             linetype = "dotted") +
  scale_x_discrete("Sending country") +
  scale_y_continuous("Proportion of female learners", limits = c(0.3, 0.7))

plot_save(p, "figures/22_10_erasmus.jpg", type = "full")
