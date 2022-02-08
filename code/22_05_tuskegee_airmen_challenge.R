#####################################
##### Tuskegee Airmen Challenge #####
############ 2022-02-08 #############
#####################################

# TidyTuesday entry for 8 February 2022. 

#----- Packages
library("tidyverse")
library("tidytuesdayR")
library("ggrepel")
library("emoGG")

#----- Code
source("plot_setup.R")

#----- Data
tuesdata <- tidytuesdayR::tt_load('2022-02-08')
airmen <- tuesdata$airmen

# Calculate the number of airmen and the number of victory credits per state
state_df <- airmen %>% 
  split(airmen$state) %>% 
  lapply(function(df) 
    data.frame(state = df$state[1],
               num_vic_creds = sum(df$number_of_aerial_victory_credits),
               num_airmen = length(unique(df$name)))) %>% 
  bind_rows()

#----- Plot
# Be really shameless with those emojis
p <- ggplot(state_df, aes(x = num_airmen, y = num_vic_creds)) +
  theme_tt +
  ggtitle("Tuskegee Airmen by US State") +
  add_emoji(emoji = "2708") +
  geom_emoji(emoji = "1f396") +
  geom_text_repel(aes(label = ifelse(num_vic_creds > 5 | num_airmen > 50, 
                                     state, ""))) +
  scale_x_continuous("Number of airmen") +
  scale_y_continuous("Number of victory credits", limits = c(0, 15))

plot_save(p, "figures/22_05_tuskegee_airmen_challenge.jpg", type = "half")
