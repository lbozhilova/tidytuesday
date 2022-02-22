###############################
##### World Freedom index #####
###############################

# TidyTuesday entry for 22 February 2022

#----- Packages
library("tidyverse")
library("tidytuesdayR")
library("MetBrewer")

#----- Code
source("plot_setup.R")

#----- Data
tuesdata <- tidytuesdayR::tt_load('2022-02-22')
freedom <- tuesdata$freedom

former_ussr_states <- c("Armenia", "Azerbaijan", "Belarus", "Estonia", "Georgia", 
                        "Kazakhstan", "Kyrgyzstan", "Latvia", "Lithuania", "Republic of Moldova", 
                        "Russian Federation", "Tajikistan", "Turkmenistan", "Ukraine", "Uzbekistan")
former_comecon_states <- c("Bulgaria", "Cuba", "Czechia", "Slovakia", "Germany","Hungary", "Mongolia",
                           "Poland", "Romania", "Viet Nam")

df <- freedom %>% 
  filter(country %in% c(former_ussr_states, former_comecon_states)) %>% 
  filter(year %in% c(1995, 2020))
df <- cbind(df, idx = .5 * (df$CL + df$PR))
df <- pivot_wider(df, 
                  id_cols = "country", 
                  names_from = "year", 
                  values_from = c("CL", "PR", "Status", "idx"))

#----- Plot
is_worse_cols <- met.brewer("Troy")[c(3, 6)]
names(is_worse_cols) <- c("TRUE", "FALSE")
is_worse_labs <- c("Less free", "As free or more free")
names(is_worse_labs) <- c("TRUE", "FALSE")

p <- ggplot(df, aes(y = country)) +
  theme_tt +
  ggtitle("Changing freedoms in the former Easterm Bloc from 1995 to 2020") +
  geom_point(aes(x = idx_1995), colour = "grey60", size = 2) +
  geom_point(aes(x = idx_2020, colour = idx_2020 > idx_1995), size = 2) +
  geom_segment(aes(x = idx_1995, xend = idx_2020, y = country, yend = country,  colour = idx_2020 > idx_1995)) +
  geom_vline(xintercept = c(2.75, 5.25), linetype = "dotted", colour = "grey50") +
  annotate("text", x = c(1.8, 4, 6.3), y = 26, label = c("Free", "Partly Free", "Not Free"), colour = "grey30") +
  scale_colour_manual(values = is_worse_cols, labels = is_worse_labs) +
  scale_y_discrete("", expand = c(0, 2)) +
  scale_x_continuous("Aggregate score", breaks = 1:7)

plot_save(p, "figures/22_08_world_freedom_index.jpg", type = "full")
