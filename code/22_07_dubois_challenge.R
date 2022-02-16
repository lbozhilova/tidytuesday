################################
##### #DuBoisChallenge2022 #####
######### 2022-02-15 ###########
################################

# TidyTuesday entry for 15 February 2022. 

#----- Packages
library("tidyverse")
library("tidytuesdayR")

#----- Code
source("plot_setup.R")

#----- Data
# Data taken from ONS: Updating ethnic contrasts in deaths involving the 
# coronavirus (COVID-19), England: 8 December 2020 to 1 December 2021 
ons_df <- read_csv("data/ons_ethnicity_covid.csv") 
ons_df$sex <- factor(ons_df$sex, levels = c("Male", "Female"))
eth <- ons_df$ethnic_group %>% unique %>% rev 
ons_df$ethnic_group <- factor(ons_df$ethnic_group, levels = eth)
ons_df$adjust <- factor(ons_df$adjust, levels = c("full", "mid", "age"))

#----- DuBois theme
theme_dubois <- theme_minimal(base_size = 10) +
  theme(
    text = element_text(color = "gray30", face = "bold"),
    # Background
    plot.background = element_rect(fill = "#fffff0"),
    # Legend
    legend.position = "top",
    legend.direction = "vertical",
    legend.justification = -2,
    legend.title = element_blank(),
    # Axes
    axis.text = element_text(face = "bold"),
    axis.title.x = element_text(vjust = -1),        
    axis.title.y = element_text(vjust = 2),
    axis.ticks.x = element_line(color="gray60", size=0.3),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "gray60", size = 0.3),
    axis.line.y = element_blank(),
    # Panel
    panel.grid.major = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

#----- Plot
hr_labs <- c("0" = "SAME\nRATE", "1" = "2x", "2" = "3x", "3" = "4x", "4" = "5x")
sex_labs <- c("Male" = "MEN", "Female" = "WOMEN")
eth_labs <- eth %>% toupper(); names(eth_labs) <- eth
adj_labs <- c("Adjusted for age",
              "Adjusted for age, residence type, geography, socio-economic factors and certain\npre-existing conditions",
              "Fully-adjusted, including for vaccination status") %>% toupper()
names(adj_labs) <- c("age", "mid", "full")

adj_cols <- c("age" = "#c30101",
              "mid" = "#377c2b",
              "full" = "#ffb800")
  
p <- ggplot(ons_df, aes(x = ethnic_group, fill = adjust)) +
  ggtitle("COVID-19: RATE OF DEATH COMPARED TO WHITE BRITISH GROUP") +
  theme_dubois +
  facet_grid(.~sex, labeller = labeller(sex = sex_labs)) +
  geom_bar(aes(y = ratio - 1), stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower - 1, ymax = upper - 1), position = "dodge",
                colour = "grey40") +
  geom_hline(yintercept = 0, colour = "grey40") +
  scale_y_continuous("", breaks = 0:4, labels = hr_labs) +
  scale_x_discrete("", labels = eth_labs) +
  scale_fill_manual(labels = adj_labs, values = adj_cols) +
  coord_flip()

plot_save(p, "figures/22_07_dubois_challenge.jpg", type = "full", ar = 1.2)
