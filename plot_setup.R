###############################
##### TidyTuesday ggtheme #####
###############################

#----- Packages 
library("tidyverse")

#----- Themes 
theme_tt <- theme_minimal(base_size = 10) +
  theme(
    text = element_text(color = "gray20"),
    # Legend
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = 0.1,
    legend.title = element_blank(),
    # Axes
    axis.text = element_text(face = "italic"),
    axis.title.x = element_text(vjust = -1),        
    axis.title.y = element_text(vjust = 2),
    axis.ticks.x = element_line(color="gray40", size=0.3),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "gray40", size = 0.3),
    axis.line.y = element_blank(),
    # Panel
    panel.grid.major = element_line(color = "gray50", size = 0.5),
    panel.grid.major.x = element_blank()
  )

#----- Save figure
plot_save <- function(p, 
                      filename, 
                      type = c("full", "half"), 
                      ar = 1,
                      dev = "jpeg"){
  type <- type[1]
  w <- 190
  if (type == "half"){
    w <- 90
  } else if (type != "full"){
    warning("Invalid type. Creating two-column image.")
  }
  h <- w/ar
  ggsave(filename = filename,
         plot = p,
         width = w,
         height = h,
         units = "mm",
         device = dev)
}
