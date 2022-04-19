#######################################
##### Crossword Puzzles and Clues #####
######################################

  # TidyTuesday entry for 19 March 2022

#----- Packages
library("tidyverse")
library("tidytuesdayR")
library("MetBrewer")

#----- Code
source("plot_setup.R")

#----- Data
tuesdata <- tidytuesdayR::tt_load('2022-04-19')
df <- rbind(tuesdata$big_dave, tuesdata$times)

# Locate the start and end of every definition, where available
locs <- str_locate(fixed(df$clue), fixed(df$definition))
df <- df[!is.na(locs[, 1]), ]
locs <- locs[!is.na(locs[, 1]), ]

# Extract the clue length
clue_len <- df$clue %>% 
  fixed() %>% 
  str_locate_all(fixed(" (")) %>% 
  sapply(function(mt) max(mt[nrow(mt), 1] - 1, 0)) %>% 
  unlist

df <- cbind(df, def_start = locs[, 1], def_end = locs[, 2], clue_len = clue_len)
sum(clue_len == 0) # 3139: just discard these
df <- filter(df, clue_len > 0)

# Softly (+- 2char) identify whether definition is first, last, or the whole clue
is_first <- df$def_start <= 3
is_last <- (df$clue_len - df$def_end) <= 2

df <- cbind(df, def_loc = NA)
df$def_loc[is_first] <- "first"
df$def_loc[is_last] <- "last"
df$def_loc[is_first & is_last] <- "full"

sum(is.na(df$def_loc)) # 1245, remove (saw these; mostly middle defs)
df <- filter(df, !is.na(def_loc))

# Do crosswords have a first / last slant?
df_lt <- df %>% 
  split(df$puzzle_name) # 8465 crosswords
sapply(df_lt, nrow) %>% sort %>% tail # check nothing crazy

slant_df <- df_lt %>%
  lapply(function(df) data.frame("puzzle_date" = df$puzzle_date[1],
                                 "puzzle_name" = df$puzzle_name[1],
                                 "source_url" = df$source_url[1],
                                 "source" = df$source[1],
                                 "num_first" = sum(df$def_loc == "first"),
                                 "num_last" = sum(df$def_loc == "last"),
                                 "num_full" = sum(df$def_loc == "full"),
                                 "num_clues" = nrow(df))) %>% 
  bind_rows()
slant_df$puzzle_name %>% str_remove_all("[0-9]") %>% unique # too messy to work with rn

slant2_df <- df_lt %>%
  lapply(function(df) data.frame("puzzle_date" = df$puzzle_date[1],
                                 "puzzle_name" = df$puzzle_name[1],
                                 "source_url" = df$source_url[1],
                                 "source" = df$source[1],
                                 "num" = c(sum(df$def_loc == "first"), sum(df$def_loc == "last")),
                                 "type" = c("first", "last"),
                                 "num_clues" = nrow(df))) %>% 
  bind_rows()

#----- Plot
cols <- met.brewer("Egypt", 2)
names(cols) <- c("first", "last")
labs <- c("first" = "Leading", "last" = "Trailing")

p <- ggplot(slant2_df, aes(x = num / num_clues, fill = type)) +
  theme_tt + 
  ggtitle("Definition location in cryptic crossword clues") +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 25) +
  scale_x_continuous("Proportion of clues") +
  scale_y_continuous("Density") +
  scale_fill_manual("Definition: ", labels = labs, values = cols)
p

plot_save(p, "figures/22_16_xwd.jpg", size = 0.75, ar = 4/3)
