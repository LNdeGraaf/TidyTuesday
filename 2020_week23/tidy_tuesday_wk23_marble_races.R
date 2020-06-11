# tidy tuesday wk23
# marble races!
# Louise Martens
library(tidyverse)
library(ggbump)
library(ggtext)
library(dplyr)

setwd("C:/Users/Louise/Documents/GitHub/TidyTuesday/2020_week23/")
windowsFonts(`IBM Plex Sans` = windowsFont("IBM Plex Sans"))

# get the data ------------------------------------------------------------
marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')



# prepare color scheme for plot -------------------------------------------
colour_scheme <- scale_colour_manual(NULL,values=c("Balls of Chaos"="orangered3", 
                                                 "Green Ducks"="lemonchiffon4",
                                                 "Gin"="turquoise2",
                                                 "Hazers"="gray38",
                                                 "Hornets"="yellow2",
                                                 "Limers"="greenyellow",    
                                                 "Mellow Yellow" = "yellow",
                                                 "Midnight Wisps" = "slategray2",
                                                 "O'rangers" = "orange3",
                                                 "Raspberry Racers" = "#CC3366",
                                                 "Rojo Rollers" = "firebrick1",
                                                 "Savage Speeders" = "orangered4",
                                                 "Snowballs" = "lightskyblue1",
                                                 "Team Galactic" = "darkorchid2",
                                                 "Team Momo"="palegreen4",
                                                 "Team Primary" = "gold1",
                                                 "Thunderbolts" = "royalblue4"))


# rank teams according to their cumulative points -------------------------
marble_for_plot <- marbles %>%
  filter(race %in% c("S1R1" ,"S1R2" ,"S1R3", "S1R4" ,"S1R5", "S1R6" ,
                     "S1R7", "S1R8")) %>%
  mutate(race = gsub("S1R*", "Race ", race)) %>%
  group_by(team_name) %>%
  mutate(total_points = cumsum(points)) %>%
  ungroup() %>%
  group_by(race) %>%
  mutate(rank_per_race = rank(-total_points, ties.method = "first")) 

# create bump chart -------------------------------------------------------
marble_plot <- marble_for_plot %>%
  
  ggplot(aes(x = race, y = rank_per_race, color = team_name, group = team_name)) +
  
  # add lines
  geom_segment(data = tibble(x = 1, xend = 8, y = 1:16),
               aes(x = x, xend = xend, y = y, yend = y),
               color = "grey30", size = .3, inherit.aes = FALSE) +
  geom_segment(aes(x = 1, xend = 1, y = 1, yend = 16),
               color = "grey20", size = .5) +
  
  # add bump chart, dots at start and end
  geom_bump(size = 1.5, alpha = 0.9, smooth = 5, show.legend = FALSE) +
  geom_point(data = marble_for_plot %>% filter(race == "Race 1"), size = 3) +
  geom_point(data = marble_for_plot %>% filter(race == "Race 8"), size = 5) +
  
  
  # correct scales
  scale_y_reverse(breaks = seq(1, 16, by = 1)) +
  scale_x_discrete(position = "top", 
                   expand = c(.010, .030),
                   labels = c(glue::glue("Race {1:7}"), "Finish"))  +
  
  # aesthetics 
  theme_classic() +
  theme_void(base_family = "IBM Plex Sans") +
  colour_scheme + 
  guides(colour = guide_legend()) +
  
  # set colours and position of text and background
  theme(plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = "black", colour = "black"),
        legend.background = element_rect(fill = "black", colour = "black"),
        legend.position="bottom", legend.box = "horizontal",
 #       plot.title = element_markdown(face = "bold", size = rel(1.3), hjust = 0.5, vjust = -10, colour = "white"),
        plot.caption = element_text(colour = "white", size = 8, hjust = 0.95, vjust = 0.8),
        legend.title = element_text(colour = "white"),
        legend.text = element_text(colour = "white"),
        axis.title.x.top = element_text(color = "grey95", size = 18, face = "bold"),
        axis.text.y = element_text(margin = margin(r = 1)),
        axis.ticks.y =element_blank(),
        axis.text.x = element_text(margin = margin(t = 1, r = 0, b = 0, l = 0, "cm")),
        axis.ticks.x = element_blank(),
        axis.text = element_text(colour = "white"),
        plot.margin = margin(1,1,1,1, "cm")) + 
  
  # labels
  labs(x = "Marbula 1 Season 1: Places Over Time ",
       caption = "Data source: Jelles Marble Race. Graphic: Louise Martens")

marble_plot

# save plot ---------------------------------------------------------------
ggsave(filename = "marble_race.png",
       device = "png",
       height = 7.5,
       width = 10,
       units = "in",
       dpi = 600)
