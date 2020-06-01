# tidy tuesday wk20
# volcanoes!
# Louise Martens

library(tidyverse)
library(maps)
library(sf)
library(mapproj)
library(scales)
library(ggtext)

setwd("C:/Users/Louise/Documents/GitHub/TidyTuesday/")
windowsFonts(`IBM Plex Sans` = windowsFont("IBM Plex Sans"))

# get the data
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

# count eruptions for each volcano - exclude discredited eruptions
eruptions_counted <- eruptions %>%
  filter(!str_detect(eruption_category, "Discredited Eruption")) %>%
  group_by(volcano_name) %>%
  summarise(eruption_count = n())

# combine counted eruptions with geographical location of each volcano
volcano_for_plot <- volcano %>%
  inner_join(eruptions_counted) %>%
  select(volcano_name, last_eruption_year, country, latitude, longitude, eruption_count) %>%
  # fix longitudes for pacific centric projection
  mutate(longitude_new = ifelse(longitude < 0, longitude + 360, longitude)) %>%
  # don't include volcanoes outside our 'field of view'
  filter(longitude_new > 60 & longitude_new < 340) %>%
  # this was for an interactive plot, but that didn't work
  mutate(hover_text = paste("Name: ", volcano_name, "\n",
                           "Last eruption: ", last_eruption_year))

# load pacific centric map and truncate it 
world_map <- map_data("world2") %>%
  filter(long > 60 & long < 340 )

# create plot
volcano_plot <- ggplot(volcano_for_plot) +
  # base layer is the world map 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "palegreen3", # fill
               colour = "white",    # borders
               size = 0.1) +
  # add volcanoes - colour by eruption count
  geom_point(aes(longitude_new, latitude, colour = eruption_count, text = hover_text), size = 2) +
  scale_color_gradient(name = "Number of total eruptions", low = alpha("red", 0.8), high = alpha("yellow", 0.5)) +
  # nice projection
  coord_map(projection = "mollweide") +
  guides(colour = guide_legend()) +
  theme_void(base_family = "IBM Plex Sans") +
  theme(plot.background = element_rect(fill = "grey100", colour = "grey100"),
        plot.title = element_markdown(face = "bold", size = rel(1.3), hjust = 0.5),
        plot.subtitle = element_markdown(size = rel(1.1), hjust = 0.5),
        plot.caption = element_text(colour = "black", size = 10, hjust = 0.5, vjust = 0)) +
  labs(title = "The Ring of Fire",
       subtitle = "Total recorded volcanic eruptions. <br>75% of the world's volcanoes are located this path along the Pacific Ocean <br> characterized by active volcanoes.",
       caption = "Source: The Smithsonian Institution & National Geographic. Graphic: Louise Martens")

ggsave(filename = "volcanoes_in_ring_of_fire.png", 
       device = "png",
       height = 5,
       width = 9,
       units = "in",
       dpi = 600)

## wanted to make the plot interactive, but ggplotly doesn't like Mollweide projections...    
#volcano_interactive <- ggplotly(volcano_plot, tooltip = "hover_text")
  
  