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
  filter(start_year >= 1900) %>%
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

(volcano_plot_2 <- ggplot(volcano_for_plot) +
  # base layer is the world map 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "gray50", # fill
               colour = "gray66",    # borders
               size = 0.2) +
  # add volcanoes - colour by eruption count
  geom_point(aes(longitude_new, latitude, colour = as.numeric(eruption_count), text = hover_text), size = 2.5) +
  scale_color_gradient(name = "Number of total eruptions", low = alpha("gold", 0.8), high = alpha("red", 0.5), 
                       guide = "colourbar") +
#  scale_colour_viridis_c(option = "magma", guide = guide_legend(title = "Number of total eruptions", title.position = "top", keyheight = 0.7, keywidth = 0.8, override.aes = list(size = 1.2))) +
  coord_fixed() +
#  guides(colour = guide_legend(guide = "colourbar")) +
  theme_void(base_family = "IBM Plex Sans") +
  theme(plot.background = element_rect(fill = "gray36", colour = "gray36"),
        plot.title = element_markdown(face = "bold", size = rel(1.3), hjust = 0.1, colour = "gray86"),
        plot.subtitle = element_markdown(face = "bold", size = rel(1), hjust = 0.63, colour = "gray86"),
        plot.caption = element_text(colour = "gray86", size = 9.5, hjust = 0.95, vjust = 3),
        legend.title = element_text(colour = "gray86"),
        legend.text = element_text(colour = "gray86"),
        legend.position = c(0.14,0.34),
        plot.margin = margin(0,0,0,0, "cm")) +
  labs(title = "The Ring of Fire: volcanic eruptions since 1900",
       subtitle = "75% of the world's volcanoes are located along this path along the Pacific Ocean characterized by active volcanoes.",
       caption = "Data source: The Smithsonian Institution & National Geographic. Graphic: Louise Martens"))

# wanted to make the plot interactive, but ggplotly doesn't like Mollweide projections...
volcano_interactive <- ggplotly(volcano_plot_2, tooltip = "hover_text")
  
  