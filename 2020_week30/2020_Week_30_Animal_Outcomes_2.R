## tidy tuesday week 30
# RSCPA Australian animal outcomes
# Louise Martens

# Load libraries and data -------------------------------------------------
library(tidyverse)
library(ggtext)
library(cowplot)
library(here)

windowsFonts(`IBM Plex Sans` = windowsFont("IBM Plex Sans"))

animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')


# Clean data for line charts ----------------------------------------------
animal_data <- animal_outcomes %>% 
  # stick to cats, dogs, and small pets ("Other Animals")
  filter(animal_type == "Cats") %>% 
  
  # tidy up 
  mutate_if(is.character, as.factor) %>% 
  select(year, animal_type, outcome, ACT:Total) %>% 
  rename(Outcome = outcome,
         Animal = animal_type) %>% 
  
  # pivot longer - territories
  pivot_longer(cols = "ACT":"WA",
               names_to = "Territory",
               values_to = "Number") %>% 
  
  # compute total actions/outcomes by animal type and year
  group_by(Territory, year) %>% 
  mutate(total_actions = sum(Number),
         pct_total_actions = round((Number/total_actions)*100,4)) %>% 
  ungroup() %>% 
  
  # rename territories 
  mutate(Territory = factor(Territory),
         Territory = recode(Territory, 
                            "ACT" = "Australian Capital Territory",
                            "NSW" = "New South Wales",
                            "NT" = "Northern Territory",
                            "QLD" = "Queensland",
                            "SA" = "South Australia",
                            "VIC" = "Victoria",
                            "TAS" = "Tasmania",
                            "WA" = "Western Australia"))

# Theme for plots ---------------------------------------------------------
RSPCA_theme <- theme(plot.background = element_rect(fill = "#e3eef4", colour = "#e3eef4"),
                     panel.background = element_rect(fill = "#e3eef4", colour = "#e3eef4"),
                     plot.title = element_markdown(face = "bold", size = rel(1.7), hjust = 0, colour = "#0a2d80"),
                     plot.subtitle = element_markdown(face = "bold", size = rel(1.1), hjust = 0, colour = "#0a2d80"),
                     plot.caption = element_text(colour = "#0a2d80", size = rel(0.75), hjust = 0.95, vjust = 3),
                     legend.title = element_text(colour = "#0a2d80"),
                     legend.text = element_text(colour = "#0a2d80"),
                     axis.text.y = element_markdown(size = rel(1), hjust = 0.5, colour = "#0a2d80"),
                     axis.text.x = element_markdown(size = rel(1), hjust = 0.5, colour = "#0a2d80"),
                     # axis.title = element_markdown(face = "bold", size = rel(1.1), hjust = 0.5, colour = "#0a2d80"),
                     axis.title = element_blank(),
                     axis.line = element_blank(),
                     axis.ticks = element_blank(),
                     panel.grid.major.y = element_line(linetype = 2, size = 0.3, colour = "#0a2d80"),
                     plot.margin = margin(1,0.5,0.5,0.5, "cm"),
                     strip.background =element_rect(fill="#e3eef4", colour = "#e3eef4"),
                     strip.text = element_markdown(face = "bold", size = rel(1.1), hjust = 0.5, colour = "#0a2d80"),
                     panel.spacing = unit(1, "lines"))


# Cats plot - by territory ------------------------------------------------
cats_plot <- ggplot(data = animal_data, aes(x = year, y = pct_total_actions, group = Outcome)) +
#  geom_line(size = 1.5, colour = "darkgray") +
  geom_line(data = animal_data %>% filter(Outcome == "Euthanized"), size = 2, colour = "#0294d0") +
  geom_line(data = animal_data %>% filter(Outcome == "Rehomed"), size = 2, colour = "#8bc68e") +
  scale_x_continuous(limits = c(1999,2018),
                      breaks = seq(2000, 2018, 2),
                     labels = c(str_c("'0", as.character(seq(0, 8, 2))),
                                str_c("'",as.character(seq(10, 18, 2))))) +
                     # labels = c("'99", 
                     #            str_c("'0", as.character(seq(0, 9, 1))), 
                     #            str_c("'",as.character(seq(10, 18, 1))))) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 101),
                     breaks = seq(0, 100, 20),
                     labels = c(str_c(as.character(seq(0, 100, 20)), "%"))) +
  theme_void(base_family = "IBM Plex Sans") +
  theme_classic() +
  RSPCA_theme + 
  facet_wrap(~as.factor(Territory)) +
  labs(title = "Cats in Australia are <b style='color:#8bc68e'>rehomed</b> more and <b style='color:#0294d0'>euthanized</b> less",
       subtitle = "Data presented as % of total outcomes reported by RSCPA Australia.  \n",
       caption = "Data source: RSCPA Australia. Graphic by Louise Martens")

ggsave(here::here("2020_week30","australian_pets_by_territory.png"), width = 8, height = 10, dpi = 300)
