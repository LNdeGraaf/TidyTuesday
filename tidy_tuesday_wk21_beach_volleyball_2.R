# tidy tuesday wk 21
# beach volleyball 
# Louise Martens

# code for animated rank plot (nr of games won) for top 10 women by year (2001-2019)
# code for getting player data adapted from Alex Cookson (@alexcookson) who posted it on Twitter :)

# load packages
library(tidyverse)
library(ggtext)
library(gifski)
library(gganimate)
library(RColorBrewer)
library(gghighlight)
library(ggchicklet)

setwd("C://Users/Louise/Desktop/")

# ---------------------------------------------------------- get the data --------------------------------------------------------------#
vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', 
                              guess_max = 76000)

# ------------------------------------------------------- get player level data --------------------------------------------------------#
player_data <- vb_matches %>%
  mutate(match_id = row_number()) %>%
  # rename player names for consistency with the other player attributes
  rename(w_p1_name = w_player1,
         w_p2_name = w_player2,
         l_p1_name = l_player1,
         l_p2_name = l_player2) %>%
  # ... so the player attributes can be more easily selected 
  select(match_id, year, gender, matches("[wl]_p[12]")) %>%
  # 'date' format of birthdate doesn't work with pivot_longer, not needed anyway
  select(-contains("birthdate")) %>%
  # select only female players
  filter(gender == "W") %>%
  # make everything a character vector b/c combining numeric values and characters in long format is a bad time
  mutate_if(is.numeric, as.character, is.factor, as.character) %>%
  # wide format to get player level stats
  pivot_longer(w_p1_name:l_p2_tot_digs,
               names_to = c("win_lose", "player", "measure"),
               names_pattern = "([wl])_(p[12])_(.*)",
               values_to = "value") %>%
  # keep player level but now all player stats will be in wide format 
  pivot_wider(names_from = measure, 
               values_from = value) %>%
  # convert player actions back to numeric 
  mutate_at(vars(tot_attacks:tot_digs), as.numeric)

# for each player, get the % of games they won for each year 
player_data_wins_by_year <- player_data %>%
  # group by player and year
  group_by(name, year) %>%
  # calculate stats per player per year
  summarise(n_matches = n(),
            pct_wins = mean(win_lose == "w", na.rm = T)) 

# ------------------------------------------------------- plot settings etc-------------------------------------------------------#
# fonts downloaded from google fonts - make them available
windowsFonts(`Bungee` = windowsFont("Bungee"))
windowsFonts(`IBM Plex Sans` = windowsFont("IBM Plex Sans"))

# set default fonts - can be overridden 
update_geom_defaults("text", list(family = "IBM Plex Sans"))
update_geom_defaults("label", list(family = "IBM Plex Sans"))

# expand existing colour palette to the number of unique players in the top10 dataframe below
# i couldn't get the code to run when I separated the data from the plot, that's why I had to put this code here 
# and add nr of colours manually
number_colours <- 68 #n_distinct(top10_wins_data$name) 
my_colours <- colorRampPalette(brewer.pal(8, "Set3"))(number_colours)

# general text/axis/panel settings 
my_theme <- theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  legend.position="none",
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  plot.title = element_markdown(face = "bold", size = rel(2.5), family = "Bungee"),
                  plot.subtitle = element_markdown(size = rel(2)),
                  plot.caption =element_text(size=12),
                  plot.background=element_blank(),
                  plot.margin = margin(1,1,1,5, "cm"))

# ------------------------------------------------------- animated plot ---------------------------------------------------------#
# get data for animated plot: matches won by top 10 player for each year
animated_plot_1 <- player_data_wins_by_year %>%
  group_by(year) %>%
  # for each player and each year, get the number of matches won
  mutate(n_matches_won = n_matches*pct_wins) %>%
  # rank players by number of matches won (again for each year). Ties.method "first" makes sure bars with ties don't overlap
  mutate(rank_player = rank(-n_matches_won, ties.method = "first")) %>%
  group_by(name) %>%
  # get the top 10 players for each year
  filter(rank_player <= 10) %>%
  ggplot(aes(rank_player, group = name)) +
  # add tiles (for bars)
  geom_tile(aes(y = n_matches_won/2,
                height = n_matches_won,
                width = 0.9,
                fill = name),
            alpha = 0.8,
            colour = NA) +
  ## wanted to highlight "winning player" in 2019 over the years but she only was in the top for a couple of years
  # gghighlight(name %in% top_player_2019$name,
  #             use_group_by = T,
  #             unhighlighted_params = list(alpha = 0.3)) %>%
  geom_text(aes(y = 0, label = paste(name, " ")),
            vjust = 0.2,
            hjust = 1,
            size = 7) +
  # add nr of matches won at the end of each bar
  geom_text(aes(y = n_matches_won, 
                label = as.character(n_matches_won),
                hjust = 0),
            size = 8) +
  # flip coordinates so bars are horizontal
  coord_flip(clip = "off", expand = TRUE) +
  scale_x_reverse() +
  # add themes and colours
  theme_minimal() +
  my_theme + 
  scale_fill_manual(values = my_colours) +
  # add animation (new top 10 for each year)
  transition_states(as.numeric(year), 
                    transition_length = 1, 
                    state_length = 2) +
  ease_aes('sine-in-out') +
  # add plot titles
  labs(title = "BEACH VOLLEYBALL TOP 10 {closest_state}",
       subtitle = "Top 10 female players who won the most matches by year (2001-2019)",
       caption = "Source: BigTimeStats.Visualization by Louise Martens")

# save animation as GIF
animate(animated_plot_1, nframes = 350,fps = 7.5,  width = 1600, height = 1000, 
        renderer = gifski_renderer("top10_beach_volley_by_year.gif"))


# ------------------------------------------------------- UNUSED ---------------------------------------------------------#
# # get names of top 10 players (in terms of % of games won)
# top10_players <- player_data_wins_by_year %>%
#   filter(year == "2019" & n_matches >= 50) %>%
#   arrange(desc(pct_wins)) %>%
#   head(n = 10) 

# # for each player, get summary of mean actions per year 
# player_data_actions_by_year_wl <- player_data %>%
#   # group by player and year
#   group_by(name, year, win_lose) %>%
#   # calculate stats per player per year
#   summarise_at(vars(tot_attacks:tot_digs), mean, na.rm = T) %>%
#   filter(year == "2019") %>%
#   filter(name %in% top10_players$name) %>%
#   drop_na()
# 
# player_data_chicklet <- player_data_actions_by_year_wl %>%
#   inner_join(top10_players) %>%
#   select(name:win_lose, tot_kills, tot_errors, tot_aces:tot_digs) %>%
#   pivot_longer(tot_kills:tot_digs,
#                names_to = "action",
#                values_to = "value")

# chicklet_plot <- player_data_chicklet %>%
#   group_by(win_lose) %>%
#   ggplot(aes(x = name, y = value, group = win_lose, fill = action)) +
#   geom_chicklet(width = 0.75) +
#   coord_flip() +
#   theme_classic()
  
