# tidy tuesday wk 21
# beach volleyball 
# Louise Martens

# load packages
library(tidyverse)
library(ggtext)

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
  select(match_id, gender, matches("[wl]_p[12]")) %>%
  # apparently birthdate messes things up so delete it?
  select(-contains("birthdate")) %>%
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
  # select only relevant data 
  select(match_id:country, tot_blocks) %>%
  # group by player and keep other relevant variables
  group_by(name, gender, hgt, country) %>%
  # so we can now calculate 
  summarise(n_matches = n(), # no of matches played by each player
            pct_winner = mean(win_lose == "w"), # percentage of games won by each player
            mean_blocks = mean(as.numeric(tot_blocks), na.rm = TRUE)) %>% # mean nr of blocks for each player
  # remove rows with missing values
  drop_na() 


# average height by country 
avg_height_by_country <- player_data %>%
  group_by(country, gender) %>%
  summarise(avg_height = mean(as.numeric(hgt), na.rm = TRUE)) %>%
  arrange(desc(avg_height)) 

# join data frames so each player has avg height for their country
player_data <- player_data %>%
  inner_join(avg_height_by_country) 

# correct player height for mean height of players from the same country (calculated separately by gender)
height_corrected_model <- lm(hgt ~ scale(avg_height), data = player_data)
player_data$height_corrected <- coef(height_corrected_model)[1] + residuals(height_corrected_model)

# correct mean number of blocks for height of player
blocks_corrected_model <- lm(mean_blocks ~ scale(as.numeric(hgt)), data = player_data)
player_data$blocks_corrected <- coef(blocks_corrected_model)[1] + residuals(blocks_corrected_model)

# -------------------------------------------------------  --------------------------------------------------------#
windowsFonts(`Roboto Condensed` = windowsFont("Roboto Condensed"))

# -------------------------------------------------------------- plots! ----------------------------------------------------------------#
(blocks_by_height <- player_data %>%
  ggplot(aes(x = mean_blocks, y = height_corrected, colour = gender)) +
  geom_point() +
  geom_smooth(method = lm, se = F, fullrange = T, color = "black") +
  theme_classic() +
  labs(x = "Mean number of blocks", y = "Player height (in)",
       title = "Do taller beach volleyball players block more?",
       subtitle = "Yes, both taller women and taller men do!",
       caption = "Source: BigTimeStats.Visualization by Louise Martens") +
  # Turn off the legends for color and fill, since the subtitle includes that
  guides(color = FALSE, fill = FALSE) +
  theme(plot.title = element_markdown(face = "bold", size = rel(1.6)),
        plot.subtitle = element_markdown(size = rel(1.3)),
        plot.margin = unit(c(0.5, 1, 0.5, 0.5), units = "lines")))

(wins_by_blocks <- player_data %>%
    ggplot(aes(x = mean_blocks, y = pct_winner)) +
    geom_point() +
    geom_smooth(method = lm, se = T, fullrange = F, color = "black") +
    theme_classic() +
    labs(x = "Mean number of blocks", y = "% of games won",
         title = "Do players who block more, win more?",
         subtitle = "",
         caption = "Source: BigTimeStats.Visualization by Louise Martens") +
    # Turn off the legends for color and fill, since the subtitle includes that
    guides(color = FALSE, fill = FALSE) +
    theme(plot.title = element_markdown(face = "bold", size = rel(1.6)),
          plot.subtitle = element_markdown(size = rel(1.3)),
          plot.margin = unit(c(0.5, 1, 0.5, 0.5), units = "lines"),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15)) +
  facet_wrap(vars(gender)))

(wins_by_blocks_corr <- player_data %>%
    ggplot(aes(x = blocks_corrected, y = pct_winner)) +
    geom_point() +
    geom_smooth(method = lm, se = T, fullrange = F, color = "black") +
    theme_classic() +
    labs(x = "Mean number of blocks", y = "% of games won",
         title = "Do players who block more, win more?",
         subtitle = "",
         caption = "Source: BigTimeStats.Visualization by Louise Martens") +
    # Turn off the legends for color and fill, since the subtitle includes that
    guides(color = FALSE, fill = FALSE) +
    theme(plot.title = element_markdown(face = "bold", size = rel(1.6)),
          plot.subtitle = element_markdown(size = rel(1.3)),
          plot.margin = unit(c(0.5, 1, 0.5, 0.5), units = "lines"),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15)) +
  facet_wrap(vars(gender)))

# -------------------------------------------------------------- plots! ----------------------------------------------------------------#


  