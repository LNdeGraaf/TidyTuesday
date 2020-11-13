## tidy tuesday week 40
# Beyoncé and Taylor Swift Lyrics
# Louise Martens 
# 2020.10.04

# first we want a table with sentiment analysis for each Taylor Swift and 
# Beyoncé album

# Load libraries and data -------------------------------------------------

library(tidyverse)
library(tidytext)
library(syuzhet) # get sentences
library(stm) # topic modelling 
library(quanteda) # quantative analysis of text
library(Rtsne) # tsne 
library(rsvd) # Randomized Singular Value Decomposition
library(janitor)
library(kableExtra)
library(textdata)
library(reactable)
library(dataui)
library(textstem)


taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv') %>% 
  clean_names() %>% 
  remove_empty(which = "rows") 

sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')
charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')


# Tidy data and remove stop words -----------------------------------------
taylor_tidy <- taylor_swift_lyrics %>% 
  select(album, title, lyrics) %>% 
  unnest_tokens(output = word, input = lyrics, to_lower = TRUE) %>% 
  anti_join(y = stop_words, by = "word")

# table to check if all stop words have been removed (from https://rpubs.com/RosieB/taylorswiftlyricanalysis)
taylor_tidy %>%
  count(word, sort = TRUE) %>%
  kable(align = "c")%>%
  kable_styling(bootstrap_options = c("striped", "condensed","responsive", "bordered")) %>%
  row_spec(0, align = "c")%>%
  add_header_above(c("Most popular words in Taylor Swift songs"= 2), bold = TRUE)  %>% 
  scroll_box(width = "500px", height = "400px") 

# additional stop words from the table above
more_stop_words <- list("ooh", "yeah", "ah", "uh", "ha", "whoah", "mmm", "ey",
                          "eh", "eeh", "la", "mm", "hoo", "ahh", "na", "aah",
                          "hm", "hmm", "ohh", "oooh", "uuh")

taylor_tidy <- taylor_tidy %>% 
  filter(!word %in% more_stop_words)

# Get sentiments ----------------------------------------------------------
afinn_sentiments <- get_sentiments("afinn")

taylor_sentiments <- taylor_tidy %>% 
  # get sentiment scores
  inner_join(afinn_sentiments) %>% 
  # get lemma for each word so we have more usable data 
  mutate(lemma = textstem::lemmatize_words(word)) %>% 

# Test test ---------------------------------------------------------------
# add 10 empty lines for each album 
empty_lines <- matrix(NA, nlevels(factor(taylor_sentiments$album))*7, ncol(taylor_sentiments))
colnames(empty_lines) <- colnames(taylor_sentiments)
# add album titles
empty_lines <- as_tibble(empty_lines) %>% 
  mutate(album = rep(levels(factor(taylor_sentiments$album)), each = 7),
         sent_per_song_neg = NA,
         sent_per_song_pos = NA)

taylor_for_plot <- taylor_sentiments %>% 
  mutate(title = factor(title)) %>% 
  group_by(title) %>% 
  mutate(sent_per_song_neg = ifelse(!is.na(title), ifelse(value < 0, mean(value), NA), 0),
         sent_per_song_pos = ifelse(!is.na(title), ifelse(value > 0, mean(value), NA), 0)) %>% 
  ungroup() %>% 
  distinct(title, .keep_all = TRUE) %>% 
  rbind(., empty_lines) %>% 
  arrange(album) %>% 
  mutate(id = row_number()) %>% 
  mutate(sent_per_song_pos = ifelse(is.na(sent_per_song_pos), 0, sent_per_song_pos),
         sent_per_song_neg = ifelse(is.na(sent_per_song_pos), 0, sent_per_song_pos)) %>% 
  ungroup()

# add col for mean pos and mean neg
mini_plot <- ggplot(data = taylor_for_plot) +
  geom_col(aes(x = id, y = sent_per_song_pos, fill = sent_per_song_pos), position = "stack") +
  geom_col(aes(x = id, y = sent_per_song_neg, fill = sent_per_song_neg), position = "stack") +
  scale_colour_manual(palette = "RdBu") +
  theme_classic() +
  coord_polar()
mini_plot

# Build plot --------------------------------------------------------------
# insert blank rows between groups 
# with add_row? 

# x. combine into table -top x songs? 
# ranking should be high to low for both artists 