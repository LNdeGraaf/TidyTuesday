## Tidy tuesday wk 22 
# cocktails 

setwd("C:/Users/Louise/Desktop")
# load packages
library(tidyverse); library(cowplot); library(gsubfn)

# get the data
cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')

# get unique drink names 
unique_ingredients <- cocktails %>%
  filter(alcoholic == "Alcoholic") %>%
  distinct(ingredient)

# fix measurements overall 
cocktails_alc <- cocktails %>%
  # select only alcoholic drinks 
  filter(alcoholic == "Alcoholic") %>%
  # one shot is one oz
  mutate(measure = str_replace_all(measure, "shot", "oz")) %>%
  mutate(measure = str_replace_all(measure, "ozs", "oz")) %>%
  # take average of oz measures with a range 
  mutate(measure = recode(measure, "70ml/2fl oz" = "2 oz", 
                          "3-4 oz" = "3.5 oz",
                          "2-3 oz" = "2.5 oz",
                          "1-2 oz" = "1.5 oz")) %>%
  # recode cocktails with duplicate names 
  mutate(drink = recode(drink, "Long Island Tea" = "Long Island Iced Tea",
                        "French \"75\"" = "French 75")) %>%
  # Fancy names --> simple names, it's all the same thing, right? 
  mutate(ingredient = recode(ingredient,  "Malibu rum" = "Rum",
                             "Light rum" = "Rum",
                             "151 proof rum" = "Rum",
                             "Añejo rum" = "Rum",
                             "Dark rum" = "Rum",
                             "Dark Rum" = "Rum",
                             "White rum" = "Rum",
                             "White Rum" = "Rum",
                             "Absolut Vodka" = "Vodka",
                             "Sloe gin" = "Gin",
                             "gin" = "Gin",
                             "Fresh Lime Juice" = "Lime juice",
                             "Lime Juice" = "Lime juice",
                             "Lemon Juice" = "Lemon juice",
                             "orange juice" = "Orange juice",
                             "Orange Juice" = "Orange juice",
                             "Peach Vodka" = "Peach schnapps",
                             "Peachtree schnapps" = "Peach schnapps")) %>%
  # For convenience, let's assume we only have an oz measure
  filter(str_detect(measure, "oz")) %>%
  group_by(drink) %>%
  # simple cocktails only - max 3 ingredients!
  filter(ingredient_number <= 3) %>%
  # split amount from measure 
  separate(measure, sep = " o", into = c("measure_num", "units"), remove = FALSE) %>%
  # convert fractions to decimal points
  mutate(measure_num = gsubfn("(\\d+) (\\d+)", ~ as.numeric(x) + as.numeric(y), 
         gsubfn("(\\d+)/(\\d+)", ~ as.numeric(x)/as.numeric(y), measure_num))) %>%
  # convert factor to numeric
  mutate(measure_num = as.numeric(measure_num))

## --------------------------------------- set plot theme ----------------------------------##
# simple theme with grid lines so measures are easier to read
theme_for_plots <- theme_cowplot() + 
  theme(panel.grid.major.y = element_line(size = 0.5, linetype = "dotted"))

# colour scheme such that colors correspond somewhat to the drinks
colour_scheme <- scale_fill_manual(NULL,values=c("Amaretto"="chocolate1", 
                                                 "Bailey's irish cream"="peachpuff1",
                                                 "Gin"="turquoise2",
                                                 "Ginger ale"="burlywood3",
                                                 "Cranberry juice"="red3",
                                                 "Orange juice"="tan1",    
                                                 "Kahlua" = "goldenrod2",
                                                 "Peach schnapps" = "lightsalmon1",
                                                 "Rum" = "tan3",
                                                 "Vodka" = "lightblue",
                                                 "7-Up" = "palegreen",
                                                 "Blue Curacao" = "blue3",
                                                 "Brandy" = "firebrick4",
                                                 "Creme de Cacao" = "chocolate4",
                                                 "Dry Vermouth"="darkseagreen4",
                                                 "Lemon juice" = "khaki1",
                                                 "Sweet Vermouth" = "seagreen1",
                                                 "Triple sec" = "darkorange",
                                                 "Lime juice" = "palegreen3",
                                                 "Pineapple juice" = "lightgoldenrod1",
                                                 "Tia maria" = "wheat4"))

## --------------------------------------- subplots ----------------------------------##
## VODKA
cocktails_vodka <- cocktails_alc %>%
  group_by(drink) %>%
  filter(any(ingredient_number == 1 & ingredient == "Vodka"))

# get the top 10 most frequently used ingredients 
vodka_top10 <-  cocktails_vodka %>%
  group_by(ingredient) %>%
  count(ingredient, sort = TRUE) %>%
  head(n = 10) %>%
  select(ingredient)

vodka_plot <- cocktails_vodka %>%
  # only cocktails with the top 10 ingredients
  filter(factor(ingredient) %in% factor(vodka_top10$ingredient)) %>%
  group_by(drink) %>%
  # ... and at least one ingredient
  filter(n() > 1) %>%
  # plot stacked bars
  ggplot(aes(x = drink, y = measure_num, fill = ingredient)) +
  geom_bar(stat = "identity") + 
  theme_for_plots +
  scale_y_continuous(breaks = seq(0, 10, by = 1), expand=c(0,0)) +
  colour_scheme + 
  ylab("measure (oz)") +
  xlab(" ") + 
  ggtitle("Vodka")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain")) 
  
## GIN
cocktails_gin <- cocktails_alc %>%
  group_by(drink) %>%
  filter(any(ingredient_number == 1 & ingredient == "Gin"))

# get the top 10 most frequently used ingredients 
gin_top10 <-  cocktails_gin %>%
  group_by(ingredient) %>%
  count(ingredient, sort = TRUE) %>%
  head(n = 10) %>%
  select(ingredient)

gin_plot <- cocktails_gin %>%
  # only cocktails with the top 10 ingredients
  filter(factor(ingredient) %in% factor(gin_top10$ingredient)) %>%
  group_by(drink) %>%
  # ... and at least one ingredient
  filter(n() > 1) %>%
  # plot stacked bars
  ggplot(aes(x = drink, y = measure_num, fill = ingredient)) +
  geom_bar(stat = "identity") + 
  theme_for_plots +
  colour_scheme +
  scale_y_continuous(breaks = seq(0, 10, by = 1), expand=c(0,0)) +
  ylab("measure (oz)") +
  xlab(" ") + 
  ggtitle("Gin")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain")) 

## RUM
cocktails_rum <- cocktails_alc %>%
  group_by(drink) %>%
  filter(any(ingredient_number == 1 & ingredient == "Rum"))

# get the top 10 most frequently used ingredients 
rum_top10 <-  cocktails_rum %>%
  group_by(ingredient) %>%
  count(ingredient, sort = TRUE) %>%
  head(n = 10) %>%
  select(ingredient)

rum_plot <- cocktails_rum %>%
  # only cocktails with the top 10 ingredients
  filter(factor(ingredient) %in% factor(rum_top10$ingredient)) %>%
  group_by(drink) %>%
  # ... and at least one ingredient
  filter(n() > 1) %>%
  # plot stacked bars
  ggplot(aes(x = drink, y = measure_num, fill = ingredient)) +
  geom_bar(stat = "identity") + 
  theme_for_plots +
  colour_scheme + 
  scale_y_continuous(breaks = seq(0, 10, by = 1), expand=c(0,0)) +
  ylab("measure (oz)") +
  xlab(" ") + 
  ggtitle("Rum")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(color = "grey20", size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain")) 

## --------------------------------------- combine plots ----------------------------------##

main_title <- ggdraw() + draw_label("Quarantine cocktails", 
                                    fontface = "bold",
                                    size = 20)
sub_title <- ggdraw() + draw_label("Easy drinks (up to 3 ingredients) to make when you have a bottle of...",
                                   fontface = "bold",
                                   size = 16)

combined_plot <- plot_grid(vodka_plot, gin_plot, rum_plot,
                           nrow = 1, 
                           ncol = 3,
                           align = "hv",
                           labels = NULL)

combined_plot_with_title <- plot_grid(main_title, sub_title, combined_plot,
                                      nrow = 3,
                                      ncol = 1,
                                      rel_heights = c(0.1, 0.05, 2))

ggsave(filename = "quarantine_cocktails.png", combined_plot_with_title, dpi = 600,
       width = 20, 
       height = 10,
       units = "in")