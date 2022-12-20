# Load packages ---------------------------------------------------------------------------------------------------
library(tidyverse)
library(stringr)
library(nflreadr)
library(showtext)

# Data ------------------------------------------------------------------------------------------------------------
pbp <- nflreadr::load_pbp(2015:2022)
rosters <- nflreadr::load_rosters(2015:2022)


# Data manipulation -----------------------------------------------------------------------------------------------

#get players we are interested in
nonqb <- rosters %>% 
  filter(position %in% c('WR', 'TE', 'RB'))

nonqb_passes <- pbp %>% 
  select(passer_player_name, passer_id, season) %>% 
  #filter passes thrown by non qbs
  filter(passer_id %in% nonqb$gsis_id)%>% 
  #filter out some special cases: k.Hinton 2020 and T.Pryor 2015
  filter(passer_id != '00-0035864',
         passer_id != '00-0028825') 

#get number of game in each season (differences because of playoffs and current season underway)
games <- data.frame(game_id = unique(pbp$game_id)) %>% 
  mutate(season = str_sub(game_id, 1, 4)) %>% 
  group_by(season) %>% 
  summarise(games = n())


nonqb_byseason <- nonqb_passes %>% 
  group_by(season) %>% 
  summarise(npasses = n(), 
            epa = mean(epa),
            wp = mean(wp)) %>% 
  mutate(games = games$games, 
         pergame = npasses/games) 

# Plot -------------------------------------------------------

# Change font of plot 
font_add(family = "Times", regular = "Times.ttf")
showtext_auto()
#The following will cause the plot to look bad in the plot window in rstudio,
#but will look good when saved with ggsave: use same dpi as in the ggsvae call.
showtext::showtext_opts(dpi = 1000) 

#plot
nonqb_byseason %>% 
  ggplot(aes(x = season, y = pergame)) +
  geom_point(size = 0.5)+
  geom_line(aes(group = 1), size = 0.5)+
  labs(title = 'Non-QB (RB, WR, TE) passes per game',
       subtitle = 'Passes by T.Pryor (Browns, 2016) and K.Hinton (Broncos, 2020) excluded as they took snaps as QB',
       y = 'Passes per game', x = NULL,
       caption = 'data: nflreadr')+
  theme_classic()+
  theme(text = element_text(family = "Times"))+
  theme(plot.title = element_text(size = 7, hjust = 0.5),
        plot.subtitle = element_text(size = 5, hjust = 0.5),
        plot.caption = element_text(size = 4.5),
        axis.title = element_text(size = 5.5),
        axis.text = element_text(size = 5),
        axis.line = element_blank(),
        panel.border = element_rect(fill = NA))
ggsave('non qb passes.png', width = 3.5, height = 2.26, units = 'in', dpi = 1000)  


  

  