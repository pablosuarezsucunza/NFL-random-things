library(tidyverse)



snapcounts = nflreadr::load_snap_counts(2016:2020)
draft = nflreadr::load_draft_picks()



player_snaps = snapcounts %>% 
  group_by(pfr_player_id) %>% 
  summarise(off_snaps = mean(offense_pct),
            def_snaps = mean(defense_pct))



drafted = draft %>% #get only drafted players that are in the snapcount df
  filter(pfr_id %in% intersect(player_snaps$pfr_player_id, draft$pfr_id)) %>% 
  filter(pfr_name != 'Ty Johnson') %>%  #2 different players share the same pfr_id "JohnTy00"- filter out the one that is not in snapcount df
  select(pfr_id, round, side, category, position)
  
player_snaps = player_snaps %>% #eliminate undrafted 
  filter(pfr_player_id %in% intersect(player_snaps$pfr_player_id, draft$pfr_id)) %>% 
  rename(pfr_id = pfr_player_id) %>% 
  select(pfr_id, off_snaps, def_snaps)




player_snaps %>% 
  left_join(drafted) %>% 
  filter(side != 'S') %>% 
  mutate(snaps = case_when(side == 'O' ~ off_snaps,
                           side == 'D' ~ def_snaps)) %>% 
  group_by(round, category) %>% 
  summarise(snaps = mean(snaps)) %>% 
  ggplot(aes(x = round, y = snaps))+
  geom_bar(stat = 'identity', color = 'black', fill = 'light grey', width = 0.75)+
  facet_wrap(~category, nrow = 2, scales = 'fixed')+
  scale_x_continuous(breaks = seq(1,7), labels = seq(1,7))+
  scale_y_continuous(breaks = seq(0.25,1,0.25), labels = seq(0.25, 1.01, 0.25))+
  labs(title = 'Snap % played in 2016-2020 per player per draft round',
       subtitle = 'Undrafted players not included',
       y = 'Snap %', x = NULL,
       caption = 'data: nflreadr')+
  theme_minimal()+
  theme(panel.background = element_rect(color = 'white'),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(color = 'white'),
        axis.text = element_text(size = 5.5),
        axis.title = element_text(size = 5.5),
        plot.title = element_text(size = 8),
        plot.subtitle = element_text(size = 6),
        plot.caption = element_text(size = 4.5))

ggsave('snapcounts by round.png', width = 4, height = 2.46, units = 'in', dpi = 1000)



