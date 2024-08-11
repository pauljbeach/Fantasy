
library(librarian)
shelf(tidyverse, ffscrapr,fflr)
ffs <- espn_connect(league_id = 762538, season = 2024)
update.packages(ask = F)


fflr::ffl_id(762538)
ids <- ff_franchises(ffs) %>% select(franchise_id, franchise_abbrev)

scores_raw <- fflr::tidy_scores(leagueHistory = T) %>% bind_rows()
fflr::transaction_counter()
scores_raw %>% 
  group_by(teamId) %>% 
  mutate(cs = cumsum(totalPoints),
         time = 1:n()) %>% drop_na() %>% 
    ggplot(aes(x = time, y = cs, color = as.factor(teamId)))+
           geom_line()
scores <- scores_raw %>% 
  filter(seasonId > 2018) %>% 
  left_join(ids, by = c("teamId" = "franchise_id")) %>%
  group_by(teamId, franchise_abbrev) %>% 
  summarise(score = sum(totalPoints),
            wins = sum(as.numeric(isWinner)),
            season = n_distinct(seasonId)) %>% 
  mutate(across(c(score,wins), ~./season)) %>% 
  ungroup() %>%
  drop_na() %>% 
  mutate(teamId = reorder(teamId,score, max))
  
scores %>%
  ggplot(aes(y = wins, x = score, color = teamId, label = franchise_abbrev)) +
  geom_point()+
  stat_smooth(inherit.aes = F, aes(x = score, y = wins),
              method = "lm", formula = y~x, se = F, linetype = "dashed", color = "black")+
  scale_x_continuous("Points Per Season")+
  scale_y_continuous("Wins Per Season")+
  geom_text_repel()+
  scale_color_pander(guide = "none")+
  theme_pander(gm =F,base_size = 14)
         