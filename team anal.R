library(librarian)
shelf(tidyverse, janitor, ffanalytics, fflr, ffscrapr, ggrepel, ggbeeswarm, zoo,
      Rglpk,ggthemes)
source(paste(getwd(),"/newFunct.R", sep=""))

week_scrape <- scrape_data_seperate(week = 2)

#espn stats
plinfo <- espn_spinfo(statsource = 0) %>%
  clean_espn_names(Player) %>%
  mutate(player_merge = case_when(player_merge == "a_j_green" ~ "aj_green",
                                  player_merge == "k_j_osborn"~ "kj_osborn",
                                  player_merge == "o_j_howard"~ "oj_howard",
                                  player_merge == "c_j_uzomah"~ "cj_uzomah",
                                  player_merge == "jeff_wilson" ~ "jeffery_wilson",
                                  TRUE ~ player_merge))

wp2 <-  week_scrape %>%
  week_projections(avg_type = "average") %>%
  mutate(player_merge = ifelse(player_merge == "taysom_hill_2", "taysom_hill", player_merge)) %>% 
  left_join(plinfo) %>% 
  mutate(across(c(Yards,RushYards,Carries, Targets, percentChange, PercentOwned, auctionValueAverage), as.numeric),
         yards = Yards + RushYards)


starters <- get_starters(wp2, waiver = T)[str_detect(get_starters(wp2, waiver = T) %>% names(), "Their Team")]

#starting 9 for week 2
wp2 %>% filter(player %in% starters)

# waiver analysis
wp2 %>% 
  filter(franchise_id %in% c(0,1)) %>%
  select(player,ft = franchise_id, pos, team, points, sd_pts,
         point_var,rank, 
         ava = auctionValueAverage, 
         PO = PercentOwned,
         PC = percentChange,
         week_score,
         Targets, Carries,
         yards)  %>% 
  group_by(ava) %>% 
  slice_max(points, n = 10) %>%
  arrange(desc(points)) %>%
  split(~pos)

#graph auction value to get a sense of positional weakness
wp2 %>% 
  filter(pos != "K",
         player %in% get_starters(wp2)) %>%
  group_by(franchise_name) %>%
  summarise(value = sum(points, na.rm = T))


wp2 %>%
  filter(pos != "K") %>% 
  group_by(pos,franchise_name) %>% 
  slice_max(auctionValueAverage, n = 2)  %>%
  summarise(value = sum(auctionValueAverage)) %>%
  group_by(pos) %>%
  mutate(value = (value - min(value))/ (max(value)- min(value)),
         myteam = ifelse(franchise_name == "Their Team", 1, 0) %>% as.factor()) %>%
  ggplot(aes(y = pos, x = value, color = myteam, label = franchise_name))+
  geom_point()+
  theme_clean()+
  geom_text_repel(direction = "x", size = 2,nudge_y = .5,angle = 75)
# third best
wp2 %>%
  filter(pos %in% c("RB", "WR", "TE")) %>% 
  group_by(pos,franchise_name) %>% 
  slice_max(auctionValueAverage, n = 3)  %>%
  slice_min(auctionValueAverage, n = 1)  %>%
  group_by(franchise_name, player, pos) %>% 
  summarise(value = sum(auctionValueAverage)) %>%
  group_by(pos) %>%
  mutate(value = (value - min(value))/ (max(value)- min(value)),
         myteam = ifelse(franchise_name == "Their Team", 1, 0) %>% as.factor()) %>%
  ggplot(aes(y = pos, x = value, color = myteam, label = player))+
  geom_point()+
  theme_clean()+
  geom_text_repel(direction = "x", size = 2,nudge_y = .5,angle = 75)
  
wp2 %>% 
  filter(franchise_id == 1) %>%
  select(player,ft = franchise_id, pos, team, points, 
         point_var,rank, 
         ava = auctionValueAverage, 
         PO = PercentOwned,
         PC = percentChange,
         week_score,
         Targets, Carries,
         yards) 
wp2 %>%
  filter(between(auctionValueAverage,1,2)) 
####

wp2 %>% left_join(ros) %>%
  drop_na() %>%
  #drop_na(fp) %>%
  get_opt_trade(pointvar = "points")

wp2 %>% 
  left_join(ros) %>%
  mutate(fp = as.numeric(fp)) %>%
  replace_na(list(fp = 0)) %>% 
  filter(franchise_id == 1, player %in% get_starters(wp2)) %>%
  summarise(av = sum(auctionValueAverage, na.rm = T),
            points = sum(fp))

