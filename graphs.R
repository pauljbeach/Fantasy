##fflr exp
library(librarian)
shelf(easystats, tidyverse, ggdist, fflr, ggthemes, ggrepel, 
      geomtextpath, ggforce, ggridges,ggbeeswarm, ggpubr, fitdistrplus)
ffl_id(762538)
select <- dplyr::select
fflr::
fflr::best_roster()
ffpros::fp_rankings("ros-ppr-overall")
fflr::league_simulation()
scores <- fflr::tidy_scores(leagueHistory = T)

# Mutate----- 
scores %>% filter(matchupPeriodId <= 3) %>%
  group_by(abbrev) %>% 
  summarise(expectedWins = mean(expectedWins)*3) %>% 
  arrange(desc(expectedWins))



scorehist <- combine_history(tidy_scores,useMatchup = F)
sh <- scorehist %>%
  filter(points > 0, scoringPeriodId < 14) %>% 
  nest(data = -seasonId) %>% 
  mutate(fit = map(data,~
      dplyr::select(.,points) %>%
      as_vector()  %>%
      fitdist("norm")) %>% 
      map("estimate"),
      mean = map(fit,"mean"),
      sd = map(fit,"sd")) %>% 
  unnest(cols = c(data,mean,sd)) %>% 
  select(-fit) %>% 
  mutate(ew = pnorm(points, mean = mean, sd = sd)) %>% 
  group_by(seasonId,matchupId, teamId) %>% 
  mutate(pointsum = sum(points)) %>%
  group_by(seasonId, matchupId) %>% 
  mutate(isWinner = as.numeric(pointsum == max(pointsum))) %>%
  group_by(teamId,seasonId) %>% 
  summarise(expectedWins = mean(ew)*max(scoringPeriodId),
                wins = sum(isWinner),
                points = sum(points))


combine_history(league_standings) %>%
  filter(seasonId > 2018) 
scoresum <- sh %>% 
summarise(expectedWins = sum(expectedWins, na.rm = T)/length(seasonId),
          wins = sum(wins)/length(seasonId),
          wae = (wins-expectedWins)*length(seasonId),
          points = sum(points)/length(seasonId)) %>% 
  left_join(fflr::league_teams())



#------
scoresum %>%
  ungroup() |>
  mutate(luck = (wins - expectedWins),
         abbrev = toupper(abbrev)) %>%
  ggplot(aes(x = points, y = wins))+
  
  geom_smooth(method = "lm",
              formula = y~x,
              se = F, 
              color = "black",
              size = .5, 
              linetype = "dashed")+
  geom_point(aes(color = luck, fill = luck),color = "black", size = 2.5, shape = 21, alpha = .7)+
  geom_label_repel(aes(label = abbrev),
                   color = "grey25",
                  size = 4,
                  family = "Gill Sans",
                  label.size = NA,
                  label.padding = .01,
                  segment.curvature = .5,nudge_x = 1,
                  segment.alpha = .5,
                  min.segment.length = 0,
                  force_pull = 0,
                  )+
  geom_rangeframe()+
  labs(x = "POINTS PER SEASON",
       y = "WINS PER SEASON", 
       title = "FANTASY PERFORMACE",
       subtitle = "SINCE 2019")+
  scale_fill_viridis_c("WINS ABOVE EXPECTED \n (PER SEASON)", 
                        breaks = c(seq(-1.5,1.5,.5)), 
                        labels = c("",-1,"",0,"",1,""), alpha = .7)+
   guides(fill = "none")+
          # guide_colorbar(title.position = "left",
  #                               title.theme = element_text(size = 10, 
  #                                                          color ="grey25",
  #                                                          family = "Gill Sans"),
  #                               direction = "vertical",
  #                               title.hjust = .5,
  #                               barheight = unit(2,"cm"),
  #                               barwidth = unit(.3,"cm"),
  #                               nrow = 1, show.limits = F,
  #                               frame.colour = "black",
  #                               ticks.colour = "black", 
  #                                 ),
         # color = "none")+
  theme_tufte(base_family = "Gill Sans",base_size = 12)+
  theme(legend.position = c(.7,.2),
        axis.title = element_text(face ="plain", color = "black"),
        legend.title = element_text(face ="plain", color = "black"),
        axis.title.x = element_text(hjust = .2),
        axis.title.y = element_text(hjust = .7))

fscores <- combine_history(tidy_scores,useMatchup = F)
curid <- fscores %>% filter(seasonId == 2022) %>% distinct(teamId, name = abbrev) 


fscores %>%
  group_by(seasonId,matchupId, teamId) %>% 
  mutate(pointsum = sum(points)) %>%
  group_by(seasonId, matchupId) %>% 
  mutate(isWinner = as.factor(pointsum == max(pointsum))) %>%
  group_by(teamId) %>% 
  mutate(pointmean = median(points)) %>% 
  left_join(curid) %>%
  ungroup %>% 
  mutate(name = toupper(name),
         name = fct_reorder(name, points, mean)) %>% 
  filter(points > 0) %>%
  ggplot(aes(y = name, x = points))+
  labs(x = NULL,
       y = "POINTS",
       title = "WEEKLY POINT TOTAL DISTRIBUTION",
       subtitle = "WINS HIGHLIGHTED IN RED")+
  #geom_beeswarm(groupOnX = F, alpha = .2, aes(color = isWinner),dodge.width = 1)+
  # stat_halfeye(position = "dodge", 
  #              width = .8, 
  #              fill = "black",
  #              alpha = .3,
  #              point_interval = mean_qi)+
  stat_dots(side = "right",
            data = . %>% filter(isWinner == T),
            width = .8,
            xmin = 0,
            alpha = 1,
            aes(fill = isWinner, color = isWinner))+
  geom_text_repel(data = . %>% filter(points %in% c(slice_max(.,points, n = 3) %>%
                                                      select(points) %>% 
                                                      as_vector(),
                                                    slice_min(.,points, n = 3) %>%
                                                      select(points) %>% 
                                                      as_vector())),
                  aes(label = points),
                  size = 3,
                  family = "Gill Sans",
                  color = "grey",arrow = arrow(length = unit(.1,"cm")),
                  segment.curvature = -.5,
                  min.segment.length = 0,
                  nudge_x = 0,
                  nudge_y = .1,
  )+
  stat_dots(side = "left",data = . %>% filter(isWinner == F),
            width = .8,
             xmin = 0,
            alpha = 1,
            aes(fill = isWinner, color = isWinner))+

  stat_pointinterval(aes(alpha = .8),point_interval = mean_qi)+
  geom_rangeframe(inherit.aes = F, aes(y = name, x = points, group = name))+
  
  scale_color_metro(guide = "none")+
  scale_fill_metro(guide = "none")+
  guides(alpha = "none")+
  theme_tufte(base_family = "Gill Sans")+
  theme(panel.grid.major.x = element_line(color = "grey"),
        axis.text.y = element_text(hjust = 1, face = "bold"),
        aspect.ratio = 1.2)

ph <- nflreadr::load_player_stats(2000:2022) %>% as_tibble() %>% 
  filter(season_type == "REG")

sched_raw <- nflreadr::load_schedules()
sched <- sched_raw %>% 
  select(season, week,team = home_team,opp = away_team, score = home_score) %>% 
  bind_rows( sched_raw %>% select(season, week,
                             team = away_team,
                             opp = home_team, score = away_score)) %>% 
  mutate(across(c(team, opp),~case_when(.== "STL"~ "LA",
                          . == "SD" ~ "LAC",
                          . == "OAK" ~"LV",
                          T ~ .)))
phk <- nflreadr::load_player_stats(2000:2022,stat_type = "kicking") %>% as_tibble()%>% 
  mutate(fantasy_points_ppr = pat_made - pat_missed - fg_missed + 3*fg_made_30_39+ 4*fg_made_40_49+5*fg_made_50_59+5*fg_made_60_ ,
         position = "K") %>%
  filter(season_type == "REG") %>%
  select(season, week,recent_team  = team,position, player_display_name =player_name,fantasy_points_ppr, pat_made,fg_made)


# Def -----
def <- ph %>% group_by(week,season,team = recent_team) %>% 
  summarise(across(c(
              rushing_yards,
              sacks,
              passing_yards, 
              rushing_tds, 
              passing_tds,
              passing_2pt_conversions,
              rushing_2pt_conversions,
              rushing_fumbles_lost,
              interceptions),sum)) %>% 
  left_join(sched, by = c("team", "season", "week")) %>% 
  left_join(phk %>% group_by(season, week, team) %>% 
              summarise(fg_made = sum(fg_made), pat_made = sum(pat_made)),
            by = c("team", "season", "week")) %>% 
  mutate(total_yards = rushing_yards + passing_yards,
         pbin = as.numeric(as.character(cut(score,
                    breaks = c(0,1,7,14,28,35,46, Inf),
                    labels = c(5,4,3,1,-1,-3,-5)))),
         tybin = as.numeric(as.character(cut(total_yards,
                     breaks = c(0,100,200,300,350,400,450,500,550,Inf),
                     labels = c(5,3,2,1,-1,-3,-5,-6,-7)))),
         across(c(pat_made,fg_made), ~replace_na(.,0)),
         implied_def_points =score - (6*rushing_tds +6*passing_tds + 3*fg_made + pat_made)
         )
def2 <- def %>%
  left_join(def %>% select(season, week, "team" = opp, idp = implied_def_points),
            by = c("season", "week", "team")) %>% 
  mutate(fantasy_points_ppr = idp + pbin + tybin + 2*interceptions + 2*rushing_fumbles_lost+sacks,
         position = "DST") %>% 
  select(season, week, 
         player_display_name = opp, recent_team = opp,
         position, 
         fantasy_points_ppr,
         total_yards,score, opp = team)

# top players ----
top_players <- ph %>%
  filter(position %in% c("QB", "RB", "WR", "TE", "DST"),
         season < 2022) %>% 
  bind_rows(phk)%>%
  bind_rows(def2) %>% 
  group_by(player_display_name,season, position, recent_team) %>% 
  summarise(points = sum(fantasy_points_ppr, na.rm = T),
            sd = sd(fantasy_points_ppr, na.rm = T),
            games = n()) %>% 
  group_by(position) %>% 
  slice_max(points, n = 3) %>% 
  select(player_display_name, points, season)

top_s_players <- ph %>% 
  filter(position %in% c("QB", "RB", "WR", "TE", "DST"),
         season < 2022) %>% 
  bind_rows(phk) %>%
  bind_rows(def2) %>% 
  group_by(player_display_name,season, position, recent_team) %>% 
  summarise(points = sum(fantasy_points_ppr, na.rm = T),
            sd = sd(fantasy_points_ppr, na.rm = T),
            games = n()) %>% 
  group_by(position, season) %>% 
  slice_max(points, n = 1) %>% 
  select(player_display_name, points, season) %>% 
  group_by(position) %>% 
  slice_max(points, n = 3)


# create df graph ----
phs <- ph %>% 
  filter(position %in% c("QB", "RB", "WR", "TE")) %>% 
  bind_rows(phk) %>% 
  bind_rows(def2) %>% 
  filter(season < 2022) %>% 
  group_by(player_display_name,season,recent_team, position) %>% 
  summarise(points = sum(fantasy_points_ppr, na.rm = T),
            sd = sd(fantasy_points_ppr, na.rm = T),
            games = n()) %>% 
  split(~position+season) %>% 
  map2_df(.,rep(c(14,14,14,35,14,35),22),
       ~ ungroup(.x) %>% 
         slice_max(points, n = .y) %>% 
         arrange(desc(points))) %>% 
  mutate(position = fct_reorder(position,points,mean,.desc = T)) 

# raincloud ----
phs %>% 
  ggplot(aes(x = points, y = position, fill = position, color = position))+
  stat_halfeye(position = "dodge",height = 1)+
  stat_dots(side = "left", height = .8)+
  theme_tufte(base_family = "Gill Sans", base_size = 13)+
  theme(axis.text.y = element_text(face = "bold"))+
  geom_rangeframe(inherit.aes = F, aes(x = points, y = position))+
  guides(fill = "none", color = 'none', alpha = "none")+
  labs(y = NULL, 
       x = "TOTAL POINTS",
       title = "SEASON TOTAL DISTRIBUTION OF LIKELY FANTASY STARTERS",
       subtitle  = "2000 - 2021 SEASONS")+
  stat_pointinterval(color = "black", aes(alpha = .8))+
  geom_text_repel(data = top_players,inherit.aes = F,
                  size = 2, 
                  family = "Gill Sans",
                  segment.curvature = .4,
                  segment.alpha = .3,
                  nudge_y = .5,
                  nudge_x = 20,
                  aes(label = paste0(player_display_name, "\n", season, " (", points, ")"),
                      x = points, y = position))+
  scale_color_metro(aesthetics = c("color", "fill"))



# ribbon plot ---- 
ribg <- phs %>% 
  filter(position %in% c("QB", "RB", "WR", "TE", "DST", "K")) %>% 
  group_by(season,position) %>% 
  summarise(mpoints = median(points, na.rm = T),
            minpoints = min(points),
            maxpoints = max(points))%>% 
  nest(data = -position) %>% 
  mutate(ma = map(data,lm,formula = maxpoints ~ season) %>% 
           map(fitted),
         mlo = map(data,lm,formula = mpoints ~ season) %>% 
           map(fitted),
         mi = map(data,lm,formula = minpoints ~ season) %>% 
           map(fitted)) %>% 
  unnest(c(data, ma, mi, mlo)) %>% 
  ungroup() %>% 
  mutate(position = fct_relevel(position,"RB", "WR", "QB", "TE", "DST", "K")) %>% 
  left_join(top_s_players)

 ribg %>% 
  ggplot(aes(y = mlo, x = season, color = position, fill = position))+
  labs(x = NULL, y = "POINTS",
       title = "POINT TOTAL EVOLUTION AMONG LIKELY STARTERS",
     subtitle = "Trendlines indicate predicted range by position \nSeason leaders highlighted",
     caption = "Levels defined for a 14 team PPR league.\nStarters are composed of the top 14 quarterbacks, tight ends, and defense \nand the top 35 wide recievers and running backs")+
  theme_tufte(ticks = F, base_family = "Gill Sans", base_size = 15)+
  theme(panel.spacing.x = unit(3, "mm"),
        strip.placement = "outside",
        axis.title.y = element_text(hjust = .8),
        strip.text = element_text(face = "bold"),
        panel.grid.major.y = element_line(color = "grey", linetype = "dotted"),
        panel.ontop = F,plot.caption = element_text(hjust = 0,size = 10)
  )+
  
  geom_text_repel(data = .%>% filter(position %in% c("RB", "DST"), !is.na(player_display_name)), 
                  aes(label = player_display_name,x = season, y = maxpoints),
                  inherit.aes = F,
                  size = 2.5,
                  family = "Gill Sans",
                  min.segment.length = 0,
                  force_pull = .2,
                  force = 2,
                  segment.curvature = .5,
                  nudge_x = 10,hjust = -1,
                  nudge_y = 0,
                  segment.alpha = .5
  )+
   geom_text_repel(data = . %>% filter(!position %in% c("RB", "DST"),
                                       !is.na(player_display_name)), 
                   aes(label = player_display_name,x = season, y = maxpoints),
                   inherit.aes = F,
                   size = 2.5,
                   family = "Gill Sans",
                   min.segment.length = 0,
                   force_pull = .2,
                   force = 2,
                   segment.curvature = -.5,
                   direction = "y",nudge_x = -10,hjust = 1,
                   nudge_y = 10,
                   segment.alpha = .5
   )+
  geom_textline(label = "Replacement Level", 
                size = 4,
                family = "Gill Sans",
                vjust = 1,
                color = "black",
                alpha = .6,
                data = . %>% filter(position == "RB"),
                inherit.aes = F, aes(x = season, y = mi, color = position))+
  geom_textline(label = "Median Starter", 
                size = 4,
                family = "Gill Sans",color = "black", alpha = .6,
                vjust = 1,
                data = . %>% filter(position == "RB"),
                inherit.aes = F, aes(x = season, y = mlo, color = position))+
  
  geom_line(size = 1)+
  
  geom_point2(aes(y = maxpoints))+
  geom_textline(label = "League Best", 
                 size = 4,
                 alpha = .5,
                 family = "Gill Sans",
                 color = "black", 
                alpha = .6,
                text_only = T,
                 vjust = 1,
                 data = . %>% filter(position == "RB"),
                 inherit.aes = F, aes(x = season, y = ma, color = position))+
   geom_ribbon(aes(ymin = mi,ymax = ma, color = position), alpha = .1)+
  
  geom_rangeframe(sides = "b",inherit.aes = F, aes(y = mpoints, x = season))+
  
  guides(color = "none", fill = "none")+
  scale_x_continuous(breaks = c(2000,2010,2020), labels = c("'00", "'10", "'20"))+
  facet_wrap(~factor(position, levels = c("RB", "QB", "WR", "TE", "DST", "K")),nrow = 1,
             strip.position = "bottom",
             labeller = as_labeller(setNames(c("Defense",
                                               "Kicker",
                                               "Quarterback",
                                               "Running \n Back",
                                               "Tight \n End",
                                               "Wide \n Receiver"
                                               ),
                                             sort(unique(ribg$position)))))
  
  


### ----
phs %>% 
  ggplot(aes(x = factor(season), y = points, color = position))+
  geom_tufteboxplot()+
  labs(x = NULL)+
  scale_x_discrete(breaks = c("2000","2021"))+
  theme_tufte()+
  theme(panel.spacing.x = unit(5, "mm"),
        strip.placement = "outside",
  )+
  geom_rangeframe(sides = "b", color = "black")+
  facet_grid(~position,switch = "both")+
  guides(color = "none")

