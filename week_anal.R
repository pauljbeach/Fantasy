#post week analyis 
library(ggpmisc)
library(XML)
#for post week evaluation
library(rvest)
ffanalytics::scrape_data()
nfl <- ffanalytics:::scrape_nfl(pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                        week = 1,draft = F)
nfl$K <- nfl$K %>% replace_na(list(fg_0019 = 0)) %>%
  mutate(fg = fg_0019 + fg_2029 + fg_3039 + fg_4049)

fap <- ffanalytics:::scrape_fantasypros(pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                                 week = 1,draft = F)

cbs <- ffanalytics:::scrape_cbs(pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                         week = 1,draft = F)

fs <- ffanalytics:::scrape_fantasysharks(pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                                   week = 1,draft = F)



nf <- ffanalytics:::scrape_numberfire(pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                                       week = 1,draft = F)


proj_week <- map2(list(nf, fs,cbs,fap,nfl), list("nf", "fs", "cbs", "fap", "nfl"), ~
.x %>% projections_table(avg_type = "average") %>%
  add_player_info() %>%
  mutate(player = paste(first_name, last_name),
         player = str_remove_all(player, "[:punct:]"),
         player = ifelse(pos == "DST", paste(last_name, "DST"), player),
         player_merge = make_clean_names(player)) %>%
  select(player_merge, points) %>%
  rename(!!.y := points)
) %>% reduce(left_join, by = "player_merge") %>%
  left_join(espn_weekscores(week = 1) %>% 
              clean_espn_names(p_name = Player)) %>%
  left_join(espn_weekscores(week = 1, statsource = 1) %>% 
              clean_espn_names(p_name = Player) %>% 
              rename(espn = week_score))
projections_table(fs, avg_type = "average") %>% add_player_info()




reg <- draft %>% 
  left_join(proj_week, by = "player_merge") %>%
  select(player,pos,week_score, nf, fs, cbs,fap, nfl,espn,
         rank,age,exp,team,sd_pts,uncertainty,overall_ecr, player_merge)
#get accuracy
acc = matrix(nrow =5, ncol = 6)
post <- c("QB", "RB", "WR", "TE", "K")
rownames(acc) <- post
colnames(acc) <- c("nf", "fs", "cbs", "fap", "nfl", "espn")

for(i in seq_along(post)){
acc[i,] <- reg %>% select(nf,fs,cbs,fap,nfl,espn) %>%
  map(~lm(data = reg, reg$week_score ~ .x,subset = reg$week_score > 5 & reg$pos == post[i])) %>%
  map(summary) %>%
  map_dbl("r.squared")
}
acc

reg %>% filter(week_score > 0) %>%
  select(-c(player, player_merge, team, fap))%>%  
  lm(data = ., formula = week_score ~ .+0) %>% summary()
  
#plot to check
reg %>% filter(week_score > 0) %>%
ggplot(aes(x = nf, y = week_score))+
  geom_point()+
  stat_poly_eq()
draft

reg %>% filter(week_score > 0)%>%
glm(data = ., week_score ~ nf+fs+cbs+fap+nfl+espn+0,family =Gamma) %>% summary()


avg_proj <- reg %>% 
  select(player_merge, nf,fs, cbs, fap, nfl, espn) %>% 
  pivot_longer(-player_merge) %>%
  group_by(player_merge) %>%
  mutate(weight = case_when(name == "nf" ~ acc[names(acc) == "nf"]^2,
                            name == "fs" ~ acc[names(acc) == "fs"]^2,
                            name == "cbs" ~acc[names(acc) == "cbs"]^2,
                            name == "fap" ~acc[names(acc) == "fap"]^2,
                            name == "nfl" ~acc[names(acc) == "nfl"]^2,
                            name == "espn"~acc[names(acc) == "espn"]^2)) %>%
  summarise(wmean = weighted.mean(value, w = weight,na.rm = T),
            mean = mean(value, na.rm = T))
reg <- reg %>% left_join(avg_proj)

reg %>% select(nf,fs,cbs,fap,nfl,espn, mean, wmean) %>%
  map(~lm(data = reg, reg$week_score ~ .x, subset = reg$week_score > 0)) %>%
  map(summary) %>%
  map_dbl("r.squared")
reg %>% filter(player == "Aaron Rodgers")

reg %>% 
  mutate()
map_dfc(names(acc), ~ reg[[.x]]*acc[names(acc) == .x]) %>%rowSums(na.rm = T)

reg %>%
  filter(week_score > 0) %>%
  ggplot(aes(x = wmean, y = week_score))+
  geom_point()+
  stat_smooth(method = "lm")+
  stat_poly_eq()+
  geom_abline()
rel_perf <- reg %>% mutate(rel_perf = week_score-wmean) %>%
  select(player_merge, rel_perf, week_score) %>%
  arrange(rel_perf %>% desc())

  reg %>% mutate(rel_perf = week_score-wmean) %>%  select(player_merge, rel_perf, week_score, wmean) %>%view()
