library(librarian)
shelf(fflr, ffscrapr)


ffs <- ffscrapr::espn_connect(2021,
                              762538,
                              espn_s2 = espn_s2, 
                              swid = SWID)
# get a draft dataframe
draft <- merge_draft(2023,1978410,proj_table = pt, SWID, espn_s2)




#see if anyone left on the table
ott <- draft %>% filter(franchise_id == 0) %>% group_by(pos) %>% slice_max(points_vor, n = 2)
draft %>% filter(franchise_name == "Team Beach") %>% bind_rows(ott) %>%
  select(player,pos, points_vor, franchise_id, rank, ecr_rank, espn_rank, uncertainty) %>%
  group_by(pos) %>% 
  arrange(desc(points_vor),.by_group = T)


starters <- get_starters(draft)
draft %>%  
  filter(player %in% starters) %>% 
  group_by(franchise_name) %>% 
  summarise(
    points = sum(points),
    risk = sum(uncertainty),
    pgm = sum(pgm)
  )
draft %>%
  mutate(risk = sd_pts^2) %>%
  filter(player %in% starters) %>%
  group_by(franchise_name) %>% 
  summarise(
    points = sum(points),
    risk = sum(risk) %>% sqrt(),
    pgm = sum(pgm)
  ) %>% ggplot(aes(x = risk,
                   y = points,
                   label = franchise_name,
                   color = franchise_name))+
  geom_point()+
  scale_color_discrete(guide = "none")+
  geom_label_repel(size = 2)+
  theme_bw()+
  labs(x = "Expected Variance", y = "Expected Points")



