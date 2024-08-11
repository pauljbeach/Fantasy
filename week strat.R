library(librarian)
shelf(tidyverse, janitor, FantasyFootballAnalytics/ffanalytics, fflr, ffscrapr, ggrepel, ggbeeswarm, zoo,
      Rglpk,ggthemes)
source(paste(getwd(),"/newFunct.R", sep=""))
week_scrape <-scrape_data(season = 2022,
                          src = c("FantasyPros","FantasySharks", 
                                  "NFL", "RTsports"),
                          week = 3,
                          pos = c("QB", "RB", "WR", "TE", "K", "DST"))
ffs <- espn_connect(id = 762538, season = 2022)

# Scoring Settings#####
vor14 <-
  c(QB = 13, RB = 38, WR = 47, TE = 13, K = 2, DST = 5, DL = 10, LB = 10, DB = 10)
src_weights <- 
  c(CBS = 0.344,
    Yahoo = 0.400,
    ESPN = 0.329,
    NFL = 0.329,
    FFToday = 0.379,
    NumberFire = 0.322,
    FantasyPros = 0.322,
    FantasySharks = 0.327,
    FantasyFootballNerd = 0.321,
    Walterfootball = 0,
    RTSports = 0.330,
    FantasyData = 0.428,
    Fleaflicker = 0.428
  )
ppr <- 
  list(
    pass = list(
      pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.04, pass_tds = 4,
      pass_int = -2, pass_40_yds = 0,  pass_300_yds = 0, pass_350_yds = 0,
      pass_400_yds = 0
    ),
    rush = list(
      all_pos = TRUE,
      rush_yds = 0.1,  rush_att = 0, rush_40_yds = 0, rush_tds = 6,
      rush_100_yds = 0, rush_150_yds = 0, rush_200_yds = 0),
    rec = list(
      all_pos = TRUE,
      rec = 1, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 0,
      rec_150_yds = 0, rec_200_yds = 0
    ),
    misc = list(
      all_pos = TRUE,
      fumbles_lost = -2, fumbles_total = 0,
      sacks = 0, two_pts = 2
    ),
    kick = list(
      xp = 1.0, fg_0019 = 3.0,  fg_2029 = 3.0, fg_3039 = 3.0, fg_4049 = 4.0,
      fg_50 = 5.0,  fg_miss = 0.0
    ),
    ret = list(
      all_pos = TRUE,
      return_tds = 6, return_yds = 0
    ),
    idp = list(
      all_pos = TRUE,
      idp_solo = 1, idp_asst = 0.5, idp_sack = 2, idp_int = 3,  idp_fum_force = 3,
      idp_fum_rec = 2,  idp_pd = 1, idp_td = 6,  idp_safety = 2
    ),
    dst = list(
      dst_fum_rec = 2,  dst_int = 2, dst_safety = 2, dst_sacks = 1, dst_td = 6,
      dst_blk = 2, dst_ret_yds = 0, dst_pts_allowed = 0
    ),
    pts_bracket = list(
      list(threshold = 0, points = 10),
      list(threshold = 6, points = 7),
      list(threshold = 20, points = 4),
      list(threshold = 34, points = 0),
      list(threshold = 99, points = -4)
    )
  )

#####


weekdf <- week_projections(week_scrape = week_scrape)

#projection
weekdf %>% 
  filter(franchise_id %in% c(1,get_opponent(ffs,)), player %in% get_starters(weekdf)) %>%
  group_by(franchise_name) %>% 
  summarise(points = sum(points),
            sd = sqrt(sum(sd_pts^2)))

#free agent comparison with team
weekdf %>% filter(franchise_id %in% c(1)) %>%
  rbind(weekdf %>% 
          filter(franchise_id == 0) %>% 
          group_by(pos) %>% 
          slice_max(points, n = 2)) %>% 
  arrange(desc(points_vor)) %>% 
  select(player, franchise_id, everything()) %>% view()

#top projected free agents 
weekdf %>% filter(franchise_id == 0) %>% arrange(desc(points)) %>% split(~pos)



#####
ffs2 <- espn_connect(league_id = 1978410, season = 2022,espn_s2 = espn_s2, swid = SWID)
wdf2 <- week_projections(week_scrape = week_scrape, id = 1978410, espn_s2 = espn_s2, swid = SWID)



wdf2 %>% 
  filter(franchise_id %in% c(6,get_opponent(ffs2,3,fid = 6)), 
         player %in% get_starters(wdf2)) %>%
  group_by(franchise_name) %>% 
  summarise(points = sum(points),
            sd = sqrt(sum(sd_pts^2)))

#free agent comparison with team
wdf2 %>% filter(franchise_id %in% c(6)) %>%
  rbind(wdf2 %>% 
          filter(franchise_id == 0) %>% 
          group_by(pos) %>% 
          slice_max(points, n = 2)) %>% 
  arrange(desc(points_vor)) %>% 
  select(player, franchise_id, everything()) %>% view()

#top projected free agents 
weekdf %>% filter(franchise_id == 0) %>% arrange(desc(points)) %>% split(~pos)
