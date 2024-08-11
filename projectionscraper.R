

library(librarian)
shelf(tidyverse, janitor, ffanalytics, fflr, ffscrapr, ggrepel, ggbeeswarm, zoo)
#setwd("/Users/paul/Documents/R/fantasy")
source(paste(getwd(),"/Functions.R", sep=""))


my_scrape <- scrape_data(season = 2022, 
                         week = 0,
                         pos = c("QB", "RB", "WR", "TE", "K", "DST"))


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

ecr <- scrape_ecr(position = "Overall",rannk_period = "draft", rank_type = "PPR") %>%
  rename(overall_ecr = avg, sd_ecr = std_dev)
pt <- projections_table(my_scrape,
                        scoring_rules = ppr,
                        src_weights = src_weights,
                        vor_baseline = vor14
                        ) %>% 
  add_adp() %>% 
  left_join(ecr) %>% 
  add_uncertainty() %>%
  add_player_info() %>%
  filter(avg_type == "robust")

injury_raw <- read.csv(file = "fullreport.csv") %>% clean_names() %>% as_tibble()

injury <- injury_raw %>%
  mutate(pos = word(player, -2),
         injury_count = str_extract(historic_stats,"[[:digit:]]+"),
         season_injury = str_extract(season_injury,"[[:digit:]]+")) %>%
  separate(player,into = c("player", "extra"), sep =",") %>%
  select(-c(extra, historic_stats, injury_probabilities))

esr <- espn_rank(2022,762538)

optimizeData <- pt %>% 
  mutate(player = paste(first_name, last_name),
         player = case_when(player %in% c("AJ Dillon") ~ "A.J. Dillon",
                            player == "Irv Smith Jr." ~ "Irv Smith",
                            player == "DK Metcalf" ~ "D.K. Metcalf",
                            player == "KJ Hamler" ~ "K.J Hamler",
                            T ~ player)) %>%
  select(player, everything()) %>% 
  left_join(y = injury, by = c("player", "pos")) %>%
  left_join(esr) %>%
  mutate(season_injury = as.numeric(season_injury),
         season_injury = ifelse(is.na(season_injury), 10, season_injury),
         pgm = ifelse(is.na(projected_games_missed), 1, projected_games_missed),
         name = paste(last_name, first_name)) %>%
  arrange(rank) %>%
  drop_na(uncertainty, points, sd_pts, pgm, pos, ecr_rank, espn_rank)
benchData <- 
pt %>% filter(pos == "DST")
    mutate(player = paste(first_name, last_name),
         player = case_when(player %in% c("AJ Dillon") ~ "A.J. Dillon",
                            player == "Irv Smith Jr." ~ "Irv Smith",
                            player == "DK Metcalf" ~ "D.K. Metcalf",
                            player == "KJ Hamler" ~ "K.J Hamler",
                            T ~ player)) %>%
  select(player, everything()) %>% 
  left_join(y = injury, by = c("player", "pos")) %>%
  left_join(esr) %>%
  mutate(season_injury = as.numeric(season_injury),
         season_injury = ifelse(is.na(season_injury), 10, season_injury),
         pgm = ifelse(is.na(projected_games_missed), 1, projected_games_missed),
         name = paste(last_name, first_name)) %>%
  arrange(rank) %>%
  drop_na(uncertainty, points, sd_pts, pos, ecr_rank, espn_rank)

