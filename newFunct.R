#New Function Repository
espn_s2 <-"AEBO0h2X7DWyhkqmH%2FZZfild9JwNKtwrWdWH5g%2BCbBR9Z7m8ewcDrKFeVvTJUvW1BiJ6tW%2BdWSTn8C%2FAzHVDZtItwU0Y%2Be8g1DAv%2BbclOW9JfEclZ%2F7dQtRB0oo9zIwjJHqhXKakr0yYlR6Yl%2BAr%2FLtUQbahsriRCm5Ii2RO4ZdGf%2BAOCjmOHJawF%2FBY9wX2vPIEDO2xG2Dg9bwoY2CV2eXAC40rCBCu%2FT%2BCGCND42VWBCo6db7EX0jcy9R6wuWJzCCuhDqegwU3JIomXUnJ7IXs"
SWID <-  "{1505CFD7-906A-426B-85CF-D7906AD26B75}"
#get kona player info from espn api
espn_proj <- function(season, id){
  conn <- espn_connect(season,id)
  
  xff <- xff <- list(players = list(
    limit = 1000,
    sortPercOwned = 
      list(sortAsc = FALSE,
           sortPriority = 1),
    filterStatsForTopScoringPeriodIDs = 
      list(value = 2,
           additionalValue = c(paste0("00", conn$season)))
  )) %>%
    jsonlite::toJSON(auto_unbox = TRUE)
  pls <- espn_getendpoint(conn, view = "kona_player_info", x_fantasy_filter = xff)
  return(pls)
}

#get current scores of players by week, will need to check 
#stat source = 1 seems to be projections, =0 is real data
espn_weekscores <- function(season = 2022, id = 762538, week = 1, statsource = 0){
  pls <- espn_proj(season,id)
  test<- matrix(nrow = 1, ncol = 2)
  test2 <- matrix(nrow = 1000, ncol = 2)
  for(j in 1:1000){
    loc <- pls$content$players[[j]]$player
    
    
    for(i in 1:length(loc$stats)){
      if(loc$stats[[i]]$seasonId == 2022 & 
         loc$stats[[i]]$scoringPeriodId == week &
         loc$stats[[i]]$statSourceId == statsource){
        test[1,2] <- loc$stats[[i]]$appliedTotal
        test[1,1] <- loc$fullName
      }
    }
    test2[j,] <- test
  }
  colnames(test2) <- c("Player", "week_score")
  test2 %>% as_tibble() %>% mutate(week_score = as.numeric(week_score)) %>% distinct(Player, .keep_all = T)
}

espn_spinfo <- function(season = 2022, id = 762538, statsource = 0, week = 0){
  pls <- espn_proj(season,id)
  test<- matrix(nrow = 1, ncol = 12)
  test2 <- matrix(nrow = 1000, ncol = 12)
  for(j in 1:1000){
    loc <- pls$content$players[[j]]$player
    for(i in 1:length(loc$stats)){
      if(loc$stats[[i]]$seasonId == 2022 & 
         loc$stats[[i]]$scoringPeriodId == week &
         loc$stats[[i]]$statSourceId == statsource){
        test[1,2] <- loc$stats[[i]]$appliedTotal
        test[1,1] <- loc$fullName
        test[1,12] <- loc$defaultPositionId
        test[1,3] <- loc$ownership$percentChange %>% round(2)
        test[1,4] <- loc$ownership$percentOwned %>% round(2)
        test[1,5] <- loc$ownership$auctionValueAverage %>% round(2)
        test[1,6] <- ifelse(is.null(loc$injuryStatus),"INACTIVE",loc$injuryStatus)
        test[1,7] <- ifelse(is.null(loc$stats[[i]]$stats$`53`),0,loc$stats[[i]]$stats$`53`) #rec
        test[1,8] <- ifelse(is.null(loc$stats[[i]]$stats$`58`),0,loc$stats[[i]]$stats$`58`) #tar
        test[1,9] <- ifelse(is.null(loc$stats[[i]]$stats$`42`),0,loc$stats[[i]]$stats$`42`) #recyard
        test[1,11] <- ifelse(is.null(loc$stats[[i]]$stats$`24`),0,loc$stats[[i]]$stats$`24`) #rushyard
        test[1,10] <- ifelse(is.null(loc$stats[[i]]$stats$`23`),0,loc$stats[[i]]$stats$`23`) #carry
        
      }
    }
    test2[j,] <- test
  }
  colnames(test2) <-
    c("Player",
      "week_score",
      "percentChange",
      "PercentOwned",
      "auctionValueAverage",
      "InjuryStatus",
      "Receptions",
      "Targets",
      "Yards",
      "Carries",
      "RushYards",
      "pos")
  test2 %>% 
    as_tibble() %>% 
    mutate(week_score = as.numeric(week_score),
           pos = case_when(pos == 1 ~ "QB",
                           pos == 2 ~ "RB",
                           pos == 3 ~ "WR",
                           pos == 4 ~ "TE",
                           pos == 5 ~ "K",
                           pos == 16 ~ "DST",
                           T ~ pos))
}

clean_espn_names <- function(data, p_name = player){
  p_name <- enquo(p_name)
  data %>%
    mutate(
      p_name = str_remove_all(!!p_name, "[:punct:]"),
      p_name = str_remove(!!p_name, regex("Jr")),
      player_merge = make_clean_names(p_name),
      player_merge = case_when(
        player_merge == "gabe_davis" ~ "gabriel_davis",
        player_merge == "allen_robinson_ii" ~ "allen_robinson",
        player_merge == "joshua_palmer" ~ "josh_palmer",
        player_merge == "irv_smith" ~ "irv_smith_jr",
        player_merge == "melvin_gordon_iii" ~ "melvin_gordon",
        player_merge == "mark_ingram_ii" ~ "mark_ingram",
        TRUE ~ player_merge
      )
    ) %>%
    dplyr::select(-c(!!p_name, p_name))
}

clean_ff_names <- function(data){
  add_player_info(data) %>% 
    mutate(player = paste(first_name, last_name),
           player = str_remove_all(player, "[:punct:]"),
           player = ifelse(pos == "DST", paste(last_name, "DST"), player),
           player_merge = make_clean_names(player))
}
get_hist_trends <- function(){
  raw <- read.csv("hist_raw.csv")
  ar <- raw %>% filter(ppr_fantasy_points > 0)
  as.numeric(raw$ppr_fantasy_points > 0) %>% mean()
  u <- ar$ppr_fantasy_points%>% mean(na.rm = T)
  gam_par <- raw %>%
    mutate(pos = case_when(pos %in% c("HB", "FB") ~ "RB",
                           str_detect(pos, "WR") ~ "WR",
                           T ~ pos)) %>% 
    group_by(player, pos) %>% 
    filter(mean(ppr_fantasy_points) > 3,
           ppr_fantasy_points > 0,
           n() > 5,
           pos %in% c("WR", "RB", "QB", "K", "TE")) %>% 
    mutate(pprmean = mean(ppr_fantasy_points),
           pbin = cut(pprmean, breaks = c(2:20, Inf), labels = c(as.character(3:20), "20+")),
           dist_group = paste0(pos,pbin)) %>% 
    group_by(dist_group) %>% 
    summarise(hist_point_bin= mean(ppr_fantasy_points),
              hist_sd = sd(ppr_fantasy_points),.groups = "keep")
  
  
  
  raw_yp <- raw %>% 
    mutate(pos = case_when(pos %in% c("HB", "FB") ~ "RB",
                           str_detect(pos, "WR") ~ "WR",
                           TRUE ~ pos)) %>%
    filter(ppr_fantasy_points > 0,
           pos %in% c("WR", "RB", "QB", "K", "TE")) %>%
    group_by(player,year, pos) %>% 
    filter(mean(ppr_fantasy_points) > 4) %>% 
    summarise(mean = mean(ppr_fantasy_points),
              sd = sd(ppr_fantasy_points))
  
  
  p_sd <- raw_yp %>% 
    filter(year >= 2019) %>% 
    group_by(player, pos) %>% 
    summarise(points = mean(mean),
              sd_pts = mean(sd)) %>%
    drop_na() %>% 
    arrange(desc(points)) %>%
    mutate(
      player = str_remove_all(player, "[:punct:]"),
      player = str_remove(player, regex("Jr")),
      player_merge = make_clean_names(player),
      player_merge = case_when(
        player_merge == "gabe_davis" ~ "gabriel_davis",
        player_merge == "allen_robinson_ii" ~ "allen_robinson",
        player_merge == "joshua_palmer" ~ "josh_palmer",
        player_merge == "irv_smith" ~ "irv_smith_jr",
        player_merge == "melvin_gordon_iii" ~ "melvin_gordon",
        player_merge == "mark_ingram_ii" ~ "mark_ingram",
        TRUE ~ player_merge
      )
    ) %>% 
    ungroup () %>% 
    dplyr::select(player_merge, hist_points = points, hist_sd_pts = sd_pts)
  return(list("gam_par" = gam_par, "p_sd" = p_sd))
}

get_starters <- function(data ,pointvar = "points", waiver = F, random = F, name_var = "player"){
  teams <- data %>% 
    split(~franchise_name)
  teams[["Free Agent"]] <- NULL

  if(waiver == T){
    waivers <- data %>% 
      filter(franchise_id == 0) %>%
      group_by(pos) %>% 
      slice_max(points, n = 3)
    pos_var <- "pos"
    teams <- map(teams, ~rbind(.x, waivers))
  }
  df <- map(teams, ~{
    data<-.x
    obj <- if(random == T) runif(length(data[[pointvar]]),0,1) else data[[pointvar]]
    A <- rbind(
      rep(1, length(data[[pos_var]])),
      as.numeric(data[[pos_var]] == "QB"),
      as.numeric(data[[pos_var]] == "K"), # num K
      as.numeric(data[[pos_var]] == "DST"), # num QB
      as.numeric(data[[pos_var]] == "WR"), # num DST
      as.numeric(data[[pos_var]] == "RB"), # num K
      as.numeric(data[[pos_var]] == "TE") # num TE
    )
    dir <- c(rep("==",4),
             rep(">=",3))
    
    b <- c(9,1,1,1,2,2,1)
    var.types <- rep("B",length(data[[pos_var]]))
    sol <- Rglpk_solve_LP(obj = obj, mat = A, dir = dir, rhs = b,types = var.types, max = TRUE)
    sol$players <- data[[name_var]][sol$solution == 1]
  }) %>% unlist()
  
  return(df)
}
get_7_starters <- function(data ,pointvar = "proj_fpts",name_var = "player_merge"){
  teams <- data %>% 
    split(~franchise_id)
  teams[["free agent"]] <- NULL
  
  df <- map(teams, ~{
    data<-.x
    obj <- data[[pointvar]]
    A <- rbind(
      rep(1, length(data[[pos_var]])),
      as.numeric(data[[pos_var]] == "QB"), # num QB
      as.numeric(data[[pos_var]] == "WR"), # num DST
      as.numeric(data[[pos_var]] == "RB"), # num K
      as.numeric(data[[pos_var]] == "TE") # num TE
    )
    dir <- c(rep("==",2),
             rep(">=",3))
    
    b <- c(7,1,2,2,1)
    var.types <- rep("B",length(data[[pos_var]]))
    sol <- Rglpk_solve_LP(obj = obj, mat = A, dir = dir, rhs = b,types = var.types, max = TRUE)
    sol$players <- data[[name_var]][sol$solution == 1]
  }) %>% unlist()
  
  return(df)
}


#need a ros points and some trade or auction value
get_opt_team <- function(data ,
                         pointvar = "points",
                         convar = "auctionValueAverage",
                         name_var = "player_merge",
                         hnd = 5){
  data[[convar]] <- ifelse(data[[convar]] > 0, data[[convar]], 0)
  starters <- get_7_starters(data)
  pos_var <- "pos"
  var.types <- rep("B", length(data[[pos_var]]))
    obj <- data[[convar]]
    A <- rbind(
      rep(1, length(data[[pos_var]])), #total players
      as.numeric(data[[name_var]] %in% starters), #but 7 starters
      as.numeric(data[[pos_var]] == "QB"), #num QB
      as.numeric(data[[pos_var]] == "WR"), # num WR
      as.numeric(data[[pos_var]] == "RB"), # num RB
      as.numeric(data[[pos_var]] == "TE"), # num TE
      ifelse(data$player_merge %in% starters, data[[convar]], 0) #
    )
    dir <- c(rep("==",3),
             rep(">=",3),
             "<="
             )
    
    b <- c(10,7,1,2,2,1,
           sum(data[[convar]][data$franchise_id == 1 & data$player_merge %in% starters], na.rm = T) + hnd
           )
    sol <- Rglpk_solve_LP(obj = obj, mat = A, dir = dir, rhs = b,types = var.types, max = TRUE)
    sol$players <- data[[name_var]][sol$solution == 1]
    return(list("players" = sol$players,
                "points" = sum(data[[pointvar]][sol$solution ==1]),
                "AV" = sum(data[[convar]][sol$solution ==1])))
}


get_opt_trade <- function(df,pointvar = "points", convar = "auctionValueAverage", team = 7, fairness = 0){
  data <- df %>%
    filter(franchise_id %in% c(1,!!team),
           !player %in% c("K", "DST"))
  data[[pointvar]][data[[pointvar]] %>% is.na()] <- 0
  data[[convar]][data[[convar]] %>% is.na()] <- 0
  pos_var <- "pos"
  name_var <- "player"
  var.types <- rep("B", length(data[[pos_var]]))
  obj <- data[[pointvar]]
  A <- rbind(
    rep(1, length(data[[pos_var]])), 
    as.numeric(data[[pos_var]] == "QB"), #QB
    as.numeric(data[[pos_var]] == "WR"), # num DST
    as.numeric(data[[pos_var]] == "RB"), # num K
    as.numeric(data[[pos_var]] == "TE"), # num TE
    data[[convar]],
    as.numeric(data[["franchise_id"]] == 1 & data$player %in% get_starters(data)) # my team
  )
  dir <- c(rep("==",2),
           rep(">=",2),
           "==",
           rep("<=",1),
           ">="
  )
  
  b <- c(7,1,2,2,1,
         sum(data[[convar]][data$player %in% get_starters(df) & !data$pos %in% c("DST", "K") &
                                             data$franchise_id == 1], na.rm = T)+fairness, #trade less than current value
         4
  )
  
  sol <- Rglpk_solve_LP(obj = obj, mat = A, dir = dir, rhs = b,types = var.types, max = TRUE)
  sol$players <- data[[name_var]][sol$solution == 1]
  recieve <- setdiff(sol$players,data$player[data$player %in% get_starters(df) & 
                                               data$franchise_id == 1])
  give <- setdiff(data$player[data$player %in% get_starters(df) & !data$pos %in% c("DST", "K") &
                                data$franchise_id == 1], data[[name_var]][sol$solution == 1])
  
  avg <- data[[convar]][data$player %in% recieve & !data$franchise_id == 1] %>% sum() - 
    data[[convar]][data$player %in% give &data$franchise_id == 1 ] %>% sum()
  
  vg <- data[[pointvar]][data$player %in% recieve] %>% sum() - 
    data[[pointvar]][data$player %in% give] %>% sum()
  
  return(list("players" = sol$players,
              "points" = sol$optimum,
              "AV1" = sum(data[[convar]][sol$solution == 1]),
              "recieve" = tibble(player = recieve,
                                 pos = data[["pos"]][data$player %in% recieve],
                                 value = data[[pointvar]][data$player %in% recieve],
                                 av = data[[convar]][data$player %in% recieve]),
              "give" = tibble(player = give,
                              pos = data[["pos"]][data$player %in% give],
                              value = data[[pointvar]][data$player %in% give],
                              av = data[[convar]][data$player %in% give]),
              "av_gain" = avg,
              "value_gain" = vg))
  
}



get_opponent <- function(conn,weekn,fid = 1){
  ff_schedule(conn) %>%
    filter(week == weekn, franchise_id == fid) %>%  
    select(opponent_id) %>% as_vector()
}

SWID <- "{1505CFD7-906A-426B-85CF-D7906AD26B75}"
espn_s2 <- "AEAL65w15x8h9k7SN8k9e9THF8kX0hQEWhL0ZAOSLkZ1h333Tb1qBrO9xdBKUg8StSlBkUtU9i8u82PHv5OrnmfH4C0V2EjAEpqAkNrcEAEypnpBlvRakQwpRVAb6VTSw0oRwBSQjF9A8jCHimN1c09nXCB9R%2FHMuwX79HKRlIQWj5%2FKbQ8a5h2tJ9d5u1Ry55Z4tNuidnowdZm6Qvud0jYm%2BvhRJIX6zcM9DnBvUOeOs%2FMEKxauoRW9vQVcz8vc8UYCQ%2BE9AgtPxX%2BrE0a9k5FV"
ffs <- ffscrapr::espn_connect(id = 762538, season = 2022, swid = SWID, espn_s2 = espn_s2)
ff_league(ffs)
glm_starters <- function(df){
  my_team <-  df[df$franchise_id == 1,]
  
  rand_team <- map_df(1:1000, ~ {
    df2 <- rbind(df[df$franchise_id %in% opp & df$player %in% get_starters(df),],
                 my_team[my_team$player %in% get_starters(my_team, random = T),])
    
    df2[["simpts"]] <-map2(df2$points, df2$hist_sd_pts,
                           ~ rgamma(1, shape = (.x / .y) ^ 2,
                                    rate = .x / (.y ^ 2)) %>% round(2)) %>% unlist()
    
    c(as.numeric(df2$simpts[df2$franchise_id == 1] %>% sum() > df2$simpts[df2$franchise_id == opp] %>% sum()),
      1:16 %in% which(my_team$player %in% df2$player[df2$franchise_id == 1]) %>%
        as.numeric()) %>%
      setNames(c("win", my_team$player))
  })
  rand_team$win %>% mean()
  glm(data = rand_team, formula = win ~ .)$coefficients
}

merge_draft <- function(year, id, proj_table, swid = NULL, s2 = NULL){
  ffs <- ffscrapr::espn_connect(year, id,swid = swid, espn_s2 = s2)
  ptc <- proj_table%>%
    mutate(
      player = paste(first_name, last_name),
      player = str_remove_all(player, "[:punct:]"),
      player = ifelse(pos == "DST", paste(last_name, "DST"), player),
      player_merge = make_clean_names(player)
    ) %>%
    filter(avg_type == "robust") %>%
    select(player, player_merge, pos, team, everything(),-avg_type) %>%
    left_join(y = injury, by = c("player", "pos")) %>%
    left_join(esr) %>%
    mutate(
      season_injury = as.numeric(season_injury),
      pgm = ifelse(
        is.na(projected_games_missed),
        mean(projected_games_missed, na.rm = T),
        projected_games_missed
      )
    )
  
  draft <- ff_draft(ffs) %>%
    mutate(player_name = str_remove_all(player_name, "[:punct:]"),
           player_name = str_remove(player_name, regex("Jr")),
           player_merge = make_clean_names(player_name),
           player_merge = case_when(player_merge == "gabe_davis" ~"gabriel_davis",
                                    player_merge == "allen_robinson_ii" ~"allen_robinson",
                                    player_merge == "joshua_palmer"~"josh_palmer",
                                    player_merge == "irv_smith" ~"irv_smith_jr",
                                    player_merge == "melvin_gordon_iii"~ "melvin_gordon",
                                    player_merge == "mark_ingram_ii"~"mark_ingram",
                                    TRUE ~ player_merge)) %>%
    select(round, pick, overall, franchise_id, 
           franchise_name,
           espn_id = player_id,
           player_merge)
  
  if(anti_join(draft, ptc, by = "player_merge") %>% nrow() > 0) print("Check Join DF")
  dfret <- left_join(ptc, draft, by = "player_merge") %>% 
    mutate(franchise_name = ifelse(is.na(franchise_name), "Free Agent", franchise_name),
           franchise_id = ifelse(is.na(franchise_id), 0, franchise_id))
  return(dfret)
}

scrape_data_seperate <- function(src = c("FantasyPros","FantasySharks","NFL","RTSports","CBS","NumberFire"),
                                 season = 2022,
                                 week = 2){
  week_scrape <- map(src,
                     ~scrape_data(season = 2022,
                                  src = .x,
                                  week = 2,
                                  pos = c("QB", "RB", "WR", "TE", "K", "DST")
                     )
  )
  
  pos <- c("QB", "RB", "WR", "TE", "K", "DST")
  sc <- list()
  for(i in seq_along(pos)){
    week_scrape[[6]][[pos[i]]]$opp <- NULL
    sc[[i]] <- map(1:6,~week_scrape[[.x]][[pos[i]]] %>% bind_rows()) %>% bind_rows()
  }
  sc <- setNames(sc,pos)
  attr(sc, "season") <- 2022 
  attr(sc, "week") <- 2
  return(sc)
}

week_projections <- function(week_scrape = week_scrape,
                              id = 762538,
                              season = 2022,
                              avg_type = "robust",
                              espn_s2 = NULL,
                              swid = NULL){
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
  
  ffs <- ffscrapr::espn_connect(season,
                                id,
                                espn_s2 = espn_s2,
                                swid = swid)
  pt <- projections_table(
    week_scrape,
    avg_type = avg_type,
    scoring_rules = ppr,
    src_weights = src_weights,
    vor_baseline = vor14
  ) %>%
    add_player_info() %>%
    filter(avg_type == avg_type)
pt <- pt[!(pt$id == 13424 & pt$pos == "QB"),]
  weekData <- pt %>%
    mutate(
      player = paste(first_name, last_name),
      player = str_remove_all(player, "[:punct:]"),
      player = ifelse(pos == "DST", paste(last_name, "DST"), player),
      player_merge = make_clean_names(player) 
    ) %>%
    filter(avg_type == avg_type) %>%
    select(player, player_merge, pos, team, everything(), -avg_type)
  
  rosters <- ffscrapr::ff_rosters(ffs) %>% 
    mutate(
      player_name = str_remove_all(player_name, "[:punct:]"),
      player_name = str_remove(player_name, regex("Jr")),
      player_merge = make_clean_names(player_name),
      player_merge = case_when(
        player_merge == "gabe_davis" ~ "gabriel_davis",
        player_merge == "allen_robinson_ii" ~ "allen_robinson",
        player_merge == "joshua_palmer" ~ "josh_palmer",
        player_merge == "irv_smith" ~ "irv_smith_jr",
        player_merge == "melvin_gordon_iii" ~ "melvin_gordon",
        player_merge == "mark_ingram_ii" ~ "mark_ingram",
        player_merge == "jeff_wilson" ~ "jeffery_wilson",
        TRUE ~ player_merge
      )
    ) %>%
    select(franchise_name,
           espn_id = player_id,
           player_merge,
           franchise_id)
  
  df <- left_join(weekData, rosters) %>%
    drop_na(sd_pts) %>% 
    mutate(franchise_name = ifelse(is.na(franchise_name), "Free Agent", franchise_name),
           franchise_id = ifelse(is.na(franchise_id), 0, franchise_id))
  return(df)
} 


win_proj_prep <- function(pt, conn = ffs, week, p_sd, gam_par){
  opp <- ff_schedule(ffs) %>% filter(week == !!week) %>% select(franchise_id, opponent_id)
  live_players <- espn_weekscores(week = week) %>% clean_espn_names(Player)
  wdf <- pt %>% 
    mutate(pbin = cut(points, breaks = c(2:20, Inf), labels = c(as.character(3:20), "20+")),
           dist_group = paste0(pos,pbin)) %>% 
    left_join(p_sd, by = "player_merge") %>%
    left_join(gam_par, by = "dist_group") %>%
    group_by(pos)%>%
    mutate(hist_sd_pts = ifelse(is.na(hist_sd_pts), hist_sd, hist_sd_pts),
           hist_sd_pts = ifelse(is.na(hist_sd_pts) & pos %in% c("DST", "K"), 6, hist_sd_pts),
           hist_sd_pts = ifelse(is.na(hist_sd_pts), mean(hist_sd_pts, na.rm = T), hist_sd_pts)
           ) %>%
    select(-c(hist_sd, hist_points, hist_point_bin, pbin)) %>% 
    left_join(live_players, by = "player_merge")
return(list(wdf = wdf, opp = opp))
}

win_sim <-function(preplist, team, iter = 100) {
  #get only starters
  oppo <- preplist$opp$opponent_id[preplist$opp$franchise_id == !!team]
  df <- preplist$wdf[preplist$wdf$franchise_id %in% c(!!team,oppo) &
              preplist$wdf$player %in% get_starters(preplist$wdf,pointvar = "points"),]
  
  win <- map_dbl(1:iter, ~{
    df[["simpts"]] <-map2(df$points, df$hist_sd_pts+.01,
                          ~ rgamma(1, shape = (.x / .y) ^ 2,
                                   rate = .x / (.y ^ 2)) %>% round(2)) %>% unlist()
    #df <- df %>% mutate(simpts = ifelse(!is.na(week_score), week_score, simpts) %>% as.numeric())
    df$simpts <- ifelse(!is.na(df$week_score), df$week_score, df$simpts)
    as.numeric(df$simpts[df$franchise_id == !!team] %>% sum() > df$simpts[df$franchise_id == oppo] %>% sum())
    
  }) %>% mean(na.rm = T)
  
  return(win)
}

win_proj_all <- function(conn, pt, week, iter = 100){
  test <- get_hist_trends()
  i_vec <- c()
  win_vec <- c()
  fid <-ff_schedule(conn) %>% distinct(franchise_id) %>% as_vector()
  preplist <- win_proj_prep(pt, conn, week, p_sd = test$p_sd, gam_par = test$gam_par)
  pb <- txtProgressBar(min = 0,max = 14, style = 2)
  for(i in seq_along(fid)){
    setTxtProgressBar(pb, i)
    win_vec[i] <- win_sim(preplist, team = fid[i],iter = iter)
  }
  sched<- ff_schedule(conn) %>% filter(week == !!week) %>% select(franchise_id, opponent_id)
  rdf <- unique(t(apply(sched, 1, sort))) %>% as_tibble() %>% rename(franchise_id = V1, opp = V2) %>%
    left_join(tibble(franchise_id = fid,win_vec))%>%
    left_join(tibble(opp = fid,opp_win = win_vec)) %>%
    mutate(win_pct = (win_vec + (1-opp_win))  / 2) %>%
    select(-c(win_vec, opp_win))
  return(rdf)
}








