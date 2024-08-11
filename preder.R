library(librarian)

install.packages("caret", version='6.0')
packageVersion("caret")
shelf(tidyverse, janitor, ffanalytics, fflr, ffscrapr, ggrepel, zoo,data.table,
      Rglpk,ggthemes,ffpros,ffsimulator, nflverse,forecast,caret, xgboost,Matrix,
      fastDummies, quiet = T)
#source(paste(getwd(),"/newFunct.R", sep=""))


in_raw <- nflreadr::load_injuries(seasons = 2009:2022) %>%
  select(season,
         week,
         gsis_id,
         last_name,
         report_primary_injury,
         report_secondary_injury, 
         report_status,
         practice_status)
raw <- nflreadr::load_player_stats(2009:2022) %>% as_tibble()
raw <- raw %>% left_join(in_raw, by = c("season", "week", "player_id" = "gsis_id")) 

#get nfl schedule data 
sched_raw <- nflfastR::fast_scraper_schedules()

sched <- sched_raw %>%
  select(season,week,
         team = home_team,
         opponent = away_team,
         score = home_score,
         opp_score = away_score,
         moneyline = home_moneyline,
         rest = home_rest,
         location,overtime,temp, wind) %>%
bind_rows(
sched_raw %>%
  select(season,week,
         team = away_team,
         opponent = home_team,
         score = away_score,
         opp_score = home_score,
         moneyline = away_moneyline,
         rest = away_rest,
         location,overtime,temp, wind)%>%
  mutate(location = ifelse(location == "Home", "Away", location))
) %>%
  mutate(wind = ifelse(is.na(wind), 0, wind),
         temp = ifelse(is.na(temp), 72, temp),
         across(c(team,opponent),~case_when(. == "STL" ~ "LA",
                                            . == "OAK" ~ "LV",
                                            . == "SD" ~ "LAC",
                                            T ~ .))) %>% 
  arrange(season, week)

raw2 <- raw %>% 
  left_join(sched, by = c("season", "week", "recent_team" = "team")) %>%
  filter(position %in% c("WR", "DST", "RB", "QB", "K", "TE")) %>%
  mutate(across(c(report_status),~ ifelse(is.na(.), "Active", .)),
         across(c(practice_status),~ ifelse(is.na(.), "Full Participation in Practice", .)),
         across(c(report_status),~ fct_relevel(.,"Active",after =  0)),
         across(c(practice_status),~ fct_relevel(.,"Full Participation in Practice",after =  0)),
         across(c(report_primary_injury,report_secondary_injury),~ ifelse(is.na(.), "None", .)),
         across(c(report_primary_injury,report_secondary_injury),~ fct_relevel(.,"None",after =  0))) 


fantprod <- raw2 %>%
  group_by(player_id, player_display_name) %>% 
  mutate(fantmean = mean(fantasy_points_ppr),
         fantprod = rollmean(fantasy_points_ppr,k = 10, align = "right",fill = NA),
         fantprod = ifelse(is.na(fantprod), cumsum(fantasy_points_ppr) / seq_along(fantasy_points_ppr), fantprod),
         fantprod = ifelse(fantprod > 1.5*mean(fantasy_points_ppr), mean(fantasy_points_ppr), fantprod),
         fantprod = ifelse(fantasy_points_ppr == 0, 0, fantprod)) %>%
  group_by(season, week, recent_team, position) %>%
  summarise(posavg = sum(fantprod)) %>%
  dplyr::group_by(season, week, recent_team) %>%
  pivot_wider(id_cols = c(season, week,recent_team),
              names_from =position,
              values_from = posavg,names_prefix = "prod"
              ) %>%
  select(-prodK)

defprod <- raw2 %>%
  group_by(season, week, opponent) %>%
  summarise(fpa = sum(fantasy_points_ppr)) %>%
  group_by(opponent) %>%
  mutate(time = 1:n(),
         prodOppDef = ifelse(time > 20,
                        rollmean(fpa,k = 20,fill = NA, align = "right"),
                        rollmean(fpa, k = 20, align = "left", fill = NA)))%>%
  select(-c(time, fpa))


rawlist <- raw2 %>% 
  left_join(fantprod) %>%
  left_join(defprod) %>% 
  split(~position)


qb <- rawlist$QB %>% 
  group_by(week, season, recent_team) %>%
  mutate(injury = ifelse(report_primary_injury == "None", 0 ,1)) %>%
  slice_max(fantasy_points_ppr) %>% 
  ungroup() %>% 
  select(week, season, fantasy_points_ppr,prodWR,prodOppDef,location,
         wind,prodTE,injury,player_display_name, player_id)

train <- qb %>% filter(season < 2022) 
test <- setdiff(qb, train)


reg <- train %>% 
  ungroup() %>% 
   select(-c(week, season, player_display_name))%>% 
  lm(data = .,fantasy_points_ppr ~ .)
ff_pred <- map_df(1:3, ~
                    ffanalytics::scrape_data(src = "FantasyPros", pos = "QB", week = .x) %>%
                    projections_table() %>%
                    add_player_info() %>%
                    mutate(player_display_name = paste(first_name, last_name)) %>% 
                    select(player_display_name, fp_points = points) %>%
                    mutate(week = .x)
)

arima_pred <- train %>% 
  filter(player_id %in% test$player_id) %>%
  split(~player_display_name) %>%
  map("fantasy_points_ppr") %>%
  map(possibly(ts,otherwise = NA_character_)) %>%
  map(~Arima(.,order = c(0,0,2))) %>%
  map(~forecast(.,h = 3)) %>%
  map(4) %>%
  map_df(as.vector) %>%
  rownames_to_column("week") %>%
  pivot_longer(-week,names_to = "player_display_name") %>%
  arrange(player_display_name) %>%
  mutate(across(week, as.numeric)) %>%
  rename(arima_pred = value)
  
playerz <- test$player_display_name %>% unique()
train_Dmatrix <- train %>%
  mutate(location = ifelse(location == "Home", 1,0)) %>% 
  dummy_cols(select_columns = "player_display_name") %>%
  drop_na() %>% 
  dplyr::select(
      prodWR,
      prodTE,
      prodOppDef, 
      location, 
      wind, injury,
      contains(playerz)
    ) %>% 
    as.matrix() %>% 
    Matrix(sparse = T)

predm <- test %>%
  mutate(location = ifelse(location == "Home", 1,0)) %>% 
  dummy_cols(select_columns = "player_display_name") %>%
  drop_na() %>% 
  dplyr::select(
    prodWR,
    prodTE,
    prodOppDef, 
    location, 
    wind, injury,
    contains(playerz)
  ) %>% 
  as.matrix() %>% 
  Matrix(sparse = T)
rownames(predm) <- test$player_display_name
targets <- train %>% drop_na() %>% .[["fantasy_points_ppr"]]


library(caret)
xgb_trcontrol <- trainControl(
  method = "cv", 
  number = 5,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)
#Building parameters set
xgb_grid <- base::expand.grid(
  list(
    nrounds = c(100, 200),
    max_depth = c(10, 15, 20), # maximum depth of a tree
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    eta = 0.1, # learning rate
    gamma = 0, # minimum loss reduction
    min_child_weight = 1,  # minimum sum of instance weight (hessian) needed ina child
    subsample = 1 # subsample ratio of the training instances
  ))


model_xgb <- caret::train(
  train_Dmatrix,
  targets,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 1
)
xgboost_pred <- predict(model_xgb,newdata = predm)
  xpred <- test %>% 
    bind_cols(xpred= xgboost_pred) %>%
    select(player_display_name,xpred,week )
  
  dfqb <- test %>%
  mutate(pred = predict(reg,newdata = test)) %>%
  left_join(ff_pred) %>% 
  left_join(arima_pred) %>%
  left_join(xpred) %>% 
  left_join(ar_pred) %>% 
  
  mutate(my_error = pred - fantasy_points_ppr,
         fp_error = fp_points - fantasy_points_ppr,
         ar_error = arima_pred - fantasy_points_ppr,
         xerror = xpred -fantasy_points_ppr,
         arcov_error = ar_pred - fantasy_points_ppr)

dfqb %>% ggplot(aes(y = pred, x = fantasy_points_ppr))+
  geom_point()


mean(dfqb$my_error^2)
mean(dfqb$fp_error^2, na.rm = T)
mean(dfqb$ar_error^2)
mean(dfqb$xerror^2)
mean(dfqb$arcov_error^2, na.rm = T)

######
df <- raw %>% group_by(player_display_name,player_id) %>% 
  filter(n() > 8,
         position %in% c("K", "DST", "QB", "WR", "RB", "TE"),
         max(season) == 2022) %>%
  ungroup() 
df %>% distinct(player_id,player_display_name) %>% arrange(player_display_name)
ardf <- df %>%
  select(name = player_display_name, fantasy_points_ppr) %>%
  split(~name) %>%
  map(~ts(.$fantasy_points_ppr)) %>%
  map(possibly(Arima, otherwise = NA_character_),order = c(1,1,2)) %>%
  map(possibly(forecast, otherwise = NA_character_), h = 1) %>%
  map(as_tibble) %>%
  bind_rows() %>%
  clean_names() %>%
  bind_cols(df %>% 
              distinct(player_id,player_display_name) %>% 
              arrange(player_display_name)
            )


qb <- rawlist$QB %>% 
  group_by(week, season, recent_team) %>%
  mutate(injury = ifelse(report_primary_injury == "None", 0 ,1)) %>%
  slice_max(fantasy_points_ppr) %>% 
  ungroup() %>% 
  select(week, season, fantasy_points_ppr,prodOppDef,location,
         wind,injury,player_display_name, player_id)


train <- qb %>% filter(season < 2022) 
test <- setdiff(qb, train)

qbxr <- train %>%
  filter(player_display_name %in% playerz) %>% 
  mutate(location = ifelse(location == "Home",1,0)) %>% 
  split(~player_display_name) %>%
  map(~select(.,prodOppDef,wind,location) %>%
        select_if(~max(.)>0) %>% 
        as.matrix())

qbxr_pred <- qb %>%
  filter(player_display_name %in% playerz) %>% 
  mutate(location = ifelse(location == "Home",1,0)) %>% 
  split(~player_display_name) %>%
  map(~select(.,season,prodOppDef,wind,location) %>%
        select_if(~max(.) > 0) %>%
        filter(season > 2021) %>%
        select(-season) %>% 
        as.matrix())

  
  qbl <- train %>%
  filter(player_display_name %in% playerz) %>% 
  split(~player_display_name) %>% 
  map(~arrange(.,season, week) %>% 
        select(fantasy_points_ppr) %>%
        ts())

arorder <- map2(qbxr,qbl,
              possibly(~auto.arima(y = .y, xreg = .x,
                                   stepwise = T,
                                   allowdrift = T),
                                 otherwise = NA_character_ )) %>%
  map(possibly(~arimaorder(.), otherwise = NA_character_))

armod <- pmap(list(qbxr,qbl,arorder,qbxr_pred),
     possibly(~Arima(y = ..2, xreg = ..1, order =c(0,0,0)) %>% 
                forecast(xreg = ..4),
              otherwise = NA_character_ )) %>% 
  map("mean") %>% 
  map(as.vector)

ar_pred <- qb %>%
  filter(player_display_name %in% playerz,
         season >2021) %>% 
  mutate(location = ifelse(location == "Home",1,0)) %>% 
  split(~player_display_name) %>%
  map2_df(armod,~bind_cols(.x,"pred" = .y)) %>% 
  select(week,season,player_id, ar_pred = pred)


shelf(tsDyn)

VECM(train %>% select(fantasy_points_ppr,prodOppDef, wind, injury), lag = 1)

      