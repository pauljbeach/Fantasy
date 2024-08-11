library(rvest)
#ros projections
url <- session("https://www.numberfire.com/nfl/fantasy/remaining-projections")

#numberfire trade value
trade <- session("https://www.numberfire.com/nfl/news/45237/fantasy-football-trade-value-chart-week-2")
tv_raw <- html_table(trade, header = T)[[1]]
tv <- tv_raw %>% clean_espn_names(Player) %>% select(player_merge, PPR) %>%
  mutate(player_merge = case_when(player_merge == "a_j_green" ~ "aj_green",
                                  player_merge == "k_j_osborn"~ "kj_osborn",
                                  player_merge == "o_j_howard"~ "oj_howard",
                                  player_merge == "c_j_uzomah"~ "cj_uzomah",
                                  player_merge == "jeff_wilson" ~ "jeffery_wilson",
                                  player_merge == "a_j_brown" ~ "aj_brown",
                                  player_merge == "a_j_dillon" ~ "aj_dillon",
                                  TRUE ~ player_merge))

#get names
names <- html_table(url,header =T)[[1]] %>%
  row_to_names(1) %>%
  as_vector() %>%
  str_split(pattern = "\n") %>%
  map(~.[[1]]) %>%
  as_vector()
#get projections and paste in names 
 ros <- html_table(url)[[2]] %>%
  row_to_names(1) %>%
  clean_names() %>%
  mutate(range = str_split(ci, "-") %>% map_chr(.,2) %>% as.numeric() -
           str_split(ci, "-") %>% map_chr(.,1) %>% as.numeric(),
         player = names,
         fp = as.numeric(fp) %>% round(2),
         across(c(yds,t_ds, ints, yds_2, t_ds_2, rec,yds_3, t_ds_3), as.numeric)
         ) %>%
   mutate(ppr_ros = yds/25 + t_ds*4 - ints*2 + yds_2/10 + t_ds_2*6 + rec + yds_3/10 + t_ds_3*6) %>%
   select(player,ppr_ros, range) %>%
  clean_espn_names()

#franchise ids
wp2 <- week_projections(week_scrape,avg_type = "robust")
ids <- wp2 %>% distinct(franchise_id) %>% as_vector()

map(ids, ~ wp2 %>% 
      left_join(ros) %>%
      left_join(tv) %>%
      get_opt_trade(pointvar = "ppr_ros",convar = "PPR", team = .x)
)

wp2 %>% filter(franchise_id == 1)
wp2 %>% left_join(ros) %>% left_join(tv) %>%
  get_opt_trade(pointvar = "ppr_ros",convar = "PPR", team = ids[6])
tv %>% view()
wp2 %>% left_join(tv) %>% filter(franchise_id ==15) %>%
  select(player, PPR, player_merge)
data <- wp2 %>% left_join(ros)
