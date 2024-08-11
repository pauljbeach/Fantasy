library(librarian)
shelf(tidyverse)
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
#currently returns projected points. Likely to change

espn_weekscores <- function(season = 2022, id = 762538){
pls <- espn_proj(season,id)
test<- matrix(nrow = 1, ncol = 2)
test2 <- matrix(nrow = 1000, ncol = 2)
for(j in 1:1000){
loc <- pls$content$players[[j]]$player

locp <- pls$content$players[[1]]$player
for(i in 1:length(loc$stats)){
  if(loc$stats[[i]]$seasonId == 2022 & 
    loc$stats[[i]]$scoringPeriodId == 1 &
    loc$stats[[i]]$statSourceId == 0){
    test[i,2] <- loc$stats[[1]]$appliedTotal
    test[i,1] <- loc$fullName
  }
}
test2[j,] <- test

}
colnames(test2) <- c("Player", "week_score")
test2 %>% as_tibble() %>% distinct() %>% pluck()
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
    select(-c(!!p_name, p_name))
}

espn_weekscores() %>% clean_espn_names(Player)




map(1:1000, ~{
c(pls$content$players[[.x]]$player$fullName,
  pls$content$players[[.x]]$player$stats[[length(pls$content$players[[.x]]$player$stats)]]$appliedTotal %>% round(2))
}) %>% unlist() %>%
  matrix(nrow = 2) %>%
  t() %>% 
  as_tibble() %>%
  mutate()
