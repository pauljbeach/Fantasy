###########################
# File: Simulation.R
# Description: Determines Optimum Roster to Maximize Projected Points and Minimize Risk Based on Simulation
# Date: 5/15/2013
# Author: Isaac Petersen (isaac@fantasyfootballanalytics.net)
# Notes:
# To do:
###########################

#Specify Maximum Risk
#maxRisk <- 3.8

#Library
library("Rglpk")

#Functions
source(paste(getwd(),"/Functions.R", sep=""))
source(paste(getwd(),"/League Settings.R", sep=""))

#createdataset
#clean data ---- 

# optimal risk-----
risk <- c()
# find optimal risk values # 1.5 and 9ish
for(i in 1:200){
  
  risk[i] <- optimizeTeam(optimizeData,
                          forecast = T,
                          maxRisk = 1,
                          draftpos = 2,
                          nteams = 14,
                          maxInjury = i / 10)$optimum
}

plot(risk[risk> 1400])

# simulate-----
give_picks <- function(data = optimizeData,
                       draftpos = 2,
                       forecast = F,
                       rank = "espn_rank",
                       nteams = 14,
                       maxRisk = 3.5,
                       maxInjury = 9,
                       current_pick = 1,
                       playerlist = NULL,
                       point_var = "points",
                       sd_var = "sd_pts",
                       iterations = 100
                       ){
solutionList <- matrix(nrow=dim(data)[1], ncol=iterations*14)
pb <- txtProgressBar(min = 0, max = iterations, style = 3)

for(i in 0:iterations - 1){
  setTxtProgressBar(pb, i)
  data[["simPts"]] <- map2(data[[point_var]], 
                              data[[sd_var]],
                              ~rnorm(n = 1, mean = .x, sd = .y))
  for(j in 0:13){
  
  add_uncertainty
  solutionList[,14*i+j+1]<-  optimizeTeam(data = data,
                                          point_var = "simPts",
                                          rank = rank,
                                          maxRisk = maxRisk,
                                          playerlist = playerlist,
                                          current_pick = current_pick,
                                          draftpos = draftpos,
                                          nteams = nteams,
                                          maxInjury = 9)$solution
  }
}
data[["simresults"]] <- rowSums(solutionList, na.rm=TRUE)
return(data %>% select(player,
                       pos,rank,espn_rank,simresults,points,
                       sd_pts,points_vor,overall_ecr,sd_ecr,
                       pgm, uncertainty) %>% arrange(rank) %>%
         arrange(rank) %>%
         filter(uncertainty < .5, simresults > 0) %>% view())
}

#toploc <- draft_order(draftpos,nteams)[max(which(draft_order(draftpos,nteams) == draft_order(draftpos,nteams)[draft_order(draftpos,nteams) < current_pick])+2)]



sim2 <- function(data = optimizeData,
                 draftpos = 2,
                 forecast = T,
                 rank = "espn_rank",
                 nteams = 14,
                 maxRisk = 3.5,
                 maxInjury = 9,
                 currentpick = 1,
                 playerlist = NULL,
                 point_var = "points",
                 sd_var = "sd_pts",
                 iterations = 100
){
  pickList <- matrix(ncol=7, nrow=iterations)
  pb <- txtProgressBar(min = 0, max = iterations, style = 3)
  current_pick <- draft_order(draftpos, nteams)[currentpick]
  
  1 %% 10 -5
  for(i in 1:iterations){
    setTxtProgressBar(pb, i)
    data[["simPts"]] <- map2(data[[point_var]], 
                             data[[sd_var]],
                             ~rnorm(n = 1, mean = .x, sd = .y))
      pickList[i,] <-  optimizeTeam(data = data,
                            point_var = "simPts",
                            forecast = forecast,
                            rank = rank,
                            maxRisk = maxRisk,
                            current_pick = current_pick + i%%10-10,
                            draftpos = draftpos,
                            playerlist = playerlist,
                            nteams = nteams,
                            maxInjury = maxInjury)$playerInfo$id
  }
  map2(1:7,paste0("pick",1:7), ~pickList[,.x] %>% 
        as_tibble() %>%
        group_by(value) %>% 
        summarise(n = n()) %>%
        rename(!!.y := n)) %>% reduce(full_join) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
    left_join(optimizeData, by = c("value" = "id")) %>%
    select(player, points_vor,uncertainty, pgm, rank,espn_rank,adp,contains("pick"))
}
sim2(rank = "espn_rank",
     maxRisk = 6,
     maxInjury = 20,
     nteams = 14,
     draftpos = 2,
     currentpick = 6,
     forecast = F,
     playerlist = c("Cooper Kupp", "Josh Allen", "James Conner", "Darren Waller", "Michael Thomas"
                    )) %>% 
  arrange(desc(pick6))

