library(librarian)
shelf(tidyverse, janitor, ffanalytics, fflr, ffscrapr, ggrepel, ggbeeswarm, zoo,
      Rglpk,ggthemes)

source(paste(getwd(),"/newFunct.R", sep=""))
week_scrape <-scrape_data(season = 2022,
                          src = c("FantasyPros","FantasySharks", 
                                  "NFL", "RTsports"),
                          week = 2,
                          pos = c("QB", "RB", "WR", "TE", "K", "DST"))

weekdf <- week_projections(week_scrape = week_scrape,)