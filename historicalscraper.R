library(librarian)
shelf(tidyverse,janitor, EnvStats)

year <- 2010:2021 %>% as.character()
week <- 1:17 %>% as.character()
library(fitdistrplus)
library(logspline)

# read in player weekly stats


raw <- map_df(year, ~ map(week,
  ~ paste0(
    "https://raw.githubusercontent.com/fantasydatapros/data/master/weekly/",
    .y,
    "/week",
    .x,
    ".csv") %>%
    read_csv(show_col_types = F) %>%
    as_tibble() %>%
    clean_names() %>% 
    mutate(week = .x, year = .y), .y = .x))
write.csv(raw, "hist_raw.csv")





