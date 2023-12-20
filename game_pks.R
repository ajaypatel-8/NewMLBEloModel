library(baseballr)
library(tidyverse)

#need to get all game pks from 2016 season on
#regular season game dates by year
dates2016 <- seq(as.Date("2016-04-03"), as.Date("2016-10-02"), by = '1 day')
dates2017 <- seq(as.Date("2017-04-02"), as.Date("2017-10-01"), by = '1 day')
dates2018 <- seq(as.Date("2018-03-29"), as.Date("2018-10-01"), by = '1 day')
dates2019 <- seq(as.Date("2019-03-20"), as.Date("2019-9-29"), by = '1 day')
dates2020 <- seq(as.Date("2020-07-23"), as.Date("2020-9-27"), by = '1 day')
dates2021 <- seq(as.Date("2021-04-01"), as.Date("2021-10-03"), by = '1 day')
dates2022 <- seq(as.Date("2022-04-07"), as.Date("2022-10-02"), by = '1 day')
dates2023 <- seq(as.Date("2023-03-30"), as.Date("2023-10-01"), by = '1 day')

#combine all vectors into one
game_dates <- c(dates2016, dates2017, dates2018, dates2019, dates2020,
                dates2021, dates2022, dates2023)

#save vector of game dates
saveRDS(game_dates, "game_dates.RDS")

#get game pks
#empty vector to add pks to
all_game_pks <- c()

#loop through all dates and append them to vector
for(date in game_dates) {
  #for checking
  print(as.Date(date))
  #need to coerce to date type
  game_pks <- baseballr::get_game_pks_mlb(as.Date(date)) 

  #make sure there were games played
  if(is.data.frame(game_pks)) {
    
    #only regular season games
    game_pks <- game_pks %>% 
      filter(seriesDescription == 'Regular Season')
    
    game_pk <- game_pks$game_pk
  }
  else{
    #move on to next date if no games
    next
  }
  
  all_game_pks <- append(all_game_pks, game_pk)
}

#save all game pks
saveRDS(all_game_pks, "all_game_pks.RDS")
