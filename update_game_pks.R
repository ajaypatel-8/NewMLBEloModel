library(baseballr)
library(tidyverse)

#read in previous game_pks
all_game_pks <- readRDS("all_game_pks.RDS")

#new dates we are adding
new_dates <- seq(as.Date("2024-03-28"), as.Date("2024-03-28"), by = '1 day')

#add game_pks from new dates to existing game_pks
for(date in new_dates) {
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

#make game_pks unique so no duplicates
all_game_pks <- unique(all_game_pks)

#save all game pks
saveRDS(all_game_pks, "all_game_pks.RDS")
