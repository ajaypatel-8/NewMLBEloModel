library(baseballr)
library(tidyverse)

#dates that we need to update and add
dates <- seq(as.Date(""), as.Date(""), by = '1 day')

#read in game pks we already have
game_pks <- readRDS('all_game_pks.RDS')
#emtpy game pks vector for new ones
new_game_pks <- c()

#update game pks
#loop through all dates and append them to vector
for(date in dates) {
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
  
  new_game_pks <- append(new_game_pks, game_pk)
  all_game_pks <- append(all_game_pks, game_pk)
}

#save updated all game pks
saveRDS(all_game_pks, "all_game_pks.RDS")

#get new starter ids
#r
