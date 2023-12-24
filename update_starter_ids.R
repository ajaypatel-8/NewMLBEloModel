library(baseballr)
library(tidyverse)

#read in game_pks
all_game_pks <- readRDS("all_game_pks.rds")

#read in sp_probables
sp_probables <- read_csv("sp_probables.csv")

#get the new game pks
existing_game_pks <- unique(sp_probables$game_pk)

#new games
new_game_pks <- setdiff(all_game_pks, existing_game_pks)

#get new games and bind to sp_probables
for(game_pk in new_game_pks) {
  
  tryCatch(
    {  
      starters <- baseballr::mlb_probables(game_pk) 
      
      #bind results
      sp_probables <- rbind(sp_probables, starters)
      
      Sys.sleep(c(0.002, 0.3, 0.09, 0.1))
    },
    
    error=function(e) {
      
      cat("Error occurred for game_pk: ", game_pk, "\n")
    }
  )
}

#save updated csv of probables
write_csv(sp_probables, "sp_probables.csv")

