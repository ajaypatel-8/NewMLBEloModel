library(baseballr)
library(tidyverse)

#read in all game pks
game_pks <- readRDS("all_game_pks.RDS")

#empty vector for pitcher ids
sp_ids <- c()

#get starters for each game -> we want their ids
for(game_pk in game_pks) {
  

  tryCatch(
    {  
      starters <- baseballr::mlb_probables(game_pk) 
      
      new_pitchers_ids <- starters$id
      
      sp_ids <- c(sp_ids, new_pitchers_ids[!new_pitchers_ids %in% sp_ids])
      
      Sys.sleep(c(0.002, 0.3, 0.09, 0.1))
      },
    
    error=function(e) {
      
      cat("Error occurred for game_pk: ", game_pk, "\n")
    }
  )
}

#save sp_ids rds object
saveRDS(sp_ids, "sp_ids.RDS")
