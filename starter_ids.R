library(baseballr)
library(tidyverse)

#getting starters for all games and storing in one large dataframe
#read in all game pks
game_pks <- readRDS("all_game_pks.RDS")

#create dummy df to bind results too
sp_probables <- baseballr::mlb_probables(446877)

#grab column names
colnames <- colnames(sp_probables)

#create empty df
sp_probables <- data.frame(matrix(ncol = length(colnames)))

colnames(sp_probables) <- colnames

#get starters for each game -> we want their ids
for(game_pk in game_pks) {
  
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

#remove row of na's
sp_probables <- sp_probables %>% filter(!is.na(game_pk)) 

#missed games -> run this after completion of initial to fill in games that got missed
missing_game_pks <- c(448105, 448250, 448407, 448899, 448925, 448948,
                      564986, 565003, 630936, 631266, 662278)

for(game_pk in missing_game_pks) {
  
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

#save csv
write_csv(sp_probables, "sp_probables.csv")
