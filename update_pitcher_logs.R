library(baseballr)
library(tidyverse)
library(readr)

#read in sp_probables csv
sp_probables <- read_csv("sp_probables.csv")

#read in existing sp_logs
sp_logs <- read_csv("sp_logs.csv")

#existing game_pks 
done_game_pks <- unique(sp_logs$game_pk)

#get new rows in sp_probables we have to update logs for
new_game_pks <- setdiff(unique(sp_probables$game_pk), done_game_pks)

#subset sp_probables to games we need to get stats for
sp_probables_new <- sp_probables %>% 
  filter(game_pk %in% new_game_pks)

#create empty statlines df to bind results to
statlines1 <- mlb_player_game_stats(game_pk = 446896, person_id  = 592789)

statlines1 <- statlines1 %>% filter(group == 'pitching') %>% select(player_id,
                                                                    game_pk,
                                                                    innings_pitched,
                                                                    batters_faced, 
                                                                    strike_outs, 
                                                                    base_on_balls)

#grab column names
colnames <- colnames(statlines1)

#create empty df
statlines <- data.frame(matrix(ncol = length(colnames)))

colnames(statlines) <- colnames

for (i in 1:nrow(sp_probables_new)) {
  
  print(i)
  
  #extract id and game_pk for the current row
  id <- sp_probables_new$id[i]
  game_pk <- sp_probables_new$game_pk[i]
  
  tryCatch({
    #call the mlb_player_game_stats function
    stats <- mlb_player_game_stats(game_pk = game_pk, person_id = id)
    
    #only want pitching stats
    stats <- stats %>% filter(group == 'pitching') %>% select(player_id,
                                                              game_pk,
                                                              innings_pitched,
                                                              batters_faced, 
                                                              strike_outs, 
                                                              base_on_balls)
    
    
    #append the results to the statlines data frame
    statlines <- rbind(statlines, stats)
    
    #sleep timer to prevent api limits
    Sys.sleep(c(0.002, 0.3, 0.09, 0.1))
    
  }, error = function(e) {
    
    #handle the error -> most are either due to na's in sp_id column (no probable listed)
    #or pitcher was switched after the probable
    cat("Error in sp_id: ", id, " game_pk: ", game_pk, "iteration", i, ":", conditionMessage(e), "\n")
  })
}


#remove row of na's
statlines <- statlines %>% filter(!is.na(player_id), !is.na(innings_pitched)) %>% 
  #convert innings pitched to number
  mutate(innings_pitched = as.numeric(innings_pitched))

#join game dates from sp_probables to the statlines
game_dates <- sp_probables_new %>% select(game_pk, game_date)

#make game_dates unique so we don't join twice
game_dates <- game_dates[!duplicated(game_dates),]

statlines <- left_join(statlines, game_dates, by = c("game_pk"))

#create k%, bb%, k-bb% columns
statlines <- statlines %>% 
  mutate(`k%` = strike_outs / batters_faced * 100,
         `bb%` = base_on_balls / batters_faced * 100,
         `k-bb%` = `k%` - `bb%`)

#have to recreate sp_logs // select columns we want to recreate
statlines_exist <- sp_logs %>% 
  select(player_id, game_pk, innings_pitched, batters_faced, 
         strike_outs, base_on_balls, game_date, `k%`, `bb%`, `k-bb%`)

#bind everything together
statlines <- rbind(statlines_exist, statlines)

#create rolling average
roll_statlines <- statlines %>% 
  group_by(player_id) %>% 
  arrange(game_date) %>% 
  mutate(roll_kbb = roll::roll_mean(`k-bb%`, width = 7))

#for games with missing k-bb% (first 7 for each pitcher) -> impute career average weighted by innings
avg_kbb <-  statlines %>%
  filter(!is.na(`k-bb%`), !is.na(innings_pitched)) %>% 
  group_by(player_id) %>%
  summarise(avg_kbb = weighted.mean(`k-bb%`, innings_pitched))

#join avg kbb back
roll_statlines <- left_join(roll_statlines, avg_kbb, by = c("player_id"))

#if missing roll_kbb, impute with avg_kbb and then drop
roll_statlines <- roll_statlines %>% 
  mutate(roll_kbb = if_else(is.na(roll_kbb), avg_kbb, roll_kbb)) %>%
  #remove any starts with missing innings pitched
  filter(!is.na(innings_pitched)) %>% 
  #don't need avg anymore
  select(-avg_kbb) %>%
  mutate(weighted_roll_kbb = (roll_kbb  * mean(innings_pitched)) / 4.8)

#join name, team, team_id to roll_statlines
team_games <- sp_probables %>% select(game_pk, fullName,id, team, team_id)

#remove any duplicate rows
team_games <- team_games[!duplicated(team_games),]


roll_statlines1 <- left_join(roll_statlines, team_games, by = c('game_pk',
                                                                "player_id" = "id"))

#rearrange order of columns
final_roll_statlines <- roll_statlines1 %>% 
  #add in year variables
  mutate(year = str_sub(game_date, 1, str_locate(game_date, "-")[, 1])) %>%
  mutate(year = as.numeric(str_remove(year, "-"))) %>%
  select(game_pk, game_date, year, fullName, player_id, team, team_id, innings_pitched, batters_faced,
         `k-bb%`, roll_kbb, weighted_roll_kbb, `k%`, `bb%`, strike_outs, base_on_balls)

#save rolling logs // these have almost every start from 2016 - 2023
write_csv(final_roll_statlines, "sp_logs.csv")
