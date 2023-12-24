library(baseballr)
library(tidyverse)

#read in vector of mlb team ids
mlb_teams <- readRDS("mlb_teams.RDS")

#read in past game scores
game_scores <- read_csv('game_scores.csv')

#year we're getting new games for
years <- c(2024)

#get game scores back to 2016
for (team in mlb_teams$team_abbr) {
  for(year in years) {
    
    print(team)
    
    #get scores of games using bref function
    data <- baseballr::team_results_bref(team, year) %>% 
      mutate(
        #add in home/away teams
        home_team = if_else(H_A == "H", Tm, Opp),
        away_team = if_else(H_A == "H", Opp, Tm),
        #add in home/away scores
        home_score = if_else(home_team == Tm, R, RA),
        away_score = if_else(home_team == Tm, RA, R)
      ) %>% 
      #create date variable from the string
      mutate(month = sub('.*,\\s*', '', Date),
             month = str_replace(month, " \\s*\\([^\\)]+\\)", ""),
             day = as.numeric(gsub(".*?([0-9]+).*", "\\1", month)),
             month = gsub('[0-9]+', '', month),
             month= gsub(" ", "", month),
             month = match(month, month.abb),
             date = as.Date(paste(Year,month,day,sep="-"),"%Y-%m-%d")) %>% 
      #drop unnecessary columns
      select(-Attendance, -Streak, -cLI, -Record, -Rank, -Win, -Loss, -Date, -Save, -Time)
    
    #bind data
    game_scores <- rbind(game_scores, data)
    
  }
}

#convert dates to date // not sure why i had to do it again but we ball
game_scores$date <- as.Date(game_scores$date)

#remove any duplicate games
game_scores <- game_scores[!duplicated(game_scores), ]

#save csv
write_csv(game_scores, "game_scores.csv")