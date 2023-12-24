library(baseballr)
library(tidyverse)
library(readr)
library(mlbplotR)
library(gt)
library(gtExtras)
library(oddsapiR)

#read in elo model
elo_model <- readRDS("elo_model.RDS")

#read in game scores
game_scores <- read_csv("game_scores.csv")

#get game_pks for date
game_pks <- baseballr::get_game_pks_mlb(date = "2023-10-03", level_ids = 1)
game_pks <- game_pks$game_pk

#create empty games df to bind to
games <- data.frame(matrix(ncol = 3))
colnames(games) <- c("game_pk","home_team", "away_team")

for (game_pk in game_pks) {
  print(game_pk)
  
  game <- baseballr::mlb_game_linescore(game_pk)
  
  game <- game %>%
    select(game_pk, home_team_file_code, away_team_file_code) %>%
    rename("home_team" = "home_team_file_code",
           "away_team" = "away_team_file_code") %>%
    mutate(home_team = str_to_upper(home_team),
           away_team = str_to_upper(away_team)) %>%
    #probably need to change this to a case when to account for all abbreviations
    mutate(
      away_team = case_when(
        away_team == "ANA" ~ "LAA",
        away_team == "SF" ~ "SFG",
        away_team == "WAS" ~ "WSN",
        away_team == "KC" ~ "KCR",
        away_team == "TB" ~ "TBR",
        away_team == "SD" ~ "SDP",
        away_team == "CWS" ~ "CHW",
        away_team == "LA" ~ "LAD",
        TRUE ~ away_team
      ),
      home_team = case_when(
        home_team == "ANA" ~ "LAA",
        home_team == "SF" ~ "SFG",
        home_team == "WAS" ~ "WSN",
        home_team == "KC" ~ "KCR",
        home_team == "TB" ~ "TBR",
        home_team == "SD" ~ "SDP",
        home_team == "CWS" ~ "CHW",
        home_team == "LA" ~ "LAD",
        TRUE ~ home_team
      )
    )
  
  games <- rbind(games, game)
  
}

#remove empty row
games <- games %>%
  filter(!is.na(home_team)) 

#remove duplicated rows
games <- games[!duplicated(games), ]

#get the starting pitchers for each game
#make empty df to bind to
dummy5 <- baseballr::mlb_probables(413663)

columns5 <- colnames(dummy5)

today_sp <- data.frame(matrix(ncol = length(columns5)))

colnames(today_sp) <- columns5

#loop through game pks and get probables
for(game_pk in game_pks) {
  
  flag <- TRUE
  
  tryCatch(
    {  
      print(game_pk)
      starters <- baseballr::mlb_probables(game_pk)
      
      today_sp <- rbind(today_sp, starters)},
    
    error=function(e) {
      
      flag <- FALSE
    }
  )
  if (!flag) next
  
}

#remove empty row
today_sp <- today_sp %>% 
  filter(!is.na(game_pk)) %>% 
  select(-game_date, -home_plate_full_name, -home_plate_id)

#load teams in // need to join
mlb_teams <- mlbplotR::load_mlb_teams() %>% select(team_abbr, team_id_num)

#mutate team abbreviations
mlb_teams <- mlb_teams %>% 
  filter(!is.na(team_id_num)) %>% 
  mutate(      team_abbr = case_when(
    team_abbr == "AZ" ~ "ARI",
    team_abbr == "SF" ~ "SFG",
    team_abbr == "WSH" ~ "WSN",
    team_abbr == "KC" ~ "KCR",
    team_abbr == "TB" ~ "TBR",
    team_abbr == "SD" ~ "SDP",
    team_abbr == "CWS" ~ "CHW",
    team_abbr == "LA" ~ "LAD",
    TRUE ~ team_abbr
  )
  )

#join to today_sp
today_sp <- left_join(today_sp, mlb_teams, by = c("team_id" = "team_id_num"))

#drop team name
today_sp <- today_sp %>% 
  select(-team, -team_id)

team_abbr <- unique(game_scores$Tm)

team_abbr <- intersect(team_abbr, today_sp$team_abbr)

#pivot wider on game_pks
today_sp1 <- today_sp %>% 
  group_by(game_pk) %>% 
  pivot_wider(names_from = c(team_abbr), values_from = fullName) %>% 
  mutate(name = invoke(coalesce, across(all_of(team_abbr)))) %>% 
  select(game_pk, id, name, team_abbr)

games_today <- left_join(games, today_sp1, by = c("game_pk"))

#join to games
away_teams <- games_today %>% 
  group_by(game_pk, home_team, away_team) %>% 
  slice(1) %>% 
  rename("away_id" = "id",
         "away_sp" = "name")

home_teams <- games_today %>% 
  group_by(game_pk, home_team, away_team) %>% 
  slice(2) %>% 
  rename("home_id" = "id",
         "home_sp" = "name")

games_today_final <- left_join(home_teams, away_teams, by =c ("game_pk", "home_team", "away_team"))

#IF MISSING SP, NO PREDICTION
games_today_final <- games_today_final %>% 
  filter(!is.na(home_id), !is.na(away_id))

#get most recent w_roll_kbb values
#vector of home ids
pitchers <- c(games_today_final$home_id)

#add away ids
pitchers <- append(pitchers, games_today_final$away_id)

#read in final_game_scores which has the adjustments
final_game_scores <- read_csv("final_sp_game_scores.csv")

#make k_bb dataframe for today's games
today_adj <- data.frame(matrix(ncol = 2))

colnames(today_adj) <-c("player_id", "sp_elo_adj")

for(pitcher in pitchers) {
  
  print(pitcher)

  data <- final_sp_game_scores %>% 
    arrange(desc(date)) %>% 
    filter(player_id == pitcher, Year == 2023) %>% 
    ungroup() %>% 
    select(player_id, sp_elo_adj) %>% 
    slice(1)
  

  #if there's not adjustment, default value
  if(nrow(data) == 0) {
    player_id <- pitcher
    #25th percentile
    sp_elo_adj <- -4.88
    data1 <- data.frame(player_id, sp_elo_adj)
    
    today_adj <- rbind(today_adj, data1)
  }
  else {
    today_adj <- rbind(today_adj, data)
  }
}

#remove row of na's
today_adj <- today_adj %>% 
  filter(!is.na(player_id))


#remove duplicated rows
today_adj <- today_adj[!duplicated(today_adj), ]


#join adj to today's games // home then away
games_today_adj <- left_join(games_today_final, today_adj, by = c("home_id" = "player_id"))

#rename col
games_today_adj <- games_today_adj %>% 
  rename("sp_elo_adj_H" = "sp_elo_adj")

#join adj to today's games
games_today_adj <- left_join(games_today_adj, today_adj, by = c("away_id" = "player_id"))

#rename col
games_today_adj <- games_today_adj %>% 
  rename("sp_elo_adj_A" = "sp_elo_adj")

games_today_adj1 <- games_today_adj %>% 
  select(game_pk, home_team, away_team, home_sp, away_sp, sp_elo_adj_H, sp_elo_adj_A)

#get probabilities
today_win_prob <- games_today_adj1 %>%
  ungroup() %>% 
  mutate(home_team_win_prob = predict(elo_model, games_today_adj1))

#prettify win probs
teams <- mlbplotR::load_mlb_teams()

teams <- teams %>% 
  select(team_abbr) %>% 
  mutate(team_abbr1 = case_when(
    team_abbr == "AZ" ~ "ARI",
    team_abbr == "SF" ~ "SFG",
    team_abbr == "WSH" ~ "WSN",
    team_abbr == "KC" ~ "KCR",
    team_abbr == "TB" ~ "TBR",
    team_abbr == "SD" ~ "SDP",
    team_abbr == "CWS" ~ "CHW",
    team_abbr == "LA" ~ "LAD",
    TRUE ~ team_abbr
  ))

today_games <- left_join(today_win_prob, teams, by =c("home_team"  = "team_abbr1")) %>% 
  rename("home_team_abbr" = "team_abbr")

today_games <- left_join(today_games, teams, by = c("away_team" = "team_abbr1")) %>% 
  rename("away_team_abbr" = "team_abbr")

#get vegas odds
Sys.unsetenv("GITHUB_PAT")
Sys.setenv(ODDS_API_KEY = "ae5caea9048e8e44d68ddb61810dd9a2")

#get mlb odds (live odds, might have to pull at start of every game)
vegas_odds <- oddsapiR::toa_sports_odds(sport_key = "baseball_mlb",
                                        markets = "h2h", odds_format = "american") %>% 
  filter(bookmaker_key == "fanduel")

#join team abbr
join_name <- mlbplotR::load_mlb_teams()

join_name <- join_name %>% 
  select(team_name, team_abbr)

join_name <- left_join(join_name, teams, by = c("team_abbr"))

#join to vegas odds
vegas_odds <- vegas_odds %>% 
  select(outcomes_name, outcomes_price)

vegas_odds <- left_join(vegas_odds, join_name, by = c("outcomes_name" = "team_name"))

vegas_odds <- vegas_odds %>% select(-team_abbr, -outcomes_name)


#join
today_games_vegas <- left_join(today_games, vegas_odds, by = c("home_team" = 
                                                                 "team_abbr1"))
#convert to win prob
today_games_vegas <- today_games_vegas %>% 
  mutate(vegas_wp = if_else(outcomes_price < 0, outcomes_price 
                            / (outcomes_price - 100), 100 / (100 + outcomes_price)))

today_games_vegas <- today_games_vegas[!duplicated(today_games_vegas), ]

#remove specific rows for dh if necessary
#today_games_vegas <- today_games_vegas[-c(13, 17), ]

#make gt table
table <- today_games_vegas %>% 
  ungroup() %>% 
  select(-game_pk, -home_team, -away_team, -outcomes_price) %>% 
  arrange(desc(home_team_win_prob)) %>% 
  gt() %>% 
  mlbplotR::gt_fmt_mlb_dot_logo(columns = contains("team_abbr")) %>% 
  gt_theme_538() %>% 
  cols_label(
    home_team_abbr = "Home Team",
    away_team_abbr = "Away Team",
    home_sp = "Home SP",
    away_sp = "Away SP",
    sp_elo_adj_H = "Home SP Elo Adj",
    sp_elo_adj_A = "Away SP Elo Adj",
    home_team_win_prob = "Home Team Win Prob",
    vegas_wp = "Vegas Win Prob"
  ) %>% 
  fmt_number(columns = c(sp_elo_adj_H, sp_elo_adj_A)) %>% 
  fmt_percent(columns = c(home_team_win_prob, vegas_wp), decimals = 0) %>% 
  cols_move_to_start(columns = c("home_team_abbr", "away_team_abbr")) %>% 
  tab_header(
    title = "Win Probabilities For 9/8 MLB Games",
    subtitle = "Games With Missing Probable SP Excluded | @ajaypatel8_"
  ) %>% 
  tab_footnote(
    footnote = "SP Elo Adjustments Are Relative To Team, Not Pitcher To Pitcher",
    locations = cells_column_labels(columns = c(sp_elo_adj_H, sp_elo_adj_A))
  )

gtsave(table, "predictions.png", vwidth = 900, vheight = 900)
