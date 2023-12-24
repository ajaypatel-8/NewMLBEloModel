library(tidyverse)
library(elo)
library(readr)
library(ParBayesianOptimization)
library(MLmetrics)
library(scales)
library(DescTools)

#read in game scores
game_scores <- read_csv("game_scores.csv")

#read in sp_logs
sp_logs <- read_csv('sp_logs.csv')

#we want to compare each sp's weighted k-bb to the team's alternatives
#we will find team avg k-bb% for the year and adjust relative to that
team_avg_wkbb <- sp_logs %>% 
  group_by(team, year) %>% 
  summarise(team_avg_wkbb = weighted.mean(`k-bb%`, innings_pitched)) 

#join this to the sp logs and make the adjustment
sp_logs <- left_join(sp_logs, team_avg_wkbb, by = c('team', 'year'))

#gonna save the multiplier for model tuning to find the best one, just subtract
#for now
sp_logs <- sp_logs %>% 
  mutate(sp_elo_adj = weighted_roll_kbb - team_avg_wkbb)

#subset the dataframe to join to game_scores
sp_logs_small <- sp_logs %>% select(game_date, game_pk,
                                    year, team, player_id, fullName, sp_elo_adj)

#rename full name to name
sp_logs_small <- sp_logs_small %>% 
  rename(name = fullName)

#need to map team names to abbreviations
teams <- baseballr::teams_lu_table %>% filter(sport.name == 'Major League Baseball') %>% 
  select(name, id, bref_abbreviation)

#change laa abbreviation
teams <- teams %>% 
  mutate(bref_abbreviation = case_when(
    bref_abbreviation == 'ANA' ~ "LAA",
    TRUE ~ bref_abbreviation
  ))

#join to sp_logs_small
sp_logs_small <- left_join(sp_logs_small, teams, by = c('team' = 'name'))

#sample game scores to figure out the join
game_scores_test <- game_scores %>% 
  filter(home_team == "ARI", home_score == 5, away_team == 'COL',
         Year == 2016, away_score == 10)

#join to game scores
game_scores_sp <- left_join(game_scores, sp_logs_small,
                            by = c("Year" = 'year',
                                   "date" = "game_date",
                                   'Tm' = "bref_abbreviation"))

#remove duplicates //games with no adjustment
game_scores_sp <- game_scores_sp[!duplicated(game_scores_sp), ]

final_sp_game_scores <- game_scores_sp %>% 
  filter(!is.na(sp_elo_adj)) 

#clip outliers
final_sp_game_scores$sp_elo_adj <- DescTools::Winsorize(final_sp_game_scores$sp_elo_adj, probs = c(0.01, 0.99))

#in case i need it later
write_csv(final_sp_game_scores, 'final_sp_game_scores.csv')

elo_game_data <- final_sp_game_scores %>% 
  ungroup() %>% 
  group_by(H_A, home_team, away_team, home_score, away_score, Year) %>% 
  select(game_pk, H_A, home_team, away_team, home_score, away_score, Year, name, sp_elo_adj) %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = H_A, values_from = c(name, sp_elo_adj)) %>% 
  #rename columns
  rename("away_sp_name" = "name_A",
         "home_sp_name" = "name_H") 

#remove duplicates for whatever reason
elo_game_data <- elo_game_data %>% 
  group_by(game_pk) %>% 
  slice(1)

#function to improve model
elo_cv <- function(hfa,
                   regress,
                   k_factor,
                   sp_coeff) {
  data <- elo_game_data %>%
    filter(Year != 2016) # filter incomplete 2023 season
  seasons <-
    unique(data$Year) # cross validate grouping over the seasons
  
  data <- data %>% 
    mutate(sp_elo_adj_H = sp_elo_adj_H * sp_coeff,
           sp_elo_adj_A = sp_elo_adj_A * sp_coeff)
  
  formula <-
    as.formula(
      glue::glue(
        "score(home_score, away_score) ~ adjust(home_team, {hfa} + sp_elo_adj_H) + adjust(away_team, sp_elo_adj_A) + regress(Year, 1500, {regress}) + k({k_factor}*log(abs(home_score - away_score) + 1))"
      )
    ) # create the formula for {elo}
  stored_brier <- c() # track the log loss of each fold
  stored_games <- c() # track the number of games of each fold
  for (eval_season in seasons) {
    # iterate over each season
    print(eval_season)
    e <- elo.run(formula,
                 data = elo_game_data %>%
                   filter(Year < eval_season)) # grab all games prior to that season
    
    
    new_data <- elo_game_data %>% # evaluate on that season
      filter(Year == eval_season)
    predictions <- predict(e, newdata = new_data)
    scores <- score(new_data$home_score, new_data$away_score)
    brier <-
      mean((scores - predictions) ^ 2 + ((1 - scores) - (1 - predictions)) ^ 2) # caculate the brier of those games
    stored_brier <- c(stored_brier, brier) # store the log loss
    stored_games <-
      c(stored_games, nrow(new_data)) # store the number of games in each tournament
  }
  brier_score <-
    sum(stored_brier * stored_games) / sum(stored_games) # calculate the ultimate log loss
  return(list(Score = -1 * brier_score)) # return the log loss
}

#test to make sure function works
elo_cv(hfa = 23.53836,
       regress = 1,
       k_factor = 4.279556,
       sp_coeff = 10)

set.seed(123)
bounds <- list(
  hfa = c(0, 40),
  regress = c(0, 1),
  k_factor = c(0, 40),
  sp_coeff = c(0, 10)
)


optObj <- bayesOpt(
  FUN = elo_cv,
  bounds = bounds,
  initPoints = 10,
  iters.n = 25,
  iters.k = 1
)

optObj$scoreSummary %>% View()

#get best parameters
getBestPars(optObj)

elo_mod <-
  elo.run(
    score(home_score, away_score) ~  # score() is a helper function that returns the winning team when two scores from one game are passed in
      #may want to come back and adjust for 2020 season
      adjust(home_team, 22.43744 + sp_elo_adj_H) +
      adjust(away_team, sp_elo_adj_A) +
      # home_team and Opp specify the players _or_ teams involved in each game
      k(1.610293 * log(abs(
        home_score - away_score
      ) + 1)) +
      regress(Year, 1500, 0.78279491),
    # an arbitrarily selected k factor
    data = elo_game_data
  ) # our training data

results <- as.data.frame(elo_mod)

#logloss
MLmetrics::LogLoss(results$p.A, results$wins.A)

rank.teams(elo_mod)
final.elos(elo_mod)
summary(elo_mod)

#ml accuracy of model -> about 56.4%
(7734+2328) / (17848) 

#save model object
saveRDS(elo_mod, "elo_model.RDS")
