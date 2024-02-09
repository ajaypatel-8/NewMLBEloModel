# MLBEloModel

This repository consists of various scripts and files that I've used to create an ELO model for Major League Baseball. The model consists of data going back to the 2016 season, and adds an adjustment for home field advantage along for the starting pitcher each team is starting. My process is pretty simple, yet effective. We train the model on past games, where each game is given the aforementioned adjustments. A tuning function is used to improve the model, where tune the home field adjustment, the k factor (how much one game influences our priors), the regression factor (how do we change team ELO's at the start of a year), and a starting pitcher coefficient (multiply the adjustment by). The starting pitcher coefficient was not used in the final model based on tuning results. After tuning, I put together a combination of sensible parameters but also well-performing in my final ELO model. The specifics of the model can be found in elo_model.R. And this is more for myself to remember, but the update process goes update_game_scores -> update_game_pks -> update_starter_ids -> update_pitcher_logs -> re-run the model in elo_model -> make predictions in model_predictions.

# Files
all_game_pks.RDS -> R object that holds game_pks for each MLB game since 2016.

elo_model.R -> R script that makes the starting pitcher ELO adjustment, creates the ELO model + tuning, and saves the model object.

elo_model.RDS -> R object that holds the ELO model.

final_sp_game_scores.csv -> CSV that has game scores going back to 2016 + the starting pitcher and their respective adjustment.

game_dates.RDS -> R object that has game dates going back to 2016 (used this for initial development / not needed now).

game_pks.R -> R script that was used to get all game_pks going back to 2016.

game_scores.R -> R script to compile all game scores back to 2016.

game_scores.csv -> CSV that holds all game scores going back to 2016.

mlb_teams.RDS -> R object that has MLB teams

model_predictions.R -> R script used to make predictions for a day of games.

pitcher_logs.R -> R script used to compile pitcher statlines going back to 2016.

predictions.png -> GT output of model's predictions for a day (Note the example should say 10/3 not 9/8 as it currently does).

sp_ids.RDS -> R object of starting pitcher ids.

sp_logs.csv -> CSV file of pitcher statlines with weighted_roll_kbb (used to make the adjustment in elo_model.R).

sp_probables.csv -> CSV file of probable starters for each game back to 2016.

starter_ids.R -> R file used to get and create sp_probables.

update_game_pks.R -> R file that will be used to update game_pks once games start again.

update_game_scores.R -> R file that will be used to update and append to game_scores.csv.

update_pitcher_logs.R -> R file that will be used to update pitcher logs.

update_starter_ids.R -> R file that will be used to update sp_probables csv.

# General Notes
Model had about a 56.5% moneyline accuracy back to 2016, performed quite well by log loss, especially compared to vegas odds. I'd want to spend more time defining what would be a "betting play" from the model, but a general guideline of 0.03-0.05 points of deviation from vegas win probability has worked well in the past. Some games won't have predictions due to missing starters or missing data in MLB's API, as well as old games that were missing listed starters or data for whatever reason. Another thing someone (myself?) could improve on is how the starting pitcher adjustment is being made. Right now, I'm just using k-bb% weighted by innings, where we take the starter's weighted k-bb% and subtract their team's average weighted k-bb% to make the ELO adjustment. I did clip outliers down to the 1st and 99th percentile to prevent starters from having large effects on a team's win probability. For example, one test case I ran into this with was Jacob DeGrom. Yes, he was insane during his prime but I don't think he should single-handedly be upping his team's win probability by 20+ percentage points. By clipping, he still has a very large effect, but nothing out of this world. I feel a lot better about this workflow compared back to the old ELO model I had, and hope it was of use if you're reading this.
