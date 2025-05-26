### In this script one can find all the code written for importing the dataset and creating
### smaller datasets 


## LIBRARIES ##

library(tidyverse)
library(tidymodels)


# link for the data ---> https://www.kaggle.com/datasets/hubertsidorowicz/football-players-stats-2024-2025?resource=download


##  IMPORTING, CLEANING AND MANIPULATING DATA ##

data_complete <- read.csv('players_data_light-2024_2025.csv', header = T)



# let's select the variables we are going to use and create a dataset for each category of statistics

player_info <- data_complete %>%
  select(Player:Min)


attacking_stats <- data_complete %>%
  select(Player:Min, Gls:PK, xG:xAG)

defending_stats <- data_complete %>%
  select(Player:Min, Tkl, TklW, Blocks_stats_defense, Int, Tkl.Int, Clr, Err)

passing_stats <- data_complete %>%
  select(Player:Min, PrgP, PrgC, KP, Ast_stats_passing, xA, PPA)

goalkeeping_stats <- data_complete %>%
  select(Player:Min, GA, Saves, Save., CS, CS., PKA, PKsv)

possession_stats <- data_complete %>%
  select(Player:Min, Touches, Carries, PrgR, Mis, Dis)

miscellaneus_stats <- data_complete %>%
  select(Player:Min, CrdY, CrdR, PKwon, PKcon, Recov)



# Now let's create the final dataset with all the variables we are going to use

df_list <- list(attacking_stats, defending_stats, passing_stats, 
                goalkeeping_stats, possession_stats, miscellaneus_stats)

data <- reduce(df_list, left_join, 
                  by = c('Player', 'Nation', 'Pos', 'Squad', 'Comp', 'Age', 'Born', 'MP', 'Starts', 'Min'))


# Let's separate the players by position... 

forwards <- data %>%
  filter(Pos == 'FW' | Pos == 'MF,FW')

midfielders <- data %>%
  filter(Pos == 'MF')

defenders <- data %>%
  filter(Pos == 'DF' | Pos == 'DF,MF')

goalkeepers <- data %>%
  filter(Pos == 'GK')

# ...and select the specific stats of every position

forwards_attacking_stats <- attacking_stats %>%
  filter(Pos == 'FW' | Pos == 'MF,FW')

defenders_defending_stats <- defending_stats %>%
  filter(Pos == 'DF' | Pos == 'DF,MF')

goalkeepers_goalkeeping_stats <- goalkeeping_stats %>%
  filter(Pos == 'GK')

midfielders_passing_possession_stats <- data %>%
  filter(Pos == 'MF') %>%
  select(Player:Born, PrgP, PrgC, KP, Ast_stats_passing, xA, PPA, Touches, Carries, PrgR, Mis, Dis)


