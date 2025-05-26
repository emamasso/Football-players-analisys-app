
library(tidyverse)
library(reticulate)
library(shiny)
library(DT)
library(patchwork)
library(ggrepel)
library(tidymodels)
library(yardstick)
library(vip)
library(themis)
library(rsconnect)
library(cluster)









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


###


filter_data_no_gk <- data %>%
  select(-c(GA, Saves, Save., CS, CS., PKA, PKsv)) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  filter(Pos != 'GK')

scale_data <- scale(filter_data_no_gk[,11:41])

pc <- prcomp(scale_data, scale = F)

summary(pc)

plot(pc, type = 'l', main = 'Screeplot') ## looking at the summary and the screeplot I would keep the first 3 PC

sort(pc$rotation[,1], decreasing = T)    ## It' intresting that the most important stats in PC1 are passing and defending stats
sort(pc$rotation[,2], decreasing = T)    ## In the PC2 we see that offensive stats are leading
sort(pc$rotation[,3], decreasing = T)    ## in the PC3 we still have many offensive stats resulting the most important ones

# GK

filter_data_gk <- goalkeepers_goalkeeping_stats %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

pc_gk <- prcomp(filter_data_gk[,11:17], scale = T)

summary(pc_gk)

plot(pc_gk, type = 'l', main = 'Screeplot')

sort(pc_gk$rotation[,1], decreasing = T)
sort(pc_gk$rotation[,2], decreasing = T)    
sort(pc_gk$rotation[,3], decreasing = T)






## It could be usefull to do the same also on the more specific dataframes so that we can find 
## more information (e.g. the defenders who play in an offensive way)




# Let's start with specific categories of stats of all the moving players

filter_attacking_data <- attacking_stats %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  filter(Pos != 'GK')

pc_off <- prcomp(filter_attacking_data[,11:18], scale = T)

summary(pc_gk)

plot(pc_off, type = 'l', main = 'Screeplot')

sort(pc_off$rotation[,1], decreasing = T)
sort(pc_off$rotation[,2], decreasing = T)    
sort(pc_off$rotation[,3], decreasing = T)


# # # # #


filter_defending_data <- defending_stats %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  filter(Pos != 'GK')

pc_def <- prcomp(filter_defending_data[,11:17], scale = T)

summary(pc_def)

plot(pc_def, type = 'l', main = 'Screeplot')

sort(pc_def$rotation[,1], decreasing = T)
sort(pc_def$rotation[,2], decreasing = T)    


# # # # #


filter_passing_data <- passing_stats %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  filter(Pos != 'GK')

pc_pas <- prcomp(filter_passing_data[,11:16], scale = T)

summary(pc_pas)

plot(pc_pas, type = 'l', main = 'Screeplot')

sort(pc_pas$rotation[,1], decreasing = T)
sort(pc_pas$rotation[,2], decreasing = T)    


# # # # #


filter_possession_data <- possession_stats %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  filter(Pos != 'GK')

pc_pos <- prcomp(filter_possession_data[,11:15], scale = T)

summary(pc_pos)

plot(pc_pos, type = 'l', main = 'Screeplot')

sort(pc_pos$rotation[,1], decreasing = T)
sort(pc_pos$rotation[,2], decreasing = T)    



# Now let's conduct the PCA on two more specific data frames:


# Offensive stats for attacking players

filter_attacking_offensive_data <- forwards_attacking_stats %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  filter(Pos != 'GK')

pc_att_off <- prcomp(filter_attacking_offensive_data[,11:18], scale = T)

summary(pc_att_off)

plot(pc_att_off, type = 'l', main = 'Screeplot')

sort(pc_att_off$rotation[,1], decreasing = T)
sort(pc_att_off$rotation[,2], decreasing = T)    


# Defensive stats for defenders

filter_defending_defensive_data <- defenders_defending_stats %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  filter(Pos != 'GK')

pc_def_def <- prcomp(filter_defending_defensive_data[,11:17], scale = T)

summary(pc_def_def)

plot(pc_def_def, type = 'l', main = 'Screeplot')

sort(pc_def_def$rotation[,1], decreasing = T)
sort(pc_def_def$rotation[,2], decreasing = T)    




## Now we have ALL the loadings that could interest someone when analyzing players performances that will help 
## building the rankings.




## CLUSTERING ##

# In this section I want to cluster my data utilizing the information obtained in the previou one, so that 
# it is possible to find groups of player with similar features for understandig when a comparison between players has
# a meaning

km <- kmeans(pc$x[,1:3], centers = 3) 

pc$Cluster <- as.factor(km$cluster)

filter_data_no_gk$Cluster <- as.factor(km$cluster)

info <- filter_data_no_gk[, 11:42] %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = T))
# seems like the group number 1 is the forward group, number 3 is the midfielder one and 2 the difender one


## Now let's do the same on the more specific clusters

### offensive stats ###

set.seed(62)

km_off <- kmeans(pc_off$x[,1:3], centers = 2)

filter_attacking_data$Cluster <- as.factor(km_off$cluster)

info_off <- filter_attacking_data[, 11:19] %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = T))


## We can't get many information here, just that we have two categories of players: the offensive players and the 
## non offensive ones


### defensive stats ###

set.seed(62)

km_def <- kmeans(pc_def$x[, 1:2], centers = 2)

filter_defending_data$Cluster <- as.factor(km_def$cluster)

info_deff <- filter_defending_data[, 11:18] %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = T))

## We can't get many information here, just that we have two categories of players: the defensive players and the 
## non defensive ones

# Probably is more accurate to analyze this stats category on the forwards and the defenders, even if thanks to that we can find
# defenders who have an offensive style of play and forwards who have a defensive style of play



### passing stats ###

set.seed(62)

km_pas <- kmeans(pc_pas$x[,1:2], centers = 2)

filter_passing_data$Cluster <- km_pas$cluster

info_pas <- filter_passing_data[,11:17] %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = T))


### possession stats ###

set.seed(62)

km_pos <- kmeans(pc_pos$x[,1:2], centers = 3)

filter_possession_data$Cluster <- km_pos$cluster

info_pos <- filter_possession_data[,11:16] %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = T))


### goalkeepers ###

set.seed(62)

km_gk <- kmeans(pc_gk$x[, 1:3], centers = 2)

filter_data_gk$Cluster <- as.factor(km_gk$cluster)

info_gk <- filter_data_gk[, 11:18] %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = T))


### offensive for forwards ###

set.seed(62)

km_att_off <- kmeans(pc_att_off$x[, 1:2], centers = 2)

filter_attacking_offensive_data$Cluster <- as.factor(km_att_off$cluster)

info_off_off <- filter_attacking_offensive_data[, 11:19] %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = T))



### defensive for defenders ###

set.seed(62)

km_def_def <- kmeans(pc_def_def$x[, 1:2], centers = 2)

filter_defending_defensive_data$Cluster <- as.factor(km_def_def$cluster)

info_def_def <- filter_defending_defensive_data[, 11:18] %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = T))


filter_attacking_data %>%
  filter(Pos == 'DF' | Pos == 'MF,DF' | Pos == 'DF,MF', Cluster == 2)

filter_attacking_data %>%
  filter(Pos == 'FW' | Pos == 'MF,FW' | Pos == 'FW,MF', Cluster == 1)





# Now i'm going to put all of this information in the dataset without the gk

filter_data_no_gk$off_cluster <- as.factor(filter_attacking_data$Cluster)
filter_data_no_gk$def_cluster <- as.factor(filter_defending_data$Cluster)
filter_data_no_gk$pos_cluster <- as.factor(filter_possession_data$Cluster)
filter_data_no_gk$pas_cluster <- as.factor(filter_passing_data$Cluster)


clustered_data <- filter_data_no_gk %>%
  mutate(off_cluster = case_when(
    off_cluster == 1 ~ 'Offensivly involved',
    off_cluster == 2 ~ 'Offensivly passive'), 
    def_cluster = case_when(
      def_cluster == 2 ~ 'Passive defender',
      def_cluster == 1 ~ 'Aggressive defender'),
    pos_cluster = case_when(
      pos_cluster == 3 ~ 'Main ball keeper',
      pos_cluster == 1 ~ 'Involved in possession but not crucial',
      pos_cluster == 2 ~ 'Secondary receiver'
    ),
    pas_cluster = case_when(
      pas_cluster == 1 ~ 'Playmaker',
      pas_cluster == 2 ~ 'Ball receiver'
    ))


info_off

### GENERAL RANKING ###

clustered_data$score <- pc$x[,1]*pc$sdev[1]**2+pc$x[,2]*pc$sdev[2]**2+pc$x[,3]*pc$sdev[3]**2



general_ranking <- bind_cols(filter_data_no_gk, pc$x[,1:3], clustered_data$cluster, 
                             clustered_data$score)


### OFFENSIVE RANKING ###

clustered_data$offensive_score <- pc_off$x[,1]*pc_off$sdev[1]**2+pc_off$x[,2]*pc_off$sdev[2]**2+pc_off$x[,3]*pc_off$sdev[3]**2



offensive_ranking <- bind_cols(filter_attacking_data, pc_pos$x[,1:3], clustered_data$off_cluster, 
                               clustered_data$offensive_score)

names(offensive_ranking$...23) <- 'off_cluster'


### DEFENSIVE RANKING ###

clustered_data$defensive_score <- pc_def$x[,1]*pc_def$sdev[1]**2+pc_def$x[,2]*pc_def$sdev[2]**2



defensive_ranking <- bind_cols(filter_defending_data, pc_def$x[,1:2], clustered_data$def_cluster, 
                               clustered_data$defensive_score)


### POSSESSION RANKING ###

clustered_data$possession_score <- pc_pos$x[,1]*pc_pos$sdev[1]**2+pc_pos$x[,2]*pc_pos$sdev[2]**2



possession_ranking <- bind_cols(filter_possession_data, pc_pos$x[,1:2], clustered_data$pos_cluster, 
                                clustered_data$possession_score)



### PASSING RANKING ###

clustered_data$passing_score <- pc_pas$x[,1]*pc_pas$sdev[1]**2+pc_pas$x[,2]*pc_pas$sdev[2]**2



passing_ranking <- bind_cols(filter_passing_data, pc_pas$x[,1:2], clustered_data$pas_cluster, 
                             clustered_data$passing_score)


### GOALKEEPERS RANKING ###

filter_data_gk$gk_score <- pc_gk$x[,1]*pc_gk$sdev[1]**2+pc_gk$x[,2]*pc_gk$sdev[2]**2+pc_gk$x[,3]*pc_gk$sdev[3]**2



###

clustered_data$PC1 <- pc$x[,1]
clustered_data$PC2 <- pc$x[,2]
clustered_data$PC3 <- pc$x[,3]

clustered_data$off_PC1 <- pc_off$x[,1]
clustered_data$off_PC2 <- pc_off$x[,2]
clustered_data$off_PC3 <- pc_off$x[,3]

clustered_data$def_PC1 <- pc_def$x[,1]
clustered_data$def_PC2 <- pc_def$x[,2]

clustered_data$pas_PC1 <- pc_pas$x[,1]
clustered_data$pas_PC2 <- pc_pas$x[,2]

clustered_data$pos_PC1 <- pc_pos$x[,1]
clustered_data$pos_PC2 <- pc_pos$x[,2]

data_frame <- bind_rows(clustered_data, filter_data_gk)

#####


### DATASET CREATION ###
#I'm going to create a dataset that suits my goals and that allows me to start the modeling

team_stats <- data_frame %>%             #This df contains a weighted mean for every stat of every team
  group_by(Squad) %>%
  summarise(across(where(is.numeric), 
                   ~ weighted.mean(., w = Min, na.rm = TRUE)))



full_data <- left_join(data_frame, team_stats, by = 'Squad')

#Now we have a huge dataset where every row is a player's stats and the average stats of his team 



### WORKFLOW TO PREDICT HOW A PLAYER SHOULD PERFORM IN HIS OWN TEAM CONSIDERING HIS STATS ###

set.seed(293819)

split <- initial_split(full_data, prop = 0.8, strata = offensive_score.x)

training_data <- split %>%
  training()

test_data  <- split %>% 
  testing()

linear_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")


##offensive

# Recipe
off_recipe <- recipe(offensive_score.x ~ score.x + offensive_score.y + possession_score.y, 
                     data = training_data) %>%
  step_normalize(all_numeric_predictors())



# Workflow
offensive_workflof <- workflow() %>%
  add_model(linear_model) %>%
  add_recipe(off_recipe)

off_fit <- fit(offensive_workflof, data = training_data)

preds <- predict(off_fit, test_data) %>%
  bind_cols(test_data %>% select(Player, offensive_score.x)) %>%
  mutate(residual = offensive_score.x - .pred)

tidy(off_fit)

metrics(preds, truth = offensive_score.x, estimate = .pred)


## Goals prediction

# Recipe
gls_recipe <- recipe(Gls.x ~ offensive_score.x + Gls.y  + offensive_score.y ,
                     data = training_data) %>%
  step_normalize(all_numeric_predictors())


# Workflow
gls_workflof <- workflow() %>%
  add_model(linear_model) %>%
  add_recipe(gls_recipe)

gls_fit <- fit(gls_workflof, data = training_data)

gls_preds <- predict(gls_fit, test_data) %>%
  bind_cols(test_data %>% select(Player, Gls.x)) %>%
  mutate(residual = Gls.x - .pred)

tidy(gls_fit)

metrics(gls_preds, truth = Gls.x, estimate = .pred)

predict(gls_fit, full_data) %>%
  bind_cols(full_data %>% select(Player, Gls.x)) %>%
  arrange(desc(.pred))


### WORKFLOW  FOR PREDICTING HOW A PLAYER SHOULD PERFORM IN HIS TEAM, WITHOUT CONSIDERING HIS CONTRIBUTION ###


numeric_vars <- data_frame %>%
  select(where(is.numeric)) %>%
  select(-Min) %>%       
  colnames()


leave_one_out_means <- function(player_row, full_data, vars) {
  player <- player_row$Player
  squad <- player_row$Squad
  
  team_data <- full_data %>%
    filter(Squad == squad, Player != player)
  
  weights <- team_data$Min
  
  sapply(vars, function(var) {
    values <- team_data[[var]]
    weighted.mean(values, w = weights, na.rm = TRUE)
  }) %>% as.list()
}


leave_one_out_result <- data_frame %>%
  split(.$Player) %>%
  map_dfr(~ {
    row <- .x[1, ]
    means <- leave_one_out_means(row, data_frame, numeric_vars)
    bind_cols(row, as_tibble(means) %>% rename_with(~ paste0("team_avg_", .)))
  })




#

set.seed(18239)

second_split <- initial_split(leave_one_out_result, 0.8, strata = offensive_score)

second_train <- second_split %>%
  training()

second_test <- second_split %>%
  testing()
#


## Offensive

# Recipe
second_offensive_recipe <- recipe(offensive_score ~ MP + score + team_avg_offensive_score + team_avg_score, 
                                  data = second_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())


# Workflow
second_offensive_workflow <- workflow() %>%
  add_model(linear_model) %>%
  add_recipe(second_offensive_recipe)

second_off_fit <- fit(second_offensive_workflow, data = second_train)

tidy(second_off_fit)

second_off_pred <- predict(second_off_fit, second_test) %>%
  bind_cols(second_test %>% select(Player, offensive_score)) %>%
  mutate(residual = offensive_score - .pred)

metrics(second_off_pred, truth = offensive_score, estimate = .pred)


predict(second_off_fit, leave_one_out_result) %>%
  bind_cols(leave_one_out_result %>% select(Player, offensive_score)) %>%
  arrange(desc(.pred))


## General  (non un gran modello)

# Recipe
second_general_recipe <- recipe(score ~ MP + team_avg_offensive_score + team_avg_score + team_avg_defensive_score +
                                  team_avg_passing_score + team_avg_passing_score, 
                                data = second_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())


# Workflow
second_general_workflow <- workflow() %>%
  add_model(linear_model) %>%
  add_recipe(second_general_recipe)

second_general_fit <- fit(second_general_workflow, data = second_train)

tidy(second_general_fit)

second_general_pred <- predict(second_general_fit, second_test) %>%
  bind_cols(second_test %>% select(Player, score)) %>%
  mutate(residual = score - .pred)

metrics(second_general_pred, truth = score, estimate = .pred)










####  






##############################

no_gk <- full_data %>%
  filter(Pos != 'GK')


off_preds <- predict(off_fit, full_data) %>%
  bind_cols(full_data %>% select(Player, offensive_score.x)) %>%
  mutate(residual_1 = offensive_score.x - .pred, pred1 = .pred) %>%
  select(Player, offensive_score.x, residual_1, pred1) %>%
  distinct(Player, .keep_all = TRUE)

one_out_off_pred <- predict(second_off_fit, leave_one_out_result) %>%
  bind_cols(leave_one_out_result %>% select(Player, offensive_score)) %>%
  mutate(residual_2 = offensive_score - .pred,  pred2 = .pred) %>%
  select(Player, offensive_score, residual_2, pred2)

gls_prediction <- predict(gls_fit, full_data) %>%
  bind_cols(full_data %>% select(Player, Gls.x)) %>%
  mutate(residual_gls = Gls.x - .pred, pred3 = .pred) %>%
  select(Player, Gls.x, residual_gls, pred3) %>%
  distinct(Player, .keep_all = TRUE)





list_of_dfs <- list(off_preds, one_out_off_pred, gls_prediction)


predictions <- reduce(list_of_dfs, ~ left_join(.x, .y, by = "Player")) %>%
  select(Player:pred1, residual_2:pred3)

names(predictions) <- c('Player', 'Actual offensive score', 'Residual', 'Predicted offensive score', 
                        'Residual 2', 'Predicted offensive score (only team)', 'Goals', 'Goals residuals', 'Predicted Goals')







datasets_gruop <- list(
  'All' = data_frame,
  'Offensive' = attacking_stats,
  'Defensive' = defending_stats,
  'Possession' = possession_stats,
  'Goalkeeping' = goalkeeping_stats,
  'Passing' = passing_stats,
  'Miscellaneous' = miscellaneus_stats
)


rankings_group <- list(
  'General' = general_ranking,
  'Offensive' = offensive_ranking,
  'Defensive' = defensive_ranking,
  'Passing' = passing_ranking,
  'Possession' = possession_ranking
)

##


# UI

ui <- navbarPage('FOOTBAL PLAYERS ANALYSES',
                 
                 ## First function of the app: visualizing a single player stats
                 tabPanel("Player stats", 
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("player", "Select a player:",
                                          choices = unique(data_frame$Player)),
                              
                              selectInput("Group", "Choose the stats you want to visualize:",
                                          choices = names(datasets_gruop))
                            ),
                            
                            mainPanel(
                              h4("Statistics table"),
                              DTOutput("stats_tab"),
                              
                              h4("Statistics graph"),
                              plotOutput("stats_graph")
                              
                            )
                          )
                 ),
                 
                 ## Second function of the app: visualizing and comparing two or more players stats  
                 tabPanel('Comparison between players',
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput('player1', 'Select the first player:',
                                          choices = unique(data_frame$Player)),
                              
                              selectInput('player2', 'Select the second player:',
                                          choices = unique(data_frame$Player)),
                              
                              selectInput("Group_Comparison", "Choose the stats you want to visualize:",
                                          choices = names(datasets_gruop), selected = 'Offensive')
                            ),
                            
                            
                            mainPanel(
                              h4("Statistics table"),
                              DTOutput("player1_stats_tab"),
                              
                              
                              h4("Statistics table"),
                              DTOutput("player2_stats_tab"),
                              
                              h4('Statistics comparison graph'),
                              plotOutput('comparison_graph')
                              
                            )
                            
                          )
                 ),
                 
                 ## Third function of the app: players ranking
                 tabPanel("Players' rankings",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("Field", "Select the ranking: ",
                                          choices = names(rankings_group),),
                              
                              selectInput('Position', "Select a position",
                                          choices = c(unique(data_frame$Pos), 'All'),
                                          selected = 'All', multiple = T)
                            ),
                            
                            mainPanel(
                              h4('Ranking'),
                              DTOutput('ranking_table'),
                              
                              h4('Plot of PCs of this field'),
                              plotOutput('ranking_plot')
                            )
                          )
                 ),
                 
                 ## Fourth function of the app: modeling
                 tabPanel("Players' evaluation",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("Player", "Select a player",
                                          choices = unique(data_frame$Player))
                            ),
                            mainPanel(
                              h4('Evaluation table'),
                              DTOutput('evaluation_tab'),
                              
                              h4('Evaluation'),
                              textOutput('evaluation_text')
                            )
                          )
                 )
                 
)







# SERVER

server <- function(input, output, session) {
  
  
  ## First function of the app: visualizing a single player stats
  
  #find player data
  player_data <- reactive({
    req(input$player, input$Group)
    
    current_dataset <- datasets_gruop[[input$Group]]
    
    current_dataset %>%
      filter(Player == input$player) %>%
      select(where(is.numeric), -c(Age, Born, MP, Starts, Min)) %>%
      pivot_longer(everything(), names_to = "Statistic", values_to = "Value")
  })
  
  player_info <- reactive({
    req(input$player)
    
    data_frame %>%
      filter(Player == input$player) %>%
      select(Player, Age, Squad, Pos, off_cluster:pas_cluster, score:passing_score, gk_score)
  })
  
  #render player's data table
  output$stats_tab <- renderDT({
    datatable(player_data())
  })
  
  #render player's data graph
  output$stats_graph <- renderPlot({
    ggplot(player_data(), aes(x = Statistic, y = Value)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Statistics of", player_info()$Player, ' | Age: ', player_info()$Age, ' | Team: ',  player_info()$Squad, 
                         ' | Position: ', player_info()$Pos),
           subtitle = paste0(' | Player description: ', player_info()$off_cluster, ', ', player_info()$def_cluster, 
                             ', ', player_info()$pas_cluster, ', ', player_info()$pos_cluster, player_info()$gk_cluster))
    
  })
  
  
  
  ## Second function of the app: visualizing and comparing two or more players stats  
  
  player1_data = reactive({
    req(input$player1, input$Group_Comparison)
    
    comparison_dataset <- datasets_gruop[[input$Group_Comparison]]
    if (is.null(comparison_dataset)) {
      showNotification("Errore: gruppo non trovato nel dizionario!", type = "error")
      return(NULL)
    }
    
    
    
    
    comparison_dataset %>%
      filter(Player == input$player1) %>%
      select(where(is.numeric), -c(Age, Born, Min, MP, Starts)) %>%
      pivot_longer(everything(), names_to = "Statistic", values_to = "Value")
  })
  
  player_1_info <- reactive({
    req(input$player1)
    
    data_frame %>%
      filter(Player == input$player1) %>%
      select(Player, Age, Squad, Pos, off_cluster:pas_cluster, score:passing_score, gk_score) %>%
      slice(1)
  })
  
  output$player1_stats_tab <- renderDT({
    datatable(player1_data(), caption = paste0('Statistics of ', input$player1))
  })
  
  
  
  ################################################################################
  
  player2_data = reactive({
    req(input$player2, input$Group_Comparison)
    
    comparison_dataset <- datasets_gruop[[input$Group_Comparison]]
    if (is.null(comparison_dataset)) {
      showNotification("Errore: gruppo non trovato nel dizionario!", type = "error")
      return(NULL)
    }
    
    comparison_dataset %>%
      filter(Player == input$player2) %>%
      select(where(is.numeric), -c(Age, Born, Min, MP, Starts)) %>%
      pivot_longer(everything(), names_to = "Statistic", values_to = "Value")
  })
  
  
  player_2_info <- reactive({
    req(input$player2)
    
    data_frame %>%
      filter(Player == input$player2) %>%
      select(Player, Age, Squad, Pos, off_cluster:pas_cluster, score:passing_score, gk_score) %>%
      slice(1)
    
  })
  
  output$player2_stats_tab <- renderDT({
    datatable(player2_data(), caption = paste0('Statistics of ', input$player2))
  })
  
  ####################
  
  output$comparison_graph <- renderPlot({
    validate(
      need(nrow(player1_data()) > 0, "Nessun dato per il primo giocatore."),
      need(nrow(player2_data()) > 0, "Nessun dato per il secondo giocatore.")
    )
    
    print(paste("Player 1:", input$player1))
    print(paste("Group selected:", input$Group_Comparison))
    
    
    g1 <- ggplot(player1_data(), aes(x = Statistic, y = Value)) +
      geom_col(fill = 'steelblue') +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Statistics of", player_1_info()$Player, ' | Age: ', player_1_info()$Age, ' | Team: ',  player_1_info()$Squad, 
                         ' | Position: ', player_1_info()$Pos),
           subtitle = paste0(' | Player description: ', player_1_info()$off_cluster, ', ', player_1_info()$def_cluster, 
                             ', ', player_1_info()$pas_cluster, ', ', player_1_info()$pos_cluster, player_1_info()$gk_cluster))  
    
    g2 <-  ggplot(player2_data(), aes(x = Statistic, y = Value)) +
      geom_col(fill = 'red') +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Statistics of", input$player2, ' | Age: ', player_2_info()$Age, ' | Team: ',  player_2_info()$Squad, 
                         ' | Position: ', player_2_info()$Pos),
           subtitle = paste0(' | Player description: ', player_2_info()$off_cluster, ', ', player_2_info()$def_cluster, 
                             ', ', player_2_info()$pas_cluster, ', ', player_2_info()$pos_cluster, player_2_info()$gk_cluster))
    
    g1/g2
  })
  
  ## Third function
  
  ranking_data <- reactive({
    
    if (input$Field == 'General') {
      df <- general_ranking %>% 
        arrange(desc(...50))
    }
    
    else if (input$Field == 'Offensive') {
      df <- offensive_ranking %>% 
        arrange(desc(...24)) 
    }
    
    else if (input$Field == 'Defensive') {
      df <- defensive_ranking %>%
        arrange(desc(...22))
    }
    
    else if (input$Field == 'Possession') {
      df <- possession_ranking %>%
        arrange(desc(...20))
    }
    
    else if (input$Field == 'Passing') {
      df <- passing_ranking %>%
        arrange(desc(...21))
    }
    
    
    if (!("All" %in% input$Position)) {
      df <- df %>%
        filter(Pos %in% input$Position)
    }
    
    
    return(df)
    
  })
  
  output$ranking_table <- renderDT(
    datatable(ranking_data())
  )
  
  output$ranking_plot <- renderPlot({
    top50 <- ranking_data() %>% 
      head(50)
    ggplot(top50, aes(x = PC1, y = PC2, label = Player, fill = Pos)) +
      geom_text_repel() +
      labs(title = paste(input$Field, " Top 50"))
  })
  
  ## Fourth funcion
  
  evaluation <- reactive({
    req(input$Player)
    
    evaluation_data <- predictions %>%
      filter(Player == input$Player) %>%
      select(`Actual offensive score`, `Predicted offensive score`, Residual, `Predicted offensive score (only team)`, 
             `Residual 2`, Goals, `Predicted Goals`, `Goals residuals`) %>%
      pivot_longer(everything(), names_to = "Statistic", values_to = "Value")
    
    
    
    
    
  })
  
  output$evaluation_tab <- renderDT(
    datatable(evaluation(), caption = paste0('Evaluation of ', input$Player))
  )
  
  
  
  output$evaluation_text <- renderPrint(
    output$evaluation_text <- renderPrint({
      
      req(evaluation())
      
      ev <- evaluation()
      
      residual <- ev %>% filter(Statistic == "Residual") %>% pull(Value)
      residual2 <- ev %>% filter(Statistic == "Residual 2") %>% pull(Value)
      goal_residual <- ev %>% filter(Statistic == "Goals residuals") %>% pull(Value)
      
      text1 <- if (residual > 0) {
        paste0(input$Player, ' is over-performing considering his contribution to his team')
      } else {
        paste0(input$Player, ' is under-performing considering his contribution to his team')
      }
      
      text2 <- if (residual2 > 0) {
        paste0(' | ', input$Player, ' is over-performing not considering his contribution to his team')
      } else {
        paste0(' | ', input$Player, ' is under-performing not considering his contribution to his team')
      }
      
      text3 <- if (goal_residual > 0) {
        paste0(' | ', input$Player, ' scored more than predicted')
      } else {
        paste0(' | ', input$Player, ' scored less than predicted')
      }
      
      cat(paste0(text1, text2, text3))
    })
    
  )
  
  
  
}







shinyApp(ui, server)



