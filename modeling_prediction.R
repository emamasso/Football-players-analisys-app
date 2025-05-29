### In this script one can find all the code needed to predict a player's/team's performance 
### in some cases, for example to predict how a player is supposed to perform considering
### some of the aggregate stats of the team, in this way we can evaluate if he is over or 
### under performing. It's possible to find also the code needed to predict a player's
### potential performance all season long in another team and a prediction of how he is
### supposed to play in his team without considering his stats




### LIBRARIES ###

library(tidymodels)



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
