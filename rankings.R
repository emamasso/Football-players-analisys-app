### In this script one can find all the code needed to create a ranking system for 
### the players based on what was done in the PCA script (the rankings are made starting 
### from the PCA itself). As always this is a trial version for the final app.

### The rankings will be determined by a score of the PCs, where the weights are
### the variance explained


### GENERAL RANKING ###

clustered_data$score <- pc$x[,1]*pc$sdev[1]**2+pc$x[,2]*pc$sdev[2]**2+pc$x[,3]*pc$sdev[3]**2

clustered_data %>%
  arrange(desc(score)) %>%
  select(Player, score)

general_ranking <- bind_cols(filter_data_no_gk, pc$x[,1:3], clustered_data$cluster, 
                                clustered_data$score)


### OFFENSIVE RANKING ###

clustered_data$offensive_score <- pc_off$x[,1]*pc_off$sdev[1]**2+pc_off$x[,2]*pc_off$sdev[2]**2+pc_off$x[,3]*pc_off$sdev[3]**2

clustered_data %>%
  arrange(desc(offensive_score)) %>%
  select(Player, offensive_score)

offensive_ranking <- bind_cols(filter_attacking_data, pc_pos$x[,1:3], clustered_data$off_cluster, 
                                clustered_data$offensive_score)

names(offensive_ranking$...23) <- 'off_cluster'

 
### DEFENSIVE RANKING ###

clustered_data$defensive_score <- pc_def$x[,1]*pc_def$sdev[1]**2+pc_def$x[,2]*pc_def$sdev[2]**2

clustered_data %>%
  arrange(desc(defensive_score)) %>%
  select(Player, defensive_score)

defensive_ranking <- bind_cols(filter_defending_data, pc_def$x[,1:2], clustered_data$def_cluster, 
                                clustered_data$defensive_score)


### POSSESSION RANKING ###

clustered_data$possession_score <- pc_pos$x[,1]*pc_pos$sdev[1]**2+pc_pos$x[,2]*pc_pos$sdev[2]**2

clustered_data %>%
  arrange(desc(possession_score)) %>%
  select(Player, possession_score)

possession_ranking <- bind_cols(filter_possession_data, pc_pos$x[,1:2], clustered_data$pos_cluster, 
                                clustered_data$possession_score)



### PASSING RANKING ###

clustered_data$passing_score <- pc_pas$x[,1]*pc_pas$sdev[1]**2+pc_pas$x[,2]*pc_pas$sdev[2]**2

clustered_data %>%
  arrange(desc(passing_score)) %>%
  select(Player, passing_score)

passing_ranking <- bind_cols(filter_passing_data, pc_pas$x[,1:2], clustered_data$pas_cluster, 
                                clustered_data$passing_score)


### GOALKEEPERS RANKING ###

filter_data_gk$gk_score <- pc_gk$x[,1]*pc_gk$sdev[1]**2+pc_gk$x[,2]*pc_gk$sdev[2]**2+pc_gk$x[,3]*pc_gk$sdev[3]**2

filter_data_gk  %>%
  arrange(desc(score)) %>%
  select(Player, score)

