### In this script various PCAs and a clusterings are conducted, so that I can 
### determine a ranking among players in general and with some filter 
### (e.g. selecting a specifing role).
### As always this works as a trial for the final app



## LIBRARIES ##

library(tidyverse)
library(cluster)


## PCAs ##

# First of all I'm going to scale the variables and to find the significant PCs. I'm going to do that
# on different data frames (one for GK and one for all the other players).


# Non GK

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

fviz_nbclust(pc$x[,1:3], kmeans, method = "wss")
fviz_nbclust(pc$x[,1:3], kmeans, method = "silhouette")  ## The perfect number of cluster is 3, as expected

pc$Cluster <- as.factor(km$cluster)

filter_data_no_gk$Cluster <- as.factor(km$cluster)

info <- filter_data_no_gk[, 11:42] %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = T))
# seems like the group number 1 is the forward group, number 3 is the midfielder one and 2 the difender one


## Now let's do the same on the more specific clusters

### offensive stats ###

fviz_nbclust(pc_off$x[,1:3], kmeans, method = "wss")
fviz_nbclust(pc_off$x[,1:3], kmeans, method = "silhouette")  ## The perfect number of cluster is 2

km_off <- kmeans(pc_off$x[,1:3], centers = 2)

filter_attacking_data$Cluster <- as.factor(km_off$cluster)

info_off <- filter_attacking_data[, 11:19] %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = T))


## We can't get many information here, just that we have two categories of players: the offensive players and the 
## non offensive ones


### defensive stats ###

fviz_nbclust(pc_def$x[,1:2], kmeans, method = "wss")
fviz_nbclust(pc_def$x[,1:2], kmeans, method = "silhouette")  ## The perfect number of cluster is 2

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

fviz_nbclust(pc_pas$x[,1:2], kmeans, method = "wss")
fviz_nbclust(pc_pas$x[,1:2], kmeans, method = "silhouette")  ## The perfect number of cluster is 2

km_pas <- kmeans(pc_pas$x[,1:2], centers = 2)

filter_passing_data$Cluster <- km_pas$cluster

info_pas <- filter_passing_data[,11:17] %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = T))


### possession stats ###

fviz_nbclust(pc_pos$x[,1:2], kmeans, method = "wss")
fviz_nbclust(pc_pos$x[,1:2], kmeans, method = "silhouette")  ## The perfect number of cluster is 2

km_pos <- kmeans(pc_pos$x[,1:2], centers = 3)

filter_possession_data$Cluster <- km_pos$cluster

info_pos <- filter_possession_data[,11:16] %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = T))


### goalkeepers ###

fviz_nbclust(pc_gk$x[,1:3], kmeans, method = "wss")
fviz_nbclust(pc_gk$x[,1:3], kmeans, method = "silhouette")  ## The perfect number of cluster is 2

km_gk <- kmeans(pc_gk$x[, 1:3], centers = 2)

filter_data_gk$Cluster <- as.factor(km_gk$cluster)

info_gk <- filter_data_gk[, 11:18] %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = T))


### offensive for forwards ###


fviz_nbclust(pc_att_off$x[,1:2], kmeans, method = "wss")
fviz_nbclust(pc_att_off$x[,1:2], kmeans, method = "silhouette")  ## The perfect number of cluster is 2

km_att_off <- kmeans(pc_att_off$x[, 1:2], centers = 2)

filter_attacking_offensive_data$Cluster <- as.factor(km_att_off$cluster)

info_off_off <- filter_attacking_offensive_data[, 11:19] %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = T))



### defensive for defenders ###

fviz_nbclust(pc_def_def$x[,1:2], kmeans, method = "wss")
fviz_nbclust(pc_def_def$x[,1:2], kmeans, method = "silhouette")  ## The perfect number of cluster is 2

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
      def_cluster == 1 ~ 'Passive defender',
      def_cluster == 2 ~ 'Aggressive defender'),
        pos_cluster = case_when(
      pos_cluster == 2 ~ 'Main ball keeper',
      pos_cluster == 3 ~ 'Involved in possession but not crucial',
      pos_cluster == 1 ~ 'Secondary receiver'
        ),
        pas_cluster = case_when(
      pas_cluster == 1 ~ 'Playmaker',
      pas_cluster == 2 ~ 'Ball receiver'
        ))




