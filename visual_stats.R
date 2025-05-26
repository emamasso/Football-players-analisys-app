### In this section i've written all the code for visualising the statistics of a player
### and selecting the ones we are more intrested in (i used one single player just to see
### if the code worked, but in the final version there will be the possibility for the user
### to select the player they want)


## LIBRARIES ## 

library(DT)
library(tidyverse)


## VISUALIZATION OF ALL THE STATS OF A PLAYER WITH A TABLE AND A BARPLOT

stats <- data %>%
  filter(Player == 'Albert Guðmundsson') 

stats_table <- datatable(stats)

stats_long <- stats[8:53] %>%
  select(-Touches, -Carries) %>%
  pivot_longer(everything(), names_to = 'Statistic', values_to = 'Value') 

ggplot(stats_long, aes(x = Statistic, y = Value)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = paste("Statistics of", stats$Player, ', age ', stats$Age, ', born in ', stats$Born))



## VISUALIZATION OF A SUBSET OF THE STATS OF A PLAYER WITH A TABLE AND A BARPLOT (here one can see
## just one subset, in the final version there will be a function to select the subset desired)

off_stats <- attacking_stats %>%
  filter(Player == 'Albert Guðmundsson')

off_stats_table <- datatable(off_stats)

off_stats_long <- off_stats[8:15] %>%
  pivot_longer(everything(), names_to = 'Statistic', values_to = 'Value')

ggplot(off_stats_long, aes(x = Statistic, y = Value)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = paste("Offensive statistics of", player, ', age ', player$Age, ', born in ', player$Born))
