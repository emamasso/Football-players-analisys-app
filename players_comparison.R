### In this script one can find the code needed for compare two or more players
### that will be implemented in the final app (as always here the comparison will be 
### between two pre-determined player, in the app the user will be able to choose).


## LIBRARIES ##

library(patchwork)

## ##

player1 <- data %>%
  filter(Player == 'Kylian Mbappé')

player2 <- data %>%
  filter(Player == 'Erling Haaland')

players <- as.data.frame(rbind(player1, player2))

datatable(player1)
datatable(player2)

stats_long_pl1 <- player1[,11:56] %>%
  select(-Touches, -Carries) %>%
  pivot_longer(everything(), names_to = 'Statistic', values_to = 'Value')


stats_long_pl2 <- player2[,11:56] %>%
  select(-Touches, -Carries) %>%
  pivot_longer(everything(), names_to = 'Statistic', values_to = 'Value')


g1 <- ggplot(stats_long_pl1, aes(x = Statistic, y = Value)) +
  geom_col(fill = 1) +
  coord_flip() +
  theme_minimal() +
  labs(title = paste("Statistics of", player1$Player, ', age ', player1$Age, ', born in ', player1$Born))

g2 <- ggplot(stats_long_pl2, aes(x = Statistic, y = Value)) +
  geom_col(fill = 2) +
  coord_flip() +
  theme_minimal() +
  labs(title = paste("Statistics of", player2$Player, ', age ', player2$Age, ', born in ', player2$Born))




