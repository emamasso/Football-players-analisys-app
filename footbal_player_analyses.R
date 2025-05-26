
## LIBRARIES ##

library(tidyverse)
library(shiny)
library(reticulate)
library(DT)
library(patchwork)
library(ggrepel)
library(tidymodels)
library(yardstick)
library(vip)
library(themis)
library(rsconnect)

## 

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

  

## LINK TO THE FINAL VERSION OF THE WEB APP: https://emamasso.shinyapps.io/fpa_2/