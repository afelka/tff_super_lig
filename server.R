#load packages
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

#loading data (not available in the repo as I spent lots of time gathering data, you can use the shinyapp if you want)
#https://eea1.shinyapps.io/Turkish_Super_League_Scores/
load("games_combined.RData")

#create tables
function(input, output) {

output$score <- renderUI({
  selectInput("score", "Select Score", 
              choices = games_combined_cleaned_final %>% filter((home_teams == input$team & 
                                                                         away_teams == input$team2 )|
                                                                        (home_teams == input$team2 & 
                                                                           away_teams == input$team ) ) %>% 
                                arrange(home_team_goals, away_team_goals) %>%
                                mutate(Score = paste(home_team_goals, away_team_goals, sep = "-")) %>% select(Score) %>% unique()
              
  ) 
  
})

output$table1 <- DT::renderDataTable({
  
data <- games_combined_cleaned_final %>% filter((home_teams == input$team & 
                                                 away_teams == input$team2 )|
                                                   (home_teams == input$team2 & 
                                                      away_teams == input$team ) ) %>% 
    mutate(Score = paste(home_team_goals, away_team_goals, sep = "-")) %>%
    group_by(home_teams, away_teams, Score) %>%
    summarise(Number_of_Times = n()) %>% arrange(desc(Number_of_Times)) %>%
    dplyr::rename(Home_Team = "home_teams",
           Away_Team = "away_teams")

datatable(data, options = list(dom = 'tpi'), filter = list(position = "bottom"))

 
})

output$selected_text <- renderText({ 
  
  paste0("<B>This table works for the first team and scores are also showing accordingly (e.g. 1-0 Away means selected team won that game). The team selected
         in this case is ", input$team,"</B>")
  
  
}) 

output$table2 <- DT::renderDataTable({
  
  data2 <- games_combined_cleaned_final %>% filter(home_teams == input$team) %>% 
    mutate(Score = paste(home_team_goals, away_team_goals, sep = "-"),
           Venue = "Home") %>%
    dplyr::rename(Selected_Team = "home_teams",
                  Opponent_Team = "away_teams",
                  Selected_Team_Goals = "home_team_goals",
                  Opponent_Team_Goals = "away_team_goals")
  
  data3 <- games_combined_cleaned_final %>% filter(away_teams == input$team) %>% 
    mutate(Score = paste(away_team_goals, home_team_goals, sep = "-"),
           Venue = "Away") %>%
    dplyr::rename(Selected_Team = "away_teams",
                  Opponent_Team = "home_teams",
                  Selected_Team_Goals = "away_team_goals",
                  Opponent_Team_Goals = "home_team_goals")
  
  data4 <- rbind(data2,data3) %>% group_by(Selected_Team, Score , Venue) %>%
                                  summarise(Number_of_Times = n()) %>% arrange(desc(Number_of_Times))
                              
 
  datatable(data4, options = list(dom = 'tpi'), filter = list(position = "bottom"))  
  
})

output$selected_text2 <- renderText({ 
  
  paste0("<B>This table works independent of team selections and shows all time scores </B>")
  
  
}) 

output$table3 <- DT::renderDataTable({
  
  data5 <- games_combined_cleaned_final %>% 
    mutate(Score = paste(home_team_goals, away_team_goals, sep = "-")) %>%
    group_by(Score) %>% summarise(Number_of_Times = n()) %>% arrange(desc(Number_of_Times))
  
  datatable(data5, options = list(dom = 'tpi'), filter = list(position = "bottom"))
  
})

output$table4 <- DT::renderDataTable({
  
  data6 <- games_combined_cleaned_final %>% 
    filter((home_teams == input$team & 
              away_teams == input$team2 )|
             (home_teams == input$team2 & 
                away_teams == input$team )) %>% 
    mutate(Score = paste(home_team_goals, away_team_goals, sep = "-")) %>%
    filter(Score == input$score) %>% select(home_teams, away_teams, Score, season) %>%
    dplyr::rename(Home_Team = "home_teams",
                  Away_Team = "away_teams",
                  Season = "season") %>% arrange(Season)
    
  datatable(data6, options = list(dom = 'tpi'), filter = list(position = "bottom"))  
  
})


}