#load packages
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)

#loading data (not available in the repo as I spent lots of time gathering data, you can use the shinyapp if you want)
#https://eea1.shinyapps.io/Turkish_Super_League_Scores/
load("games_version_28022024.RData")

games_combined_cleaned_final <- games_with_new_data

games_combined_cleaned_final <- games_combined_cleaned_final %>% group_by(season) %>% 
                                     mutate(season_number = cur_group_id()) %>%
                                     ungroup()



#create tables
function(input, output) {
  
seasons_selected <- reactive({
    
data <- games_combined_cleaned_final %>% filter(season_number >= min(input$seasons_selected) &
                                                season_number <= max(input$seasons_selected))
    
})  

first_season <- reactive({
  
games_combined_cleaned_final %>% filter(season_number == min(input$seasons_selected)) %>%  distinct(season) %>%
                            as.character()

})

last_season <- reactive({
  
  games_combined_cleaned_final %>% filter(season_number == max(input$seasons_selected)) %>%  distinct(season) %>%
    as.character()
  
})
  
  
output$score <- renderUI({
  selectInput("score", "Select Score", 
              choices = games_combined_cleaned_final %>% filter((home_teams == input$team & 
                                                                         away_teams == input$team2 )|
                                                                        (home_teams == input$team2 & 
                                                                           away_teams == input$team )  ) %>% 
                                arrange(home_team_goals, away_team_goals) %>%
                                mutate(Score = paste(home_team_goals, away_team_goals, sep = "-")) %>% select(Score) %>% unique()
              
  ) 
  
})

output$table1 <- DT::renderDataTable({
  
data <- seasons_selected() %>% filter((home_teams == input$team & 
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




output$selected_seasons_text <- renderText({ 
  
  
  
  paste0("<B>Season interval is between ", first_season()," and ",last_season() ," ","</B>")
  
  
}) 

output$selected_text <- renderText({ 
  
  paste0("<B>This table works for the first team and scores are also showing accordingly (e.g. 1-0 Away means selected team won that game). The team selected
         in this case is ", input$team, " and Season interval is between ", first_season()," and ",last_season() ," ", "</B>")
  
  
}) 

output$table2 <- DT::renderDataTable({
  
  data2 <- seasons_selected() %>% filter(home_teams == input$team) %>% 
    mutate(Score = paste(home_team_goals, away_team_goals, sep = "-"),
           Venue = "Home") %>%
    dplyr::rename(Selected_Team = "home_teams",
                  Opponent_Team = "away_teams",
                  Selected_Team_Goals = "home_team_goals",
                  Opponent_Team_Goals = "away_team_goals")
  
  data3 <- seasons_selected() %>% filter(away_teams == input$team) %>% 
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
  
  paste0("<B>This table works independent of team selections and shows all time scores and Season interval is between ", first_season()," and ",last_season() ," " ,"</B>")
  
  
}) 

output$table3 <- DT::renderDataTable({
  
  data5 <- seasons_selected() %>% 
    mutate(Score = paste(home_team_goals, away_team_goals, sep = "-")) %>%
    group_by(Score) %>% summarise(Number_of_Times = n()) %>% arrange(desc(Number_of_Times))
  
  datatable(data5, options = list(dom = 'tpi'), filter = list(position = "bottom"))
  
})

output$table4 <- DT::renderDataTable({
  
  data6 <- seasons_selected() %>% 
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

output$selected_text3 <- renderText({ 
  
  paste0("<B>This table works for the first team. In all seasons, calculations made by assuming
         teams get 3 points for wins and 1 point for draws. 
         The team selected in this case is ", input$team," and Season interval is between ", first_season()," and ",last_season() ," " ,"</B>")

}) 

output$selected_text4 <- renderText({ 
  
  paste0("<B>Season interval is between ", first_season()," and ",last_season() ," ","</B>")
  
}) 

output$plot <- renderPlotly({
  
  data7 <- seasons_selected() %>% filter(home_teams == input$team) %>% 
    mutate(Score = paste(home_team_goals, away_team_goals, sep = "-"),
           Venue = "Home") %>%
    dplyr::rename(Selected_Team = "home_teams",
                  Opponent_Team = "away_teams",
                  Selected_Team_Goals = "home_team_goals",
                  Opponent_Team_Goals = "away_team_goals") 
  
  data8 <- seasons_selected() %>% filter(away_teams == input$team) %>% 
    mutate(Score = paste(away_team_goals, home_team_goals, sep = "-"),
           Venue = "Away") %>%
    dplyr::rename(Selected_Team = "away_teams",
                  Opponent_Team = "home_teams",
                  Selected_Team_Goals = "away_team_goals",
                  Opponent_Team_Goals = "home_team_goals")
  
  data9 <- rbind(data7,data8) %>% 
    mutate(Selected_Team_Goals = as.numeric(gsub("[^0-9-]", "",Selected_Team_Goals)),
           Opponent_Team_Goals = as.numeric(gsub("[^0-9-]", "",Opponent_Team_Goals))) %>% 
    mutate(Points = if_else(Selected_Team_Goals == Opponent_Team_Goals, 1,
                            if_else(Selected_Team_Goals > Opponent_Team_Goals,3,0 ))) %>% 
    group_by(Selected_Team, season) %>%
    summarise(avg_points = round(mean(Points),2),
              avg_goals_scored = round(mean(Selected_Team_Goals),2),
              avg_goals_conceded = round(mean(Opponent_Team_Goals),2))
  
  gg <- ggplot(data9, aes(x = season)) +  
    geom_point(aes(y = avg_points, colour  = "avg_points")) +  
    #geom_text(aes(y = avg_points, label = avg_points), vjust = 1, color = "darkorange", size = 3 ) +
    geom_point(aes(y = avg_goals_scored , colour  = "avg_goals_scored")) + 
    #geom_text(aes(y = avg_goals_scored, label = avg_goals_scored), vjust = 1, color = "blue", size = 3 ) +
    geom_point(aes(y = avg_goals_conceded , colour  = "avg_goals_conceded")) + 
    #geom_text(aes(y = avg_goals_conceded, label = avg_goals_conceded), vjust = 1, color = "red", size = 3 ) +
    theme_classic()  + 
    theme(plot.title = element_text(size=22))+
    theme(plot.title = element_text(hjust = 0.5)) +  
    theme(axis.title.x=element_blank()) +
    theme(axis.title.y=element_blank()) +
    theme(axis.text.x = element_text(angle = 80, hjust = 1,size=12)) + 
    theme(legend.position="top") +
    labs(color = NULL) +
    scale_color_manual(values = c( "avg_goals_scored" = "blue", "avg_points" = "darkorange", "avg_goals_conceded" = "red"), 
                       labels = c("Avg. Goals Conceded",  "Avg. Goals Scored", "Avg. Points"))
  
  p <- plotly_build(gg)
  
  length<-length(p$x$data)
  invisible(lapply(1:length, function(x) p$x$data[[x]]<<-c(p$x$data[[x]], textposition ='top center')))
  
  p
})

output$selected_text5 <- renderText({ 
  
  paste0("<B> This page works only for the seasons after 1972-1973. The team selected
         in this case is ", input$team, " Season interval is between ", first_season()," and ",last_season() ," ","</B>")
  
}) 

output$table5 <- DT::renderDataTable({
  
  data2 <- seasons_selected() %>% filter(home_teams == input$team) %>% 
    mutate(Score = paste(home_team_goals, away_team_goals, sep = "-"),
           Venue = "Home") %>%
    dplyr::rename(Selected_Team = "home_teams",
                  Opponent_Team = "away_teams",
                  Selected_Team_Goals = "home_team_goals",
                  Opponent_Team_Goals = "away_team_goals")
  
  data3 <- seasons_selected() %>% filter(away_teams == input$team) %>% 
    mutate(Score = paste(away_team_goals, home_team_goals, sep = "-"),
           Venue = "Away") %>%
    dplyr::rename(Selected_Team = "away_teams",
                  Opponent_Team = "home_teams",
                  Selected_Team_Goals = "away_team_goals",
                  Opponent_Team_Goals = "home_team_goals")
  
  data_consecutive <- rbind(data2,data3) %>% filter(!is.na(dates_of_games)) %>% 
                      arrange(dates_of_games) %>%
                      mutate(Result = if_else(Selected_Team_Goals > Opponent_Team_Goals, "Win",
                             if_else(Selected_Team_Goals == Opponent_Team_Goals, "Draw", "Loss"))) %>%
                      mutate(is_consecutive = Result == lag(Result, default = first(Result))) %>%
                      group_by(Result, grp = cumsum(!is_consecutive)) %>%
                      mutate(consecutive_count = n()) %>% ungroup()
  
  data_consecutive_summarised <- data_consecutive %>% group_by(Selected_Team, Result) %>% 
                                 summarise(Max_Consecutive = max(consecutive_count)) %>% arrange(desc(Result))
                                
            
  datatable(data_consecutive_summarised, options = list(dom = 'tpi'), filter = list(position = "bottom"))  
  
})


output$selected_text6 <- renderText({ 
  
  paste0("<B> This page works only for the seasons after 1972-1973. It is independent of Selected Team. Season interval is between ", first_season()," and ",last_season() ," ","</B>")
  
}) 

output$table6 <- DT::renderDataTable({
  
  data2 <- seasons_selected() %>% 
    mutate(Score = paste(home_team_goals, away_team_goals, sep = "-"),
           Venue = "Home") %>%
    dplyr::rename(Selected_Team = "home_teams",
                  Opponent_Team = "away_teams",
                  Selected_Team_Goals = "home_team_goals",
                  Opponent_Team_Goals = "away_team_goals")
  
  data3 <- seasons_selected() %>% 
    mutate(Score = paste(away_team_goals, home_team_goals, sep = "-"),
           Venue = "Away") %>%
    dplyr::rename(Selected_Team = "away_teams",
                  Opponent_Team = "home_teams",
                  Selected_Team_Goals = "away_team_goals",
                  Opponent_Team_Goals = "home_team_goals")
  
  data_consecutive_by_score <- rbind(data2,data3) %>% filter(!is.na(dates_of_games)) %>% 
    arrange(Selected_Team,dates_of_games) %>% distinct() %>%
    mutate(Result = if_else(Selected_Team_Goals > Opponent_Team_Goals, "Win",
                            if_else(Selected_Team_Goals == Opponent_Team_Goals, "Draw", "Loss"))) %>%
    mutate(is_consecutive = Score == lag(Score, default = first(Score))) %>%
    group_by(Selected_Team, Score, grp = cumsum(!is_consecutive)) %>%
    mutate(consecutive_count = n()) %>% ungroup()
  
  data_consecutive_by_score_summarised <- data_consecutive_by_score %>% group_by(Selected_Team, Score) %>% 
    summarise(Max_Consecutive = max(consecutive_count)) %>% ungroup() %>% group_by(Score) %>%
    filter(Max_Consecutive == max(Max_Consecutive)) %>%
    summarise(Max_Consecutive = first(Max_Consecutive),
              Teams_List = toString(Selected_Team)) %>% arrange(desc(Max_Consecutive))
  
  
  datatable(data_consecutive_by_score_summarised, options = list(dom = 'tpi'), filter = list(position = "bottom"))  
  
})

output$selected_text7 <- renderText({ 
  
  paste0("<B> This page is independent of Selected Team and shows avg. goals per game per season. Season interval is between ", first_season()," and ",last_season() ," ","</B>")
  
}) 

output$plot2 <- renderPlotly({
  
  data7 <- seasons_selected() %>% mutate(total_goals = as.numeric(gsub("[^0-9-]", "",home_team_goals))
                                                                  + as.numeric(gsub("[^0-9-]", "",away_team_goals))) %>%
           group_by(season) %>% summarise(no_of_matches = n(),
                                          sum_total_goals = sum(total_goals),
                                          avg_goals = round(sum_total_goals/no_of_matches,2))
  
  gg2 <- ggplot(data7, aes(x = season)) +  
    geom_point(aes(y = avg_goals, colour  = "avg_goals")) +  
    theme_classic()  + 
    theme(plot.title = element_text(size=22))+
    theme(plot.title = element_text(hjust = 0.5)) +  
    theme(axis.title.x=element_blank()) +
    theme(axis.title.y=element_blank()) +
    theme(axis.text.x = element_text(angle = 80, hjust = 1,size=12)) + 
    theme(legend.position="top") +
    labs(color = NULL) +
    scale_color_manual(values = c( "avg_goals" = "blue"), 
                       labels = c("Avg. Goals"))
  
  p2 <- plotly_build(gg2)
  
  length<-length(p2$x$data)
  invisible(lapply(1:length, function(x) p2$x$data[[x]]<<-c(p2$x$data[[x]], textposition ='top center')))
  
  p2
})

output$selected_text8 <- renderText({ 
  
  paste0("<B> This page works only for the seasons after 1972-1973. The team selected
         in this case is ", input$team, " Season interval is between ", first_season()," and ",last_season() ," ","</B>")
  
}) 

output$table7 <- DT::renderDataTable({
  
  data2 <- seasons_selected() %>% filter(home_teams == input$team) %>%
           mutate(home_team_goals = as.numeric(gsub("[^0-9-]", "",home_team_goals)),
                  away_team_goals = as.numeric(gsub("[^0-9-]", "",away_team_goals))) %>%
           dplyr::rename(Selected_Team = "home_teams",
                  Opponent_Team = "away_teams",
                  Selected_Team_Goals = "home_team_goals",
                  Opponent_Team_Goals = "away_team_goals")
  
  data3 <- seasons_selected() %>% filter(away_teams == input$team) %>% 
           mutate(home_team_goals = as.numeric(gsub("[^0-9-]", "",home_team_goals)),
           away_team_goals = as.numeric(gsub("[^0-9-]", "",away_team_goals))) %>%
           dplyr::rename(Selected_Team = "away_teams",
                  Opponent_Team = "home_teams",
                  Selected_Team_Goals = "away_team_goals",
                  Opponent_Team_Goals = "home_team_goals")
  
  data_consecutive_by_goals <- rbind(data2,data3) %>% filter(!is.na(dates_of_games)) %>% 
    arrange(Selected_Team,dates_of_games) %>% distinct() %>%
    mutate(is_consecutive = Selected_Team_Goals == lag(Selected_Team_Goals, default = first(Selected_Team_Goals))) %>%
    group_by(Selected_Team, Selected_Team_Goals, grp = cumsum(!is_consecutive)) %>%
    mutate(consecutive_count = n()) %>% ungroup()
  
  data_consecutive_by_goals_summarised <- data_consecutive_by_goals %>% 
    group_by(Selected_Team, Selected_Team_Goals) %>% 
    summarise(Max_Consecutive = max(consecutive_count)) %>% ungroup() %>% group_by(Selected_Team_Goals) %>%
    filter(Max_Consecutive == max(Max_Consecutive)) %>%
    summarise(Max_Consecutive = first(Max_Consecutive)) %>% arrange(Selected_Team_Goals)
  
  
  datatable(data_consecutive_by_goals_summarised, options = list(dom = 'tpi'), filter = list(position = "bottom"))  
  
})


}