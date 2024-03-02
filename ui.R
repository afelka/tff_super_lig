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

#define unique team choices
unique_teams <- unique(sort(games_combined_cleaned_final$home_teams))

team_choices <- setNames(unique_teams, unique_teams)

#design shiny app: 
shinyUI(fluidPage(
  
  
  titlePanel("Turkish Super League Scores"),
  
  wellPanel(
    
    #style = "background: #ff6666",
    
    selectInput("team", "Choose First Team", 
                   choices = team_choices, selected = "GALATASARAY"),
    
    selectInput("team2", "Choose Second Team", 
                   choices = team_choices, selected = "FENERBAHÇE"),
    
    sliderInput("seasons_selected", "Select Season Interval (1 = 1959 , 65 = 2022-2023)",
                min = min(games_combined_cleaned_final$season_number), 
                max = max(games_combined_cleaned_final$season_number),
                value = c(min(games_combined_cleaned_final$season_number),
                          max(games_combined_cleaned_final$season_number)),
                step = 1 )
    
    
  ),
  
  mainPanel(
    
    tabsetPanel(type = "tabs",
                
                tabPanel("Scores between Selected Teams", htmlOutput("selected_seasons_text"), DT::dataTableOutput("table1")),
                
                tabPanel("All Scores by a Team", htmlOutput("selected_text"), DT::dataTableOutput("table2")),
                
                tabPanel("All Scores", htmlOutput("selected_text2"), DT::dataTableOutput("table3")),
                
                tabPanel("List of Games with the Selected Score", uiOutput("score"), htmlOutput("selected_text4"), DT::dataTableOutput("table4")),
                         
                tabPanel("Avg. Points vs. Avg. Goals Scored/Conceded", htmlOutput("selected_text3"), plotlyOutput('plot',width = "150%",height = "600px")),
                
                tabPanel("Most Consecutive Wins/Losses/Draws by Selected Team", htmlOutput("selected_text5"), DT::dataTableOutput("table5")),
                
                tabPanel("Most Consecutive Scores by Any Team", htmlOutput("selected_text6"), DT::dataTableOutput("table6")),
                
                tabPanel("Avg. Goals Per Game over the Years", htmlOutput("selected_text7"), plotlyOutput('plot2',width = "150%",height = "600px")),
                
                tabPanel("Most Consecutive Games with X goals", htmlOutput("selected_text8"), DT::dataTableOutput("table7")),
                
                tabPanel("Most Consecutive Games with at least X goals", uiOutput("no_of_goals"), htmlOutput("selected_text9"), DT::dataTableOutput("table8"))
                
                
                )
                
    )
    
  )
  
)