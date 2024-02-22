#load packages
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

#loading data (not available in the repo as I spent lots of time gathering data, you can use the shinyapp if you want)
#https://eea1.shinyapps.io/Turkish_Super_League_Scores/
load("games_combined.RData")

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
                   choices = team_choices, selected = "FENERBAHÇE")
    
  ),
  
  mainPanel(
    
    tabsetPanel(type = "tabs",
                
                tabPanel("Scores between Selected Teams", DT::dataTableOutput("table1")),
                
                tabPanel("All Scores by a Team", htmlOutput("selected_text"), DT::dataTableOutput("table2")),
                
                tabPanel("All Scores", htmlOutput("selected_text2"), DT::dataTableOutput("table3")),
                
                tabPanel("List of Games with the Selected Score", uiOutput("score"), DT::dataTableOutput("table4") 
                         
                         )
                )
                
    )
    
  )
  
)