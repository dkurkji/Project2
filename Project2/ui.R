#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(dashboardthemes)
library(ggplot2)
library(dplyr)


#Read in the Scores Data Set
scoresData <- read_csv("C:/Users/dkurk/OneDrive/Documents/ST590 (R)/spreadspoke_scores.csv",
                       col_types = cols(over_under_line=col_double(),weather_humidity = col_number()))

#Switch to a Date format
newScoresData <- scoresData %>% mutate(schedule_date = as.Date(schedule_date,"%m/%d/%Y"))

#Filter the data and create a new over_under variable
newScoresData <- filter(newScoresData,schedule_date >= "2000-09-03" & schedule_date <= "2018-09-17") %>% 
                 arrange(desc(schedule_date)) %>% mutate(team_home = replace(team_home,team_home == "San Diego Chargers",
                                                                             "Los Angeles Chargers")) %>% 
  mutate(team_away = replace(team_away,team_away == "San Diego Chargers","Los Angeles Chargers")) %>%
  mutate(team_home = replace(team_home,team_home == "St. Louis Rams","Los Angeles Rams")) %>% 
  mutate(team_away = replace(team_away,team_away == "St. Louis Rams","Los Angeles Rams")) %>% 
  mutate(over_under = case_when(over_under_line < score_home + score_away ~ "Over",
                                               over_under_line > score_home + score_away ~ "Under",
                                               TRUE ~ "Push"))



# Define UI for application
dashboardPage(
  

              
              #add title
              dashboardHeader(title="NFL Team Over/Under Application",titleWidth=700
                              
                              ),
                 
                 #define sidebar items
                 dashboardSidebar(sidebarMenu(
                   menuItem("About", tabName = "about", icon = icon("archive")),
                   menuItem("Over/Under Chart", tabName = "chart", icon = icon("bar-chart")),
                   menuItem("Cover Margin Plot", tabName = "plot", icon = icon("line-chart")),
                   menuItem("Data",tabName = "data",icon = icon("table"))
                   
                 )), #Closes dashboard sidebar
                 
                 #define the body of the app
                 dashboardBody(
                   
                shinyDashboardThemes(
                  theme = "blue_gradient"
                ),
                   
                   tabItems(
                     
                     # First tab content
                     tabItem(tabName = "about",
            
                             fluidRow(
                               
                               #add in latex functionality if needed
                               withMathJax(),
                               
                               #two columns for each of the two items
                               column(6,
                
                                      #Description of App
                                      h1("What does this app do?"),
                                      
                                      #box to contain description
                                      box(background = "light-blue", width=12,
                                          h4("Interest in historical sports data continues to grow as sports handicappers look to the past to try and predict the future."),
                                          h4("This app specifically looks at historical Over/Under trends for NFL organizations."),
                                          h4("You can select specific situations to view how an individual team or matchup performed in regards to the Over/Under")
                                      )
                               ),
                               
                               column(6,
                                      
                                      #How to use the app
                                      h1("How to use the app?"),
                                      
                                      #box to contain description
                                      box(background = "light-blue",width=12,
                                          h4("The app consists of two separate pages. Both pages include the same controls. The user-specified inputs are located on the left hand side of each page. The resulting chart/plot will be produced on the right hand side. The data being used for the chart/plot will be shown underneath."),
                                          h4("The user first selects the team of interest from the first drop down menu. The selected team can be viewed individually or in a specified matchup. This can be done by selecting the opponent from the second drop down menu."),
                                          h4("The user can then select the past number of games played. The past 10 games is selected as the default."),
                                          h4("Lastly, the user can select if the team of interest is home or on the road. Either is selected as the default and includes both home and road stats.")
                                          ) 
                                      
                               )
                               
                             )
                     ),#Closes first tab item 
                     
                     #1st app page
                     tabItem(tabName = "chart", 
                             fluidRow(  
                               
                               #1st page header
                               h2("NFL Teams Recent Over/Under Trends")
                               
                             ),
                             
                             fluidRow(
                               #This will be the left side of the 1st app page (3 of 12 width)
                               column(3,br(), 
                                      
                                      #Input for Team of Interest
                                      selectizeInput("team","Team Name", 
                                                     choices=levels(as.factor(newScoresData$team_home))), 
                                      
                                      #Input Opponent
                                      selectizeInput("opponent","Opponent",selected = "None",
                                                     choices = c("None",levels(as.factor(newScoresData$team_home)))),
                                      
                                      
                                      #Select number of games
                                      radioButtons("gameNumber","Last Number of Games",
                                                   selected = 10, choices = c(10,15,20), inline = TRUE),
                                      
                                      #Select game location
                                      radioButtons("location","Game Location",selected = "Either",
                                                   choices = c("Either","Home","Away"))
                                      
                                      
                                                       
                                      ),
                               
                               #Right side of 1st app page (9 of 12 width)
                               column(9,
                                      
                                      fluidRow(
                                        
                                        column(10
                                      
                                        
                                        ),
                                        column(2,
                                        downloadButton("downloadPlot", "Save Plot")
                                          
                                        )
                                          
                                        ),
                                      
                                      fluidRow(
                                        
                                    
                                        #Show a plot of the distribution
                                        plotOutput("barChart")
                                        
                                      
                                      ),
                                      
                                      fluidRow(
                                        
                                        #1st line of text under plot
                                        textOutput("info")
                                        
                                      ),
                                      
                                      fluidRow( 
                                        
                                        #2nd line of text under plot
                                        textOutput("info2"),
                                        br()
                               ),
                               
                               fluidRow(
                                 
                                 column(9,
                                        
                                #Table title     
                                 uiOutput("tableTitle")
                                 
                                 ),
                                 
                                 column(3,
                                 downloadButton("downloadTable", "Save Table")
                                 
                                 )
                                 
                               ),
                               
                               fluidRow(
                                 
                                 #Table underneath the chart
                                 tableOutput("table")
                               )
                     )
                     
                   )
                   
                 ), #End of first app page
                 
                 tabItem(tabName = "plot",
                         
                         fluidRow(
                           
                           #2nd page header
                           h2("NFL Teams Recent Margin of Cover Trends"),
                           br(),
                           h4("This plot shows how far under/how far over the selected team is from the Vegas predicted game total")
                           
                           
                         ),
                         
                           #This will be the left side of the app (3 of 12 width)
                           column(3,br(), 
                                  
                                  #Input for Team of Interest
                                  selectizeInput("team2","Team Name", 
                                                 choices=levels(as.factor(newScoresData$team_home))), 
                                  
                                  #Input Opponent
                                  selectizeInput("opponent2","Opponent",selected = "None",
                                                 choices = c("None",levels(as.factor(newScoresData$team_home)))),
                                  
                                  
                                  #Select number of games
                                  radioButtons("gameNumber2","Last Number of Games",
                                               selected = 10, choices = c(10,15,20), inline = TRUE),
                                  
                                  #Select game location
                                  radioButtons("location2","Game Location",selected = "Either",
                                               choices = c("Either","Home","Away"))
                                  
                                ),
                           
                           
                           #Right side of app (9 of 12 width)
                           column(9,
                                  
                                  fluidRow(
                                    
                                    column(9
                                           
                                    ),
                                    column(3,
                                           downloadButton("downloadPlot2", "Save Plot")
                                           
                                    )
                                    
                                  ),
                                  
                                  fluidRow(
                                    
                                    #Show a plot of the selected data
                                    plotOutput("scatterPlot")
                                    
                                  ),
                                  
                                  fluidRow(
                                    
                                    #1st line of text under plot
                                    textOutput("info3")
                                    
                                  ),
                                  
                                  fluidRow( 
                                    
                                    #2nd line of text under plot
                                    textOutput("info4"),
                                    br()
                                  ),
                                  
                                  fluidRow(
                                    
                                    column(9,
                                           uiOutput("tableTitle2")
                                           
                                    ),
                                    
                                    column(3,
                                           downloadButton("downloadTable2", "Save Table")
                                           
                                    )
                                    
                                  ),
                                  
                                  fluidRow(
                                    
                                    tableOutput("table2")
                                  )
                           )
                           
                         
                 ), #End of 2nd plot page
                 
                 #Last page of app
                 tabItem(tabName = "data",
                         
                         fluidRow(
                           
                           #Data page header
                           h2("Historical NFL Odds Data From 09-03-2000 to 09-17-2018"),
                           br()
                         ),
                         
                         fluidRow(
                           
                           #Dataset
                           dataTableOutput("data")
                         ),
                         
                         #Link to the full data
                         fluidRow(
                           
                           h4("You can view the entire data set which begins in the 1966 season"),
                           a(href="https://www.kaggle.com/tobycrabtree/nfl-scores-and-betting-data#spreadspoke_scores.csv",target="_blank","Link to Full Data")
                           
                         )
                         
                   
                 ) #End of last page
                      
                         
                         
                 )#Closes all tab items
                 
                 )#Closes dashboard body
              
)#Closes dashboard page


                 
  