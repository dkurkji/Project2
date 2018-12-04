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
library(class)
library(dataPreparation)
library(plotrix)


#Read in the Scores Data Set
scoresData <- read_csv("spreadspoke_scores.csv",
                       col_types = cols(
                         over_under_line = col_double(),
                         weather_humidity = col_number()
                         )
                       )

#Switch to a Date format
newScoresData <- scoresData %>% mutate(schedule_date = as.Date(schedule_date,"%m/%d/%Y"))

#Filter the data and create a new over_under variable
newScoresData <- filter(newScoresData,schedule_date >= "2000-09-03" & schedule_date <= "2018-09-01") %>% 
                 arrange(desc(schedule_date)) %>% mutate(team_home = replace(team_home,team_home == "San Diego Chargers",
                                                                             "Los Angeles Chargers")) %>% 
  mutate(team_away = replace(team_away,team_away == "San Diego Chargers","Los Angeles Chargers")) %>%
  mutate(team_home = replace(team_home,team_home == "St. Louis Rams","Los Angeles Rams")) %>% 
  mutate(team_away = replace(team_away,team_away == "St. Louis Rams","Los Angeles Rams")) %>% 
  mutate(over_under = case_when(over_under_line < score_home + score_away ~ "Over",
                                               over_under_line > score_home + score_away ~ "Under",
                                               TRUE ~ "Push"))


#Create a filtered data set for the logistic regression model
glmScoresData <- filter(newScoresData,(over_under != "Push") & (schedule_playoff == "FALSE"))

glmScoresData <- select(glmScoresData,spread_favorite:weather_wind_mph,over_under) %>% 
  mutate(weather_detail = replace_na(weather_detail,"Neutral")) %>%
  mutate(weather_detail = replace(weather_detail,weather_detail == "Fog" | weather_detail == "Rain"
                                  | weather_detail == "Rain | Fog" | weather_detail == "Snow" | 
                                    weather_detail == "Snow | Fog" 
                                  | weather_detail == "Snow | Freezing Rain","Precipitation")) %>% 
  mutate(num_over_under = case_when(over_under == "Over" ~ 1, TRUE ~ 0))

#Use the absolute values of the spreads
glmScoresData$spread_favorite <- abs(glmScoresData$spread_favorite)


#Subset Data for kNN
knnData <- select(glmScoresData, spread_favorite:over_under, - weather_detail)

#Set Over/Under as factor
knnData$over_under <- as.factor(knnData$over_under)

knnData$weather_temperature <- as.numeric(knnData$weather_temperature)
knnData$weather_wind_mph <- as.numeric(knnData$weather_wind_mph)

#get training and test sets 
set.seed(538) 

train <- sample(1:nrow(knnData), size = nrow(knnData)*0.6) 
test <- dplyr::setdiff(1:nrow(knnData), train)

knnDataTrain <- select(knnData[train, ], spread_favorite:weather_wind_mph) 
knnDataTest <- select(knnData[test, ], spread_favorite:weather_wind_mph)

#Standardize training and tests sets
means <- colMeans(knnDataTrain) 
sds <- matrixStats::colSds(as.matrix(knnDataTrain))

knnDataTrainStd <- apply(knnDataTrain, MARGIN = 2, function(x){(x-mean(x))/sd(x)}) 

knnDataTestStd <- as.data.frame(lapply(1:ncol(knnDataTest), 
                                       FUN = function(w, x, y, z){(x[,w]-y[w])/z[w]}, 
                                       x = knnDataTest, y = means, z = sds))

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
                   menuItem("Over/Under Prediction", tabName = "glm", icon = icon("line-chart")),
                   menuItem("KNN Predictors",tabName = "knn", icon = icon("area-chart")),
                   menuItem("Clustering",tabName = "clust",icon = icon("pie-chart")),
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
                                 
                                 column(9,
                                 #Table underneath the chart
                                 tableOutput("table")
                                 ),
                                 column(3
                                        )
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
                 
                 #Start of glm page
                 tabItem(tabName = "glm",
                         
                         fluidRow(
                           
                           #Header
                           h2("Predict Over/Under Probabilities"),
                           br(),
                           #Info about page
                           h4("This page plots a GLM fit using logistic regression. Choose either one or two
                              predictors and view the corresponding plot and summary on the left hand side.
                              You can also select individual values which will give an over/under probability
                              underneath the plot."),
                           br()
                         ),
                         
                         #Variable selection
                         fluidRow(
                           
                           column(4,
                                  
                                  box(width = 6,
                           selectizeInput("varSelect1","Select 1st Predictor",
                                          names(glmScoresData[1:5]),selected = "spread_favorite"),
                           
                           selectizeInput("varSelect2","Select 2nd Predictor",
                                          choices = c(" ", names(glmScoresData[1:5])),
                                                    selected = NULL  )
                           
                                  )
                           ),
                           column(8,
                                  
                                  box(width = 12,
                                  #Prediction selection based on variable input
                                  conditionalPanel(condition = "input.varSelect1 == 'spread_favorite' ||
                                                   input.varSelect2 == 'spread_favorite'",
                                                   sliderInput("spread","Select Spread",
                                                               min=0,max=25,value=4.5,step=.5)),
                                  
                                  conditionalPanel(condition = "input.varSelect1 == 'over_under_line' ||
                                                   input.varSelect2 == 'over_under_line'",
                                                   sliderInput("total","Select Total",
                                                               min=30,max=63,value=45,step=.5)),
                                  
                                  conditionalPanel(condition = "input.varSelect1 == 'weather_detail' ||
                                                   input.varSelect2 == 'weather_detail'",
                                                   selectInput("weather","Select Weather Detail",
                                                               choices = levels(as.factor(glmScoresData$weather_detail)),
                                                               selected = "Neutral")),
                                  
                                  conditionalPanel(condition = "input.varSelect1 == 'weather_temperature' ||
                                                   input.varSelect2 == 'weather_temperature'",
                                                   sliderInput("temp","Select Temperature",
                                                               min=0,max=95,value=65,step=1)),
                                  
                                  conditionalPanel(condition = "input.varSelect1 == 'weather_wind_mph' ||
                                                   input.varSelect2 == 'weather_wind_mph'",
                                                   sliderInput("wind","Select Wind Speed",
                                                               min=0,max=25,value=6,step=1))
                                  
                                  
                                  )
                                  )
                         
                         
                         
                         ),
                         
                         fluidRow(
                           
                                  box(verbatimTextOutput("glmModel"),width = 6),
                                  
                                  box(plotOutput("glmPlot"),width = 6),
                         
                         br(),
                         textOutput("prediction")
                 )
                 ),
                 
                 #Start of KNN tab
                 tabItem(tabName = "knn",
                         
                         fluidRow(
                           
                           #Header
                           h2("K-Nearest Neighbors"),
                           br(),
                           h4("This page predicts the class variable Over/Under by using the k nearest 
                              neighbors of the given predictors. Select two predictors and the desired value 
                              for k. If checkbox is selected you can click on a point to view the corresponding 
                              neighborhood. Underneath the plot shows the corresponding missclassification rate."),
                           br()
                           
                         ),
                           
                           column(3,
                                  
                                  selectizeInput("knnVar1","Select 1st Predictor",
                                                 names(select(glmScoresData,spread_favorite,over_under_line,
                                                              weather_temperature,weather_wind_mph)),
                                                       selected = "spread_favorite"),
                                  
                                  selectizeInput("knnVar2","Select 2nd Predictor",
                                                 names(select(glmScoresData,spread_favorite,over_under_line,
                                                              weather_temperature,weather_wind_mph)),
                                                       selected = "over_under_line"),
                                  
                                  sliderInput("knn", "Select the Number of K Nearest Neighbors",
                                              value = 50, min = 1,max = 500),
                                  
                                  checkboxInput("knnPoint","Show the Neighborhood for One Point (click to select a point)")
                                  ),
                           
                           column(9,
                             
                            plotOutput("knnPlot",width = "600px", height = "600px",click = "click_plot"),
                           br(),
                           textOutput("missInfo")
                           )
                         
                         ),
                 
                 tabItem(tabName = "clust",
                         
                         fluidRow(
                           
                           #Header
                           h2("Hierarchical Clustering"),
                           br(),
                           h4("This page uses hierarchical clustering to divide the selected variables. Choose 
                              your desired variables and the number of clusters used for group membership. There 
                              is also an option for the method on how the observations are joined together."),
                           br()
                           
                         ),
                         
                         column(3,
                                
                                selectizeInput("clustVar1","Select 1st Variable",
                                               names(select(glmScoresData,spread_favorite,over_under_line,
                                                            weather_temperature,weather_wind_mph)),
                                               selected = "spread_favorite"),
                                
                                selectizeInput("clustVar2","Select 2nd Variable",
                                               names(select(glmScoresData,spread_favorite,over_under_line,
                                                            weather_temperature,weather_wind_mph)),
                                               selected = "over_under_line"),
                                
                                selectizeInput("clustMethod","Select Method for Linkage",
                                               choices = c("Complete","Average")),
                                
                                sliderInput("clusters", "Select the Number Clusters",
                                            value = 8, min = 1,max = 15)
                         ),
                         
                         column(9,
                                
                                plotOutput("clustPlot")
                         )
                 ),
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


                 
  