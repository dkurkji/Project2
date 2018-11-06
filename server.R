#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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
scoresData <- read_csv("spreadspoke_scores.csv",
                       col_types = cols(
                         over_under_line = col_double(),
                         weather_humidity = col_number()
                       )
)

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


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  
  #Define object for Number of Games inputted
  gameNumber <- reactive({
    n <- input$gameNumber
  })
  
  #Define object for location
  textLocation <- reactive({
    
    if(input$location == "Either"){
      location <- "at either Home or Away"
    }
    
    else if(input$location == "Home"){
      location <- "at Home"
    }
    
    else{
      location <- "on the Road"
    }
  })
  
  
  #Conditional filtered data if No opponent
  defaultData <- reactive({ 
    
    n <- gameNumber()
    
    newScoresData$schedule_date <- as.character(newScoresData$schedule_date)
    
    if(input$location == "Either"){ 
      newData <- newScoresData %>% select(schedule_date,schedule_week,team_home,team_away,over_under_line,over_under,score_home,score_away) %>% 
        filter(team_home == input$team | team_away == input$team) %>% slice(1:n) 
    }
    
    else if(input$location == "Home"){
      newData <- newScoresData %>% select(schedule_date,schedule_week,team_home,team_away,over_under_line,over_under,score_home,score_away) %>% 
        filter(team_home == input$team) %>% slice(1:n) 
    }
    
    else{
      newData <- newScoresData %>% select(schedule_date,schedule_week,team_home,team_away,over_under_line,over_under,score_home,score_away) %>% 
        filter(team_away == input$team) %>% slice(1:n) 
    }
    
  })
  
  #Conditional filtered data if Opponent selected
  opponentData <- reactive({ 
    
    n <- gameNumber()
    
    newScoresData$schedule_date <- as.character(newScoresData$schedule_date)
    
    if(input$location == "Either"){
      newData <- newScoresData %>% select(schedule_date,schedule_week,team_home,team_away,over_under_line,over_under,score_home,score_away) %>% 
        filter((team_home == input$team & team_away == input$opponent) | 
               (team_away == input$team & team_home == input$opponent)) %>% slice(1:n) 
    }
    
    else if(input$location =="Home"){ 
      newData <- newScoresData %>% select(schedule_date,schedule_week,team_home,team_away,over_under_line,over_under,score_home,score_away) %>% 
        filter(team_home == input$team & team_away == input$opponent) %>% slice(1:n) 
    }
    
    else{
      newData <- newScoresData %>% select(schedule_date,schedule_week,team_home,team_away,over_under_line,over_under,score_home,score_away) %>% 
        filter(team_away == input$team & team_home == input$opponent) %>% slice(1:n)  
    }
               
    
  })
  
  #Select appropriate data
  getData <- reactive({
    
    if(input$opponent != "None"){ 
      newData <- opponentData()
    }
    
    else{
      newData <- defaultData()
    }
  })
  
  #Calculate Win Record
  winRecord <- reactive({
    
    newData <- getData()
    
    win <- sum((newData$team_home == input$team) & (newData$score_home > newData$score_away),
                 (newData$team_away == input$team) & (newData$score_home < newData$score_away))
    
  })
  
  #Calculate Loss Record 
  lossRecord <- reactive({
    
    newData <- getData()
    
    loss <- sum((newData$team_home == input$team) & (newData$score_home < newData$score_away),
                   (newData$team_away == input$team) & (newData$score_home > newData$score_away))
    
  })
  
  
  #Create plot based on inputs
  overUnderPlot <- reactive({
    
    newData <- getData() 
    
    newData$over_under <- factor(newData$over_under,levels = c("Over","Under","Push"))
    
    g <- ggplot(newData, aes(over_under,fill=over_under)) 
    
    g + geom_bar() +
      labs(title = paste(input$team, "Over/Under Chart")) + 
      scale_fill_manual(values = c("skyblue","royalblue", "navy"))
  })
    
    #Output plot updated on If Opponent or Not
    output$barChart <- renderPlot({
      
      overUnderPlot()
  })
  
  #Text Output underneath the plot
  output$info <- renderText({
    
    newData <- getData()
    
    location <- textLocation()
    
    if(input$opponent != "None"){
      
      paste("You selected", input$team, "against", input$opponent, location, "over their last",
            input$gameNumber, "games")
    }
    
    else{
      
      paste("You selected the", input$team, location, "over their last",
            input$gameNumber, "games") 
    }
    
    })

  #2nd line of text showing record and over/under record
  output$info2 <- renderText({
    
    newData <- getData()
    
    win <- winRecord()
    
    loss <- lossRecord()
    
    paste("The", input$team, "have a", win, "-", loss, "record and are", sum(newData$over_under == "Over"), 
          "-", sum(newData$over_under == "Under"), "-", sum(newData$over_under == "Push"), 
          "Over/Under in these games")
    })
  
  #Output selected data
  output$table <- renderTable({
    
    getData()
  })
  
  
  #Output table title
  output$tableTitle <- renderUI({
    
    title <- paste(input$team, "Selected Data Table")
    h1(title)
  })
  
  #Download button for the selected data
  output$downloadTable <- downloadHandler(
    
    filename = function(){ 
      paste(input$team, "_Team_Data.csv")
    },
    
    content = function(file) {
      
      write.csv(getData(),file)
      
    })
  
  #Download Button for the selected plot
  output$downloadPlot <- downloadHandler(
    
    filename = function(){
      paste(input$team, "Over/Under_Plot.png")
    },
    
    content = function(file){
      ggsave(file, plot = overUnderPlot(), device = "png")
    }
  )
  
  #___________________________________________________________________________________________________________
  
  #Define object for Number of Games inputted
  gameNumber2 <- reactive({
    n <- input$gameNumber2
  })
  
  
  #Conditional filtered data if No opponent
  defaultData2 <- reactive({ 
    
    n <- gameNumber2()
    
    newScoresData$schedule_date <- as.character(newScoresData$schedule_date)
    
    if(input$location2 == "Either"){ 
      newData <- newScoresData %>% select(schedule_date,schedule_week,team_home,team_away,over_under_line,over_under,score_home,score_away) %>% 
        filter(team_home == input$team2 | team_away == input$team2) %>% slice(1:n) 
    }
    
    else if(input$location2 == "Home"){
      newData <- newScoresData %>% select(schedule_date,schedule_week,team_home,team_away,over_under_line,over_under,score_home,score_away) %>% 
        filter(team_home == input$team2) %>% slice(1:n) 
    }
    
    else{
      newData <- newScoresData %>% select(schedule_date,schedule_week,team_home,team_away,over_under_line,over_under,score_home,score_away) %>% 
        filter(team_away == input$team2) %>% slice(1:n) 
    }
    
  })
  
  #Conditional filtered data if Opponent selected
  opponentData2 <- reactive({ 
    
    n <- gameNumber2()
    
    newScoresData$schedule_date <- as.character(newScoresData$schedule_date)
    
    if(input$location2 == "Either"){
      newData <- newScoresData %>% select(schedule_date,schedule_week,team_home,team_away,over_under_line,over_under,score_home,score_away) %>% 
        filter((team_home == input$team2 & team_away == input$opponent2) | 
                 (team_away == input$team2 & team_home == input$opponent2)) %>% slice(1:n) 
    }
    
    else if(input$location2 =="Home"){ 
      newData <- newScoresData %>% select(schedule_date,schedule_week,team_home,team_away,over_under_line,over_under,score_home,score_away) %>% 
        filter(team_home == input$team2 & team_away == input$opponent2) %>% slice(1:n) 
    }
    
    else{
      newData <- newScoresData %>% select(schedule_date,schedule_week,team_home,team_away,over_under_line,over_under,score_home,score_away) %>% 
        filter(team_away == input$team2 & team_home == input$opponent2) %>% slice(1:n)  
    }
    
    
  })
  
  #Select appropriate data
  getData2 <- reactive({
    
    if(input$opponent2 != "None"){ 
      newData <- opponentData2()
    }
    
    else{
      newData <- defaultData2()
    }
  })
  
  #Create cover margin statistic
  coverMargin <- reactive({
    
    newData <- getData2() 
    
    newData <- newData %>% mutate(coverMargin = abs(newData$over_under_line - 
                                                         (newData$score_home + newData$score_away)))
    
  })
  
  #Create scatterplot of cover margin
  coverMarginPlot <- reactive({
    
    newData <- coverMargin()
    
    sp <- ggplot(newData,aes(x=over_under_line,y=coverMargin))
    
    sp + geom_point(size = 3, aes(col=over_under)) + 
      labs(title = paste(input$team2, "Cover Margin Plot"), x = "Over/Under Line", y = "Cover Margin")
    
  })
  
  #Output scatterplot
  output$scatterPlot <- renderPlot({
    
    coverMarginPlot()
    
  })
  
  output$info3 <- renderText({
    
    newData <- coverMargin()
    
    paste("The", input$team2, "have an average cover margin of", round(mean(newData$coverMargin),2),"
          for the selected games")
  })
  
  #Output selected data
  output$table2 <- renderTable({
    
    coverMargin()
  })
  
  
  #Output table title
  output$tableTitle2 <- renderUI({
    
    title2 <- paste(input$team2, "Selected Data Table")
    h1(title2)
  })
  
  #Download Button for the selected plot
  output$downloadPlot2 <- downloadHandler(
    
    filename = function(){
      paste(input$team2, "Cover_Margin_Plot.png")
    },
    
    content = function(file){
      ggsave(file, plot = coverMarginPlot(), device = "png")
    }
  )
  
  #Download button for the selected data
  output$downloadTable2 <- downloadHandler(
    
    filename = function(){ 
      paste(input$team2, "_Team_Data.csv")
    },
    
    content = function(file) {
      
      write.csv(coverMargin(),file)
      
    })
  
  
  #Output last page of data set
  output$data <- renderDataTable({
    newScoresData
  })
    
    })

