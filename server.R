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
  
  knnDataTrain <-knnData[train, ]
  knnDataTest <- knnData[test, ]
  
  #Standardize training and tests sets
  scales <- build_scales(knnDataTrain,cols = c("spread_favorite","over_under_line","weather_temperature",
                                               "weather_wind_mph"),verbose = FALSE)
  
  knnDataTrainStd <- fastScale(knnDataTrain,scales = scales,verbose = FALSE)
  
  knnDataTestStd <- fastScale(knnDataTest,scales = scales,verbose = FALSE)

# Define server logic
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
    
  
#_______________________________________________________________________________________________________

#Grab data used for modeling purposes  
getGlmData <- reactive({
  
  glmData <- glmScoresData
  
})

#Fit the model based on variable inputs
glmFit <- reactive({
  
  glmData<-getGlmData()
  
  if (input$varSelect2 == " " | input$varSelect2 == input$varSelect1){
  
    glmFit <- glm(paste0("num_over_under~",input$varSelect1),data = glmData,family = "binomial")
  }
  
  else{ 
   
  glmFit <-glm(paste0("num_over_under~", input$varSelect1,"*",input$varSelect2),
               data = glmData,family = "binomial")
  }
  
})

#Ouput summary of fitted model
output$glmModel<-renderPrint({
  
  glmFit <- glmFit()
  
  summary(glmFit)
  
})

#Output plot of fitted model
output$glmPlot <- renderPlot({
  
  glmFit <- glmFit()
  
  ggiraphExtra::ggPredict(glmFit)
  
})

#Run prediction based on inputted values
glmPredict <- reactive({
  
  glmData <- getGlmData()
  
  glmFit <- glmFit()
    
    glmPredict <- predict(glmFit,newdata = data.frame(spread_favorite = input$spread,
                                                      over_under_line = input$total,
                                                      weather_detail = input$weather,
                                                      weather_temperature = input$temp,
                                                      weather_wind_mph = input$wind),type = "response")
})

#Output prediction
output$prediction <- renderText({
  
paste("The Estimated Over/Under Probability with the chosen values is ", round(glmPredict(),4))

})

#___________________________________________________________________________________________________________

#Create Subset of Training set
knnDataTrainSub <- as.matrix(sapply(knnDataTrainStd, as.numeric))
  
knnTrainSub <- reactive(knnDataTrainSub[,c(input$knnVar1,input$knnVar2)])

knnY <- knnDataTrainSub[,5]

#Obtain sequence of first predictor 
getPX1 <- reactive({
  
  if(input$knnVar1 == "spread_favorite"){
    
    px1 <- seq(-1.6,5.6,.1)
  }
  
  else if(input$knnVar1 == "over_under_line"){
    
    px1 <- seq(-2.7,4.2,.1)
  }
  
  else if(input$knnVar1 == "weather_temperature"){
    
    px1 <- seq(-3.8,2.2,.1)
  }
  
  else{
    
    px1 <- seq(-1.2,4.8,.1)
  }
  
})

#Obtain sequence of 2nd predictor
getPX2 <- reactive({
  
  if(input$knnVar2 == "spread_favorite"){
    
    px2 <- seq(-1.6,5.6,.05)
  }
  
  else if(input$knnVar2 == "over_under_line"){
    
    px2 <- seq(-2.7,4.2,.05)
  }
  
  else if(input$knnVar2 == "weather_temperature"){
    
    px2 <- seq(-3.8,2.2,.05)
  }
  
  else{
    
    px2 <- seq(-1.2,4.8,.05)
  }
})

#Expand grid on predictors 
knnTestSub <- reactive(expand.grid(getPX1(),getPX2()))

idx  <- NULL

dmat <- NULL

## ID the point clicked on 

xy  <- reactive(c(input$click_plot$x, input$click_plot$y))

id <- observe({
  
  if (!is.null(xy())) {
    
    dmat <- as.matrix(dist(rbind(xy(), knnTrainSub())))
    
    idx <<- which.min(dmat[1, -1])
    
    dmat <<- dmat[-1, -1]
    
  }
})

#Output knn plot
output$knnPlot <- renderPlot({
  
  xy()
  
  validate(
    need(input$knnVar1 != input$knnVar2,"Please select distinct predictors")
  )
  
  #Fit kNN Model
  knnFit <- knn(train = knnTrainSub(),test = knnTestSub(),cl = knnY, k = input$knn)
  
  probs <- matrix(knnFit, length(getPX1()), length(getPX2()))
  
  ## Plot create empty plot
  plot(knnTrainSub(), asp = 1, type = "n", xlab = input$knnVar1, ylab = input$knnVar2, 
       xlim = range(getPX1()), ylim = range(getPX2()), 
       main =  paste0(input$knn,"-Nearest Neighbors"))
  

  ## Get neighbourhood, draw circle, if needed
  if (input$knnPoint & !is.null(idx)) {
    
   rad <- sort(dmat[, idx])[1 + input$knn]
    
  draw.circle(x = knnTrainSub()[idx, 1], y = knnTrainSub()[idx, 2], radius= rad, col = "lightgoldenrod1")
    
 }
  
  ## Plot the grid
 grid <- expand.grid(x = getPX1(), y = getPX2())
  
 points(grid, pch = 20, cex = 0.2, col = ifelse(probs > 1.5, "coral", "cornflowerblue"))
  
 points(knnTrainSub(), col = ifelse(knnY == 2, "coral", "cornflowerblue"), 
       cex = 1.5, pch = 21, lwd = 2)
  
  
  ## Add decision boundary
  contour(getPX1(),getPX2(), probs, levels = 1.5, labels = "", lwd = 1.5, add = TRUE)
  

  ## ID points within neighborhood
  if (input$knnPoint & !is.null(idx)) {
    
    points(knnTrainSub()[which(dmat[, idx] <= rad), ], col = "red", pch = 20, cex = 0.75)
    
    points(knnTrainSub()[idx, , drop = FALSE], pch = 3, cex = 1.5, lwd = 2)
    
 }
  
})

#Output missclassification rate  
output$missInfo <- renderText({
  
  validate(
    need(input$knnVar1 != input$knnVar2,"Please select distinct predictors")
  )
  
  pred <- ifelse(knn(knnTrainSub(),knnTrainSub(),cl = knnY,k = input$knn)=="1", 1, 2)

  missRate <- 1 - sum(pred==knnY)/length(knnY)
  
  paste0("The Missclassification Rate for ",input$knnVar1," and ",input$knnVar2," when k = ",input$knn," is ",
         round(missRate,4))
})
#_____________________________________________________________________________________________________________

#Create Hierarchical Clustering based on inputted linkage

getClustPlot <- reactive({
  
glmData <- getGlmData()

validate(
  need(input$clustVar1 != input$clustVar2,"Please select distinct variables")
)
  
hierClust <- hclust(dist(select(glmData, spread_favorite,over_under_line,weather_temperature,
                                           weather_wind_mph)), method = tolower(input$clustMethod))
  
memberships <- as.factor(cutree(hierClust, k = input$clusters))

ggplot(cbind(glmData, memberships), aes_string(x = input$clustVar1, y = input$clustVar2, color = memberships)) + 
  geom_point() + 
  labs(col = "Cluster Membership")

})

#Output cluster plot
output$clustPlot <- renderPlot({

getClustPlot()

})
})
