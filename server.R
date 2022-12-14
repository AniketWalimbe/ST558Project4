#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Data Tab

library(shiny)
library(tidyverse)
library(DT)
library(shinydashboard)
library(ggplot2)
library(caret)

source("helper.R")

shinyServer(function(session, input, output){
  
  fetchData <- reactive({
    filterdata <- getData()
    filterdata <- filterdata %>% 
      filter(male %in% input$sex) %>% 
      filter(age >= input$ageslider) %>%
      filter(currentSmoker >= input$currentSmoker) %>%
      filter(totChol >= input$cholestrol) %>%
      filter(glucose >= input$glucose)
    
    
    
    
    filterdata
  })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable({
    data <- fetchData()
    DT::datatable(data,
                  options = list(autoWidth = TRUE))
    
  })
  
  
  
  
  
  plotdata <- getData()
  
  output$diabplot <- renderPlot({
    
    #da <- plotdata %>% group_by_(input$plotinput1, input$plotinput2) %>% summarize(abc = n())
    
    
    
    
    if(input$plotType1 == "Scatter"){
      plot <- ggplot(data = plotdata, aes_string(x = input$plotinput1, y = input$plotinput2))
      plot <- plot + geom_point(size = 2)
    }else if(input$plotType1 == "Box"){
      plot <- ggplot(data = plotdata, aes_string(x = input$plotinput1, y = input$plotinput2))
      plot <- plot  + geom_jitter()+ geom_boxplot()
    }
    else{
      plotdatabar <- plotdata %>% group_by_("diabetes", input$groupby) %>% summarize(numberof = n())
      plot <- ggplot(data = plotdatabar, aes_string(x = "diabetes", y="numberof"))
      plot <- plot + geom_bar(stat = "identity", aes_string(fill =input$groupby), position="dodge")
    }
    
    plot
  })
  
  output$summ1 <- renderText({
    #get filtered data
    newData <- getData()
    newDataD1 <- newData %>% filter(diabetes ==1)
    
    out = ""
    
    if(input$stats1 == "age"){
      out =paste0("The average Age for people with diabetes is: ", round(mean(newDataD1$age),2))
    }
    if(input$stats1 == "heartRate"){
      out =  paste0("The average Heart Rate for people with diabetes is: ", round(mean(newDataD1$heartRate),2))
    }
    if(input$stats1 == "glucose"){
      out = paste0("The average Glucose level for people with diabetes is: ", round(mean(newDataD1$glucose),2))
    }
    if(input$stats1 == "totChol"){
      out =paste0("The average Cholestrol for people with diabetes is: ", round(mean(newDataD1$totChol),2))
    }
    if(input$stats1 == "BMI"){
      out =paste0("The average BMI for people with diabetes is: ", round(mean(newDataD1$BMI),2))
    }
    if(input$stats1 == "sysBP"){
      out =paste0("The average Systolic Blood Pressure for people with diabetes is: ", round(mean(newDataD1$sysBP),2))
    }
    if(input$stats1 == "diaBP"){
      out = paste0("The average Diabolic Blood Pressure for people with diabetes is: ", round(mean(newDataD1$diaBP),2))
    }
    if(input$stats1 == "cigsPerDay"){
      out = paste0("The average Cigarette consumed per day for people with diabetes is: ", round(mean(newDataD1$cigsPerDay),2))
    }
    out
  })
  
  
  
  
  output$GLMModel <- renderPrint({
    "GLM Model Summary"
  })
  
  output$CTModel <- renderPrint({
    "Classification Tree Model Summary"
  })
  
  output$RFModel <- renderPrint({
    "Random Forest Model Summary"
  })
  
  
  output$GLMTest <- renderPrint({
    "GLM Model Test Statistics"
  })
  
  output$CTTest <- renderPrint({
    "Classification Tree Test Statistics"
  })
  
  output$RFTest <- renderPrint({
    "Random Forest Test Statistics"
  })
  
  #################################################################################withProgress(message = 'Models Running', value = 0, {
  observeEvent(input$runmodel, {
    
    getDataForGlm <- getData()
    
    getDataForGlm <- drop_na(getDataForGlm)
    
    newdfForGlm <- getDataForGlm %>% select(diabetes)
    newdfForGlm$diabetes <- as_factor(newdfForGlm$diabetes)
    
    if (input$glmmale){
      newdfForGlm$male <- as_factor(getDataForGlm$male)
    }
    
    if (input$glmage){
      newdfForGlm$age <- getDataForGlm$age
    }
    
    if (input$glmcurrentSmoker){
      newdfForGlm$currentSmoker <- as_factor(getDataForGlm$currentSmoker)
    }
    
    if (input$glmBMI){
      newdfForGlm$BMI <- getDataForGlm$BMI
    }
    
    if (input$glmcigsPerDay){
      newdfForGlm$cigsPerDay <- getDataForGlm$cigsPerDay
    }
    
    if (input$glmheartRate){
      newdfForGlm$heartRate <- getDataForGlm$heartRate
    }
    
    if (input$glmsysBP){
      newdfForGlm$sysBP <- getDataForGlm$sysBP
    }
    
    if (input$glmdiaBP){
      newdfForGlm$diaBP <- getDataForGlm$diaBP
    }
    
    if (input$glmprevalentStroke){
      newdfForGlm$prevalentStroke <- as_factor(getDataForGlm$prevalentStroke)
    }
    
    if (input$glmtotChol){
      newdfForGlm$totChol <- getDataForGlm$totChol
    }
    
    if (input$glmglucose){
      newdfForGlm$glucose <- getDataForGlm$glucose
    }
    
    if (input$glmBPMeds){
      newdfForGlm$BPMeds <- as_factor(getDataForGlm$BPMeds)
    }
    
    
    
    set.seed(100)
    trainindex <- createDataPartition(newdfForGlm$diabetes, p = input$traintest, list=FALSE)
    trainglm <- newdfForGlm[trainindex,]
    testglm <- newdfForGlm[-trainindex,]
    
    glm <- train(diabetes~., data = trainglm,
                 trControl = trainControl(method = "cv", number = input$cvglm),
                 preProcess = c("center", "scale"),
                 method = "glmnet",
                 family = "binomial"
    )
    
    glmcf <- confusionMatrix(data = predict(glm, newdata = testglm),testglm$diabetes)
    
    
    
    output$GLMModel <- renderPrint({
      glm
    })
    
    output$GLMTest <- renderPrint({
      glmcf
    })
    
    
    
    getDataForct <- getData()
    
    getDataForct <- drop_na(getDataForct)
    
    newdfForct <- getDataForct %>% select(diabetes)
    newdfForct$diabetes <- as_factor(newdfForct$diabetes)
    
    if (input$ctmale){
      newdfForct$male <- as_factor(getDataForct$male)
    }
    
    if (input$ctage){
      newdfForct$age <- getDataForct$age
    }
    
    if (input$ctcurrentSmoker){
      newdfForct$currentSmoker <- as_factor(getDataForct$currentSmoker)
    }
    
    if (input$ctBMI){
      newdfForct$BMI <- getDataForct$BMI
    }
    
    if (input$ctcigsPerDay){
      newdfForct$cigsPerDay <- getDataForct$cigsPerDay
    }
    
    if (input$ctheartRate){
      newdfForct$heartRate <- getDataForct$heartRate
    }
    
    if (input$ctsysBP){
      newdfForct$sysBP <- getDataForct$sysBP
    }
    
    if (input$ctdiaBP){
      newdfForct$diaBP <- getDataForct$diaBP
    }
    
    if (input$ctprevalentStroke){
      newdfForct$prevalentStroke <- as_factor(getDataForct$prevalentStroke)
    }
    
    if (input$cttotChol){
      newdfForct$totChol <- getDataForct$totChol
    }
    
    if (input$ctglucose){
      newdfForct$glucose <- getDataForct$glucose
    }
    
    if (input$ctBPMeds){
      newdfForct$BPMeds <- as_factor(getDataForct$BPMeds)
    }
    
    
    
    set.seed(100)
    trainindex <- createDataPartition(newdfForct$diabetes, p = input$traintest, list=FALSE)
    trainct <- newdfForct[trainindex,]
    testct <- newdfForct[-trainindex,]
    
    ct <- train(diabetes~., data = trainct,
                trControl = trainControl(method = "cv", number = input$cvct),
                preProcess = c("center", "scale"),
                method = "rpart",
                tuneGrid = expand.grid(cp = seq(0.1, 0.2, by=0.001))
    )
    
    ctcf <- confusionMatrix(data = predict(ct , newdata = testct), testct$diabetes)
    
    
    
    
    
    output$CTModel <- renderPrint({
      ct
    })
    
    output$CTTest <- renderPrint({
      ctcf
    })
    
    
    getDataForrf <- getData()
    
    getDataForrf <- drop_na(getDataForrf)
    
    newdfForrf <- getDataForrf %>% select(diabetes)
    newdfForrf$diabetes <- as_factor(newdfForrf$diabetes)
    
    if (input$rfmale){
      newdfForrf$male <- as_factor(getDataForrf$male)
    }
    
    if (input$rfage){
      newdfForrf$age <- getDataForrf$age
    }
    
    if (input$rfcurrentSmoker){
      newdfForrf$currentSmoker <- as_factor(getDataForrf$currentSmoker)
    }
    
    if (input$rfBMI){
      newdfForrf$BMI <- getDataForrf$BMI
    }
    
    if (input$rfcigsPerDay){
      newdfForrf$cigsPerDay <- getDataForrf$cigsPerDay
    }
    
    if (input$rfheartRate){
      newdfForrf$heartRate <- getDataForrf$heartRate
    }
    
    if (input$rfsysBP){
      newdfForrf$sysBP <- getDataForrf$sysBP
    }
    
    if (input$rfdiaBP){
      newdfForrf$diaBP <- getDataForrf$diaBP
    }
    
    if (input$rfprevalentStroke){
      newdfForrf$prevalentStroke <- as_factor(getDataForrf$prevalentStroke)
    }
    
    if (input$rftotChol){
      newdfForrf$totChol <- getDataForrf$totChol
    }
    
    if (input$rfglucose){
      newdfForrf$glucose <- getDataForrf$glucose
    }
    
    if (input$rfBPMeds){
      newdfForrf$BPMeds <- as_factor(getDataForrf$BPMeds)
    }
    
    
    
    set.seed(100)
    trainindex <- createDataPartition(newdfForrf$diabetes, p = input$traintest, list=FALSE)
    trainrf <- newdfForrf[trainindex,]
    testrf <- newdfForrf[-trainindex,]
    
    rf <- train(diabetes~., data = trainrf,
                trControl = trainControl(method = "cv", number = input$cvrf),
                preProcess = c("center", "scale"),
                method = "rf",
                tuneGrid = expand.grid(mtry = input$mtryrf)
    )
    
    rfcf <- confusionMatrix(data = predict(rf , newdata = testrf),testrf$diabetes)
    
    
    
    
    
    output$RFModel <- renderPrint({
      rf
    })
    
    output$RFTest <- renderPrint({
      rfcf
    })
    
    
    ###########################################
    
    if(input$modelsel == "Generalized Linear Regression"){
      glmPreddf <- data.frame(diabetes = c(0))
      glmPreddf <- glmPreddf %>% select(diabetes)
      
      glmPreddf$male <- c(input$predglmmale)
      glmPreddf$age <- c(input$predglmage)
      glmPreddf$BMI <- c(input$predglmBMI)
      glmPreddf$currentSmoker <- c(input$predglmcurrentSmoker)
      glmPreddf$cigsPerDay <- c(input$predglmcigsPerDay)
      glmPreddf$heartRate <- c(input$predglmheartRate)
      glmPreddf$sysBP <- c(input$predglmsysBP)
      glmPreddf$diaBP <- c(input$predglmdiaBP)
      glmPreddf$totChol <- c(input$predglmtotchol)
      glmPreddf$prevalentStroke <- c(input$predglmprevalentStroke)
      glmPreddf$glucose <- c(input$predglmglucose)
      glmPreddf$BPMeds <- c(input$predglmBPMeds)
      
      glmPreddf$male <- as_factor(glmPreddf$male)
      glmPreddf$BPMeds <- as_factor(glmPreddf$BPMeds)
      
      glmPreddf$prevalentStroke <- as_factor(glmPreddf$prevalentStroke)
      glmPreddf$currentSmoker <- as_factor(glmPreddf$currentSmoker)
      
      glmprediction <- predict(glm, newdata = glmPreddf)
      
      ##change lmgpred to ctpred rfpred
      output$glmpred <- renderText({
        paste0("The predicted diabetes using ", input$modelsel," model is: ", glmprediction)
      })
    }else if(input$modelsel == "Classification Tree"){
      
    }else{
      
    }
    
    ############################################
    
    
    output$RFPlot <- renderPlot({
      plot(varImp(rf))
    })
    
  })
  #############################################################################})
  
  
  
})

#     if (input$man != "All") {
#       data <- read_data[read_data$manufacturer == input$man,]
#     }
#     if (input$cyl != "All") {
#       data <- read_data[read_data$cyl == input$cyl,]
#     }
#     if (input$trans != "All") {
#       data <- read_data[read_data$trans == input$trans,]
#     }
#     data
#   }
# }




# function(input, output) {
#   
#   # display 10 rows initially
#   output$ex1 <- DT::renderDataTable(
#     DT::datatable(read_data, options = list(pageLength = 25))
#   )
# } 

# # Define server logic required to draw a histogram
# shinyServer(function(input, output) {
# 
#     output$distPlot <- renderPlot({
# 
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
# 
#     })
# 
# })
