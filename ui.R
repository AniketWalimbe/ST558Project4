#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(tidyverse)
library(DT)
library(shinydashboard)
library(ggplot2)
library(caret)

library(shinycssloaders)

source("helper.R")


sidebar <- dashboardSidebar(
  #width = 200,
  hr(),
  sidebarMenu(id = "aa",
              
              menuItem("About", tabName="about", icon = icon("question-circle")),
              
              menuItem("Data", tabName = "dataset", icon = icon("table")),
              
              menuItem("Data Exploration", tabName = "eda", icon=icon("line-chart")),
              
              menuItem("Modelling", tabName = "modeling", icon = icon("circle-info"))
              
              
              
  )
)

## Data Tab
## About the project

body <- dashboardBody(
  #tags$head(
  #tags$script("document.title = 'Framhingham Heart Disease Data set'"),
  tabItems(tabItem(tabName = "about",
             fluidPage(
               column(align = "center", 12, span(tags$img(src = "diabetes.jpg", width = "50%"))),
               fluidRow(column(align = "center", 12, h3(strong("Framhingham Heart Disease")))),
               fluidRow(h4("The main purpose of the app is to predict the effect of various different factors like age, sex, glucose level, smoking, cholesterol and many others on diabetes.
               This app will help to determine what levels of different factors can be dangerous and a user can monitor his/her reports based on the reference level provided.")),
               fluidRow(h3(strong("The Data :"))),
               fluidRow(h4("The dataset was collected from", a("this link.", href = "https://www.kaggle.com/datasets/naveengowda16/logistic-regression-heart-disease-prediction") , "From this data, I have selected diabetes as the response variable and all the other factors as predictors."),
                        h4("Male(Categorical) :  Whether the sex of the person is male or female"),
                        h4("Age(Numeric) : Age of the person"),
                        h4("Education (Categorical) : The level of education of the person"),
                        h4("currentSmoker(Categorical) : Does the person smoke"),
                        h4("cigsperDay( Numeric) : how many cigarettes per day"),
                        h4("BPMeds (Categorical) : If the person takes BP medication"),
                        h4("prevalentStroke (Categorical) : If the person had a previous heart stroke"),
                        h4("totChol (Numeric) : total cholesterol reading of the person."),
                        h4("sysBP (Numeric) :"),
                        h4("diaBP (Numeric) :"),
                        h4("BMI (Numeric) : BMI of the person"),
                        h4("heartRate : heart rate of the person"), 
                        h4("glucose : glucose level of the person")),
               
               fluidRow(h3(strong(" The app consists of 4 tabs: "))),
                        h4(strong("1. The Introduction Tab")),
                        h4("This tab provides information about the app, it's purpose, data used and provides brief information about all the tabs present in the app."),
                        h4(strong("2. The Data Tab")),
                        h4("This tab shows the user the data used and allows them to apply various filters based on any column of choice.
                           It also provides an option to download the filtered data and saves it at a csv file. The filters are given for columns
                           namely age, sex, smoking habbits ,cholestrol level and glucose level."),
                        h4(strong("3. Exploratory Data Analysis Tab")),
                        h4("This tab performs exploratory data analysis on the data. It gives the user, options to select the variables they want to find the relation between 
                           and also gives the option of type of plot they want to look at. This tab also shows the statistics for the average value of predictors at which a person has diabetes."),
                        h4(strong("4. Modelling Tab")),
                        h4("This tab consists of 3 sub tabs namely :"),
                        h4("a. Modelling info :"),
                        h4("In this tab, information about 3 models fitted in the app, which are, Generalized Linear Model, Classification Tree,
                           and Random Forest is given. It consists of their advantages and disavdantages as well."),
                        h4("b' Model Fitting :"),
                        h4("This tab allows the user to split the data into testing and training set and also allows the selection of variables
                           that should be used for fitting all the three models. Once the variables are selected, the Build All button fits all the
                           models and shows the summaries. "),
                        h4("c. Prediction :"),
                        h4("This tab allows the user to select the values of variables at which whether a person has diabetes or not need to be predicted.
                           The prediction is the displayed.")
                        
                        
               
             )
    
  ),
    #   tabItem(tabName = "about" ,
    #           fluidPage(
    #             fluidRow(
    #             )
    #           ))
    
    
    
    ### DataSet creation
    tabItem(tabName = "dataset",
            fluidPage(
              fluidRow(
                title = h4("Framhingham Heart Disease Data set"),
                dataTableOutput("table")
              ),
              
              
              br(),
              
              fluidRow(
                column(6, 
                       box(title = "Filter",
                           selectizeInput("sex", "Select Sex", choices = unique(getData()$male), multiple = TRUE, selected = unique(getData()$male) ),
                           
                           sliderInput("ageslider", "Filter by Age", step = 5, min = min(getData()$age), max = max(getData()$age), value = min(getData()$age)),
                           
                           selectizeInput("currentSmoker", "Check by smoking habbits", choices = unique(getData()$currentSmoker), multiple = TRUE, selected = unique(getData()$currentSmoker)),
                           
                           width = 1500
                           
                       )),
                
                column(6,
                       box(title = "Filter on health conditions",
                           sliderInput("cholestrol", "Filter by cholestrol", step = 5, min = 1, max = 400, value = 10),
                           
                           sliderInput("glucose", "Filter by glucose",step = 5, min = 1, max = 120, value = 10),
                           
                           width = 12
                       ),
                       fluidRow(
                         downloadButton('download',"Download")
                       )
                )
              )
            )),
    
    
    
    
    ### Data Visualization
    tabItem(tabName = "eda",
            fluidPage(
              fluidRow(
                selectizeInput("plotinput1", "Select x-variable to plot", choices = names(getData()), selected = "age"),
                selectizeInput("plotinput2", "Select y-variable to plot", choices = names(getData()), selected = "heartRate"),
                selectizeInput("plotType1", "Select the type of Plot", choices = c("Box","Scatter", "Bar"), selected = "Scatter"),
                conditionalPanel("input.plotType1 == 'Bar'",
                                 selectizeInput("groupby", "Group Bar Plot By", choices = c("male", "currentSmoker", "prevalentStroke" ), selected = "male"))
              ),
              fluidRow(
                plotOutput("diabplot")
              ),
              fluidRow(
                h3(strong("Select the statistic you want to see for people who have diabetes")),
                
                selectizeInput("stats1", "Select metric", choices = c("age", "heartRate", "glucose", "totChol", "BMI", "sysBP", "diaBP", "cigsPerDay"), selected = "age"),
                
                box(
                  width = 12,
                  background = "purple",
                  textOutput("summ1")
                )
              )
              
            )
    ),
    
    
    
    ### Modelling 
    
    tabItem(
      tabName = "modeling",
      fluidPage(
        tabBox(width = 12,
               tabPanel("Modeling info", "Different types of models :",
                        column(12,   
                               fluidRow(h3(strong("1. Generalized Linear Model"))),
                               fluidRow(h4("Generalized Linear Regression model allows for response from non normal distributions and have both continuous and categorical predictors. The GLM equation is :")),
                               withMathJax(
                                 helpText('$$ {\\ln\\left(\\frac{p}{1-p}\\right)} = \\beta_0 + \\beta_1\\ x_1 + .... + \\beta_n\\ x_n $$')
                               ),
                               fluidRow(h4(strong("Advantages :"))),
                               fluidRow(h4("Response variable can have any form of exponential distribution type.")),
                               fluidRow(h4(strong("Disadvantages :"))),
                               fluidRow(h4("Does not select features. ")),
                               fluidRow(h4("Predictor variables need to be uncorrelated. ")),
                               
                               fluidRow(h3(strong("2. Classification Tree"))),
                               fluidRow(h4("This method splits up predictor space into regions and performs different predictions for each region. Classification trees are generally used when the goal is to predict group membership. A Classification tree is built through a process known as binary recursive partitioning.")),
                               fluidRow(h4(strong("Advantages :"))),
                               fluidRow(h4(("Simple to understand and easy to interpret"))),
                               fluidRow(h4(("Predictors don't need to be scaled"))),
                               fluidRow(h4("No statistical assumptions necessary")),
                               fluidRow(h4(strong("Disadvantages :"))),
                               fluidRow(h4(("Small changes in data can vastly change tree"))),
                               fluidRow(h4("Greedy algorithm necessary (no optimal algorithm")),
                               fluidRow(h4("Usually pruning is needed")),
                               
                        
                               fluidRow(h3(strong("3. Random Forest")),
                                        h4("Random Forest creates multiple trees from the bootstrap samples and averages the results of 
                                           all. It does not use all the predictors but uses a random subset of predictors for each bootstrap sample fit."),
                                        h4(strong("Advantages :")),
                                        h4("It can perform both regression as well as classification tasks."),
                                        h4("It can handle large data efficiently."),
                                        h4("It provide higher accuracy than decision tree models."),
                                        h4(strong("Disadvantages :")),
                                        h4("Large number of trees can make the algorithm too slow")
               ))),
               
               
               tabPanel("Model Fitting", 
                        fluidRow(
                          column(12, align = "center",
                                 h5("Select the training/testing data split ratio"),
                                 sliderInput("traintest", "Select a ratio", min = 0, max = 1, value = 0.7)
                          ),
                          br(),
                          column(12,
                                 h3(strong("Train Models")),
                                 h4("Select training Parameters for each model"),
                                 h4("Response Variable: ", strong("Diabetes", style = "color:#00496b;")),
                                 br(),
                                 h4(strong("Model 1 - Generalized Linear Regression"), style = "color:#00496b;"),
                                 h4("Select the k-folds for cross validation for GLM model"),
                                 numericInput("cvglm", "Set the K-Folds", min = 2, max = 15, value = 3),
                                 h4("Select the variables you want to train your GLM Model to:"),
                                 column(6,
                                        checkboxInput("glmmale", "male", value = TRUE),
                                        checkboxInput("glmage", "age", value = TRUE),
                                        checkboxInput("glmBMI", "BMI", value = TRUE),
                                        checkboxInput("glmcurrentSmoker", "currentSmoker", value = TRUE),
                                        checkboxInput("glmcigsPerDay", "cigsPerDay", value = TRUE),
                                        checkboxInput("glmheartRate", "heartRate", value = TRUE)
                                 ),
                                 column(6,
                                        checkboxInput("glmsysBP", "sysBP", value = TRUE),
                                        checkboxInput("glmdiaBP", "diaBP", value = TRUE),
                                        checkboxInput("glmtotChol", "totChol", value = TRUE),
                                        checkboxInput("glmprevalentStroke", "prevalentStroke", value = TRUE),
                                        checkboxInput("glmglucose", "glucose", value = TRUE),
                                        checkboxInput("glmBPMeds", "BPMeds", value = TRUE)
                                        
                                 ),
                                 br(),
                                 h4(strong("Model 2 - Classifcation Tree"), style = "color:#00496b;"),
                                 h4("Select the k-folds for cross validation for Classification Tree model"),
                                 numericInput("cvct", "Set the K-Folds", min = 2, max = 15, value = 3),
                                 h4("Select the tunelength parameter for Classification Tree"),
                                 numericInput("tunect", "Set the tunelength", min = 2, max = 15, value = 2),
                                 h4("Select the variables you want to train your Classification Tree Model to:"),
                                 column(6,
                                        checkboxInput("ctmale", "male", value = TRUE),
                                        checkboxInput("ctage", "age", value = TRUE),
                                        checkboxInput("ctBMI", "BMI", value = TRUE),
                                        checkboxInput("ctcurrentSmoker", "currentSmoker", value = TRUE),
                                        checkboxInput("ctcigsPerDay", "cigsPerDay", value = TRUE),
                                        checkboxInput("ctheartRate", "heartRate", value = TRUE)
                                 ),
                                 column(6,
                                        checkboxInput("ctsysBP", "sysBP", value = TRUE),
                                        checkboxInput("ctdiaBP", "diaBP", value = TRUE),
                                        checkboxInput("ctprevalentStroke", "prevalentStroke", value = TRUE),
                                        checkboxInput("cttotChol", "totChol", value = TRUE),
                                        checkboxInput("ctglucose", "glucose", value = TRUE),
                                        checkboxInput("ctBPMeds", "BPMeds", value = TRUE)
                                        
                                 ),
                                 br(),
                                 h4(strong("Model 3 - Random Forest"), style = "color:#00496b;"),
                                 h4("Select the k-folds for cross validation for Random Forest model"),
                                 numericInput("cvrf", "Set the K-Folds", min = 2, max = 15, value = 3),
                                 h4("Select the mtyr parameter for Random Forest model"),
                                 numericInput("mtryrf", "Enter mtry", min = 1, max = 15, value = 2),
                                 h4("Select the variables you want to train your Random Forest Model to:"),
                                 column(6,
                                        checkboxInput("rfmale", "male", value = TRUE),
                                        checkboxInput("rfage", "age", value = TRUE),
                                        checkboxInput("rfBMI", "BMI", value = TRUE),
                                        checkboxInput("rfcurrentSmoker", "currentSmoker", value = TRUE),
                                        checkboxInput("rfcigsPerDay", "cigsPerDay", value = TRUE),
                                        checkboxInput("rfheartRate", "heartRate", value = TRUE)
                                 ),
                                 column(6,
                                        checkboxInput("rfsysBP", "sysBP", value = TRUE),
                                        checkboxInput("rfdiaBP", "diaBP", value = TRUE),
                                        checkboxInput("rfprevalentStroke", "prevalentStroke", value = TRUE),
                                        checkboxInput("rftotChol", "totChol", value = TRUE),
                                        checkboxInput("rfglucose", "glucose", value = TRUE),
                                        checkboxInput("rfBPMeds", "BPMeds", value = TRUE)
                                        
                                 ),
                                 actionButton("runmodel", h4("Build All", style = "color:#00496b;")),
                                 br(),
                                 h3(strong("Model Output:")),
                                 
                                 h4(strong("Generalized Linear Regression")),
                                 h4("Summary:"),
                                 verbatimTextOutput("GLMModel"),
                                 h4("Test Statistics:"),
                                 verbatimTextOutput("GLMTest"),
                                 
                                 
                                 h4(strong("Classification Tree")),
                                 h4("Summary:"),
                                 verbatimTextOutput("CTModel"),
                                 h4("Test Statistics:"),
                                 verbatimTextOutput("CTTest"),
                                 
                                 
                                 h4(strong("Random Forest")),
                                 h4("Summary:"),
                                 verbatimTextOutput("RFModel"),
                                 h4("Test Statistics:"),
                                 verbatimTextOutput("RFTest"),
                                 h4("Variable Importance Graph"),
                                 plotOutput("RFPlot")
                                 
                                 
                          )
                        )
               ),
               
               
               tabPanel("Prediction", 
                        h4("Predict Diabetes:"),
                        selectizeInput("modelsel", "Select Model for Preiction", choices = c("Generalized Linear Regression", "Classification Tree", "Random Forest"), selected = "Generalized Linear Regression"),
                        
                        
                    
                        conditionalPanel("input.modelsel == 'Generalized Linear Regression'",
                                         conditionalPanel("input.glmmale == 1", 
                                                          selectizeInput("predglmmale", "Select male", choices = c(0,1), selected = 0)
                                         ),
                                         conditionalPanel("input.glmage == 1", 
                                                          numericInput("predglmage", "Enter Age", min = 0, value = 50)),
                                         conditionalPanel("input.glmBMI == 1", 
                                                          numericInput("predglmBMI", "Enter BMI", min = 0, value = 10)),
                                         conditionalPanel("input.glmcurrentSmoker == 1", 
                                                          selectizeInput("predglmcurrentSmoker", "Current Smoker?", choices = c(0,1), selected = 0)),
                                         conditionalPanel("input.glmcigsPerDay", 
                                                          numericInput("predglmcigsPerDay", "Number of Cigarettes Per Day", min = 0, value = 2)),
                                         conditionalPanel("input.glmheartRate", 
                                                          numericInput("predglmheartRate", "Enter Heart Rate", min = 0, value = 80)),
                                         conditionalPanel("input.glmsysBP", 
                                                          numericInput("predglmsysBP", "Systolic Blood Pressure", min = 30, value= 50)),
                                         conditionalPanel("input.glmdiaBP",
                                                          numericInput("predglmdiaBP", "Diabolic Blood Pressure", min = 70, value= 100)),
                                         conditionalPanel("input.glmtotChol",
                                                          numericInput("predglmtotchol", "Cholestrol Value", min = 0, value= 20)),
                                         conditionalPanel("input.glmprevalentStroke",
                                                          selectizeInput("predglmprevalentStroke", "Prevalent Stroke?", choices = c(0,1), selected = 0)),
                                         conditionalPanel("input.glmglucose",
                                                          numericInput("predglmglucose", "Glucose Value", min = 0, value= 20)),
                                         conditionalPanel("input.glmBPMeds",
                                                          selectizeInput("predglmBPMeds", "Any BP medications ?", choices = c(0,1), selected = 0)),
                                         textOutput("glmpred")
                        
                        ),
                        conditionalPanel("input.modelsel == 'Classification Tree'",
                                         conditionalPanel("input.ctmale == 1", 
                                                          selectizeInput("predctmale", "Select male", choices = c(0,1), selected = 0)
                                         ),
                                         conditionalPanel("input.ctage == 1", 
                                                          numericInput("predctage", "Enter Age", min = 0, value = 50)),
                                         conditionalPanel("input.ctBMI == 1", 
                                                          numericInput("predctBMI", "Enter BMI", min = 0, value = 10)),
                                         conditionalPanel("input.ctcurrentSmoker == 1", 
                                                          selectizeInput("predctcurrentSmoker", "Current Smoker?", choices = c(0,1), selected = 0)),
                                         conditionalPanel("input.ctcigsPerDay", 
                                                          numericInput("predctcigsPerDay", "Number of Cigarettes Per Day", min = 0, value = 2)),
                                         conditionalPanel("input.ctheartRate", 
                                                          numericInput("predctheartRate", "Enter Heart Rate", min = 0, value = 80)),
                                         conditionalPanel("input.ctsysBP", 
                                                          numericInput("predctsysBP", "Systolic Blood Pressure", min = 30, value= 50)),
                                         conditionalPanel("input.ctdiaBP",
                                                          numericInput("predctdiaBP", "Diabolic Blood Pressure", min = 70, value= 100)),
                                         conditionalPanel("input.cttotChol",
                                                          numericInput("predcttotchol", "Cholestrol Value", min = 0, value= 20)),
                                         conditionalPanel("input.ctprevalentStroke",
                                                          selectizeInput("predctprevalentStroke", "Prevalent Stroke?", choices = c(0,1), selected = 0)),
                                         conditionalPanel("input.ctglucose",
                                                          numericInput("predctglucose", "Glucose Value", min = 0, value= 20)),
                                         conditionalPanel("input.ctBPMeds",
                                                          selectizeInput("predctBPMeds", "Any BP medications ?", choices = c(0,1), selected = 0)),
                                         textOutput("ctpred")
                        
                        ),
                        conditionalPanel("input.modelsel == 'Random Forest'",
                                         conditionalPanel("input.rfmale == 1", 
                                                          selectizeInput("predrfmale", "Select male", choices = c(0,1), selected = 0)
                                         ),
                                         conditionalPanel("input.rfage == 1", 
                                                          numericInput("predrfage", "Enter Age", min = 0, value = 50)),
                                         conditionalPanel("input.rfBMI == 1", 
                                                          numericInput("predrfBMI", "Enter BMI", min = 0, value = 10)),
                                         conditionalPanel("input.rfcurrentSmoker == 1", 
                                                          selectizeInput("predrfcurrentSmoker", "Current Smoker?", choices = c(0,1), selected = 0)),
                                         conditionalPanel("input.rfcigsPerDay", 
                                                          numericInput("predrfcigsPerDay", "Number of Cigarettes Per Day", min = 0, value = 2)),
                                         conditionalPanel("input.rfheartRate", 
                                                          numericInput("predrfheartRate", "Enter Heart Rate", min = 0, value = 80)),
                                         conditionalPanel("input.rfsysBP", 
                                                          numericInput("predrfsysBP", "Systolic Blood Pressure", min = 30, value= 50)),
                                         conditionalPanel("input.rfdiaBP",
                                                          numericInput("predrfdiaBP", "Diabolic Blood Pressure", min = 70, value= 100)),
                                         conditionalPanel("input.rftotChol",
                                                          numericInput("predrftotchol", "Cholestrol Value", min = 0, value= 20)),
                                         conditionalPanel("input.rfprevalentStroke",
                                                          selectizeInput("predrfprevalentStroke", "Prevalent Stroke?", choices = c(0,1), selected = 0)),
                                         conditionalPanel("input.rfglucose",
                                                          numericInput("predrfglucose", "Glucose Value", min = 0, value= 20)),
                                         conditionalPanel("input.rfBPMeds",
                                                          selectizeInput("predrfBPMeds", "Any BP medications ?", choices = c(0,1), selected = 0)),
                                         textOutput("rfpred")
                        )
                        
                        
               )
        )
      )
    )
    
    
  ))




dashboardPage(
  dashboardHeader(title = "Framingham Heart Disease dataset"),
  sidebar,
  body
)


# body <- dashboardBody(
#   tabItems(
#     tabItem(tabName = "data",
#             fluidPage(
#               fluidRow(
#                 titlePanel("Framhingham Heart Disease Data set"),
#                 dataTableOutput("table")
# column(4,
#        selectInput("sex",
#                    "Sex",
#                    c("Male")))
#                 )
#                 )
#   )
# )
# )

# Define UI for application that draws a histogram
#shinyUI(fluidPage(

# Application title
#   titlePanel("Old Faithful Geyser Data"),

# Sidebar with a slider input for number of bins
#  sidebarLayout(
#     sidebarPanel(
#        sliderInput("bins",
#                  "Number of bins:",
#                   min = 1,
#                 max = 50,
#                value = 30)
#),

# Show a plot of the generated distribution
#mainPanel(
#    plotOutput("distPlot")
#  )
#   )
#))
