## Loading of Data and pre-processing

#PACKAGE TIMEEEEE
pkgs = c("TTR", "quantmod",
         "caret", "fscaret", "neuralnet", "kernlab", "gmodels", "C50", "nnet",
         "shiny")
for (p in pkgs) {
  if (! require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}


library(plyr)
library(tidyverse)
library(ggplot2)

library(quantmod)
library(TTR)
library(caret);
library(fscaret);
library(neuralnet);
library(kernlab);
library(gmodels);
library(C50);
library(nnet);
library(shiny)

# Data Prep
fulldata_temp <- read.table(file = "../data/marcel.csv", sep=";", dec=",", header=TRUE)
fulldata <- fulldata_temp[20:nrow(fulldata_temp),]

move_cols <- sapply(fulldata, is.character)
move_cols[["RSI.Move"]] <- TRUE
move_cols[["Target"]] <- TRUE
move_data <- as.data.frame(sapply(fulldata[,move_cols], as.factor))
move_col_names <- names(move_data)
non_move_data <- fulldata[, -which(names(fulldata) %in% move_col_names)]
fulldata <- cbind(non_move_data,move_data)

Target <- fulldata$Target
fulldata$Target <- NULL
fulldata <- cbind(fulldata,Target)
fulldata <- fulldata[,2:ncol(fulldata)]
numRows = dim(fulldata)[1]
lastday = fulldata[numRows, ] 
fulldata = fulldata[1:numRows-1, ]

set.seed(123)

## Load packages
##################

body <- dashboardBody(
  fluidRow(
    # Content
    box(plotOutput("plot1", height = 250),
    # Content
      title = "Box title", 
      width = 6,
      status = "primary"
    ),
    box(
      status = "warning", 
      width = 6,
      "Box content"
    )
  ),
  
  fluidRow(
    column(width = 4,
           box(
             title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
             "Box content"
           ),
           box(
             width = NULL, background = "black",
             "A box with a solid black background"
           )
    ),
    
    column(width = 4,
           box(
             title = "Title 3", width = NULL, solidHeader = TRUE, status = "warning",
             "Box content"
           ),
           box(
             title = "Title 5", width = NULL, background = "light-blue",
             "A box with a solid light-blue background"
           )
    ),
    
    column(width = 4,
           box(
             title = "Title 2", width = NULL, solidHeader = TRUE,
             "Box content"
           ),
           box(
             title = "Title 6", width = NULL, background = "maroon",
             "A box with a solid maroon background"
           )
    )
  )
)

# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  dashboardHeader(title = "Mixed layout"),
  dashboardSidebar(selectInput("kernel", "Choose a Kernel:",
                               choices = c("polydot", "vanilladot")),
                   sliderInput("split", "Training split rate", 0.01, 0.99, 98),
                   checkboxGroupInput("predictors", "Run ML-SVM algorithm with following preditors:",
                                      names(fulldata), selected = names(fulldata))),
  body
)

# Server
server <- function(input, output) {

  
  fulldata <- reactive({
    fulldata [,input$predictors]
  })
  
  splitIndex <- reactive({
    fdd <- fulldata()
    createDataPartition(fdd$Target, p = input$split, list = FALSE, times = 1)
  })
  
  #random train / testing
  
  
  trainDF <- reactive({
    fd <- fulldata()
    sub1 <- fd[splitIndex(),]
    sub1
    })
  
  testDF <- reactive({
    fd <- fulldata()
    sub2 <- fd[splitIndex(),]
    sub2
  })
  
  # Modelltraining
  svm.model <- reactive({ksvm(Target ~., data = trainDF(), kernel = input$kernel, C=9)})
  
  #Modellperformance-Auswertung
  #svm.predict <- predict(svm.model,testDF)
  #table(svm.predict,testDF$Target)
  

  #confusion matrix
  #svm.confusion.linear <-  predict(svm.model.linear,trainDF)
  
  #predicted values
  svm.predict <-  reactive({predict(svm.model(),testDF())})
  
  #accuracy f?r modell
  agreement <- reactive({svm.predict() == testDF()$Target})
  
  accu = reactive({sum(agreement()) / length(agreement())})
  
  # print(accu())
  # print(svm.model())
  
  output$plot1 <- renderPlot({

      ggplot()+
      labs(title = paste0(accu()))
  })
}

# Preview the UI in the console
shinyApp(ui = ui, server = server)