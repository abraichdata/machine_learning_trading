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
library(shinydashboard)

library(plyr)
library(tidyverse)
library(ggplot2)
library(ggalt)

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
library(dygraphs)

# Data Prep
fulldata_temp <- read.table(file = "marcel.csv", sep=";", dec=",", header=TRUE)
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
dday <- fulldata$Date[1:length(fulldata$Date)-1]
fulldata <- fulldata[,2:ncol(fulldata)]
numRows = dim(fulldata)[1]
lastday = fulldata[numRows, ] 
fulldata = fulldata[1:numRows-1, ]

set.seed(123)

## Load packages
##################

body <- dashboardBody(
  
  tabBox(title = "intro",id= "ttabs", width = 12, height = "420px",
         tabPanel("Model Evaluation", 
                  
                  
                  fluidRow(
                    box(plotOutput("plot1"),
                        # Content
                        title = "Predictions vs. History", 
                        status = "primary"
                        
                    ),
                    box(dygraphOutput("tipping_dygraph"),
                        # Content
                        title = "Splitting Rate calibration", 
                        status = "primary"
                      
                    )
                    ),
                  fluidRow(
                    # A static valueBox
                    
                    valueBoxOutput("acc_box"),
                    valueBoxOutput("acc_box2"),
                    valueBoxOutput("acc_box3"),
                    valueBoxOutput("acc_box4"),
                    valueBoxOutput("acc_box5")
                    
                    
                  )
                  
                  
                  
                  
                  
                  
                  ),
         tabPanel("Complete Data", 
                  box(dataTableOutput("DTT"),width = 5,
                      title = "Titel",
                      status = "primary")
                  
                  
         ))
  
  
)

# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  dashboardHeader(title = "Model tuning"),
  dashboardSidebar(selectInput("kernel", "Choose a Kernel:",
                               choices = c("polydot", "vanilladot")),
                   sliderInput("split", "Training split rate", 0.01, 0.99, value=  0.7),
                   
                   checkboxGroupInput("predictors", "Run ML-SVM algorithm with following preditors:",
                                      names(fulldata), selected = names(fulldata))),
  body
)

accu.linear <- read.csv("accu.csv")

# Server
server <- function(input, output) {

  fulldata_re <- reactive({
    data.frame(fulldata [input$predictors],date = dday)
  })
  
  splitIndex <- reactive({
    createDataPartition(fulldata_re()$Target, p = input$split, list = FALSE, times = 1)
  })
  
  #random train / testing
  
  trainDF <- reactive({fulldata_re()[splitIndex(),]})
  testDF <- reactive({fulldata_re()[-splitIndex(),]})

  # Modelltraining
  svm.model <- reactive({ksvm(Target ~., data = trainDF(), kernel = input$kernel, C=9)})
  svm.model2 <- reactive({ksvm(Target ~., prob.model = TRUE,data = trainDF(), kernel = input$kernel, C=9)})
  
  #Modellperformance-Auswertung
  #svm.predict <- predict(svm.model,testDF)
  #table(svm.predict,testDF$Target)
  

  #confusion matrix
  #svm.confusion.linear <-  predict(svm.model.linear,trainDF)
  
  #predicted values
  svm.predict <-  reactive({predict(svm.model(),testDF())})
  svm.predictprob <-  reactive({predict(svm.model2(),testDF(),type = "prob")})
  

  
  
  #accuracy f?r modell
  agreement <- reactive({svm.predict() == testDF()$Target})
  tp <- reactive({svm.predict() == 1 &  testDF()$Target == 1})
  fp <- reactive({svm.predict() == 1 &  testDF()$Target == 0})
  tn <- reactive({svm.predict() == 0 &  testDF()$Target == 0})
  fn <- reactive({svm.predict() == 0 &  testDF()$Target == 1})
  
  
  accu = reactive({sum(agreement()) / length(agreement())})
  tps = reactive({sum(tp()) / length(agreement())})
  fps = reactive({sum(fp()) / length(agreement())})
  tns = reactive({sum(tn()) / length(agreement())})
  fns = reactive({sum(fn()) / length(agreement())})
  
  
  
  # print(accu())
  # print(svm.model())
  

  
    
  output$acc_box <- renderValueBox({

    valueBox(round(accu(),3), "Accuracy",icon = icon("scale", lib = "glyphicon"), color = "aqua",
             href = NULL)
    
  })
  
  output$acc_box2 <- renderValueBox({
    
    valueBox(round(tps(),3), "True Positives",icon = icon("ok-sign", lib = "glyphicon"), color = "green",
             href = NULL)
    
  })
  
  output$acc_box3 <- renderValueBox({
    
    valueBox(round(fps(),3), "False Positives",icon = icon("ok-sign", lib = "glyphicon"), color = "red",
             href = NULL)
    
  })
  
  output$acc_box4 <- renderValueBox({
    
    valueBox(round(tns(),3), "True negatives",icon = icon("remove-sign", lib = "glyphicon"), color = "green",
             href = NULL)
    
  })
  
  output$acc_box5 <- renderValueBox({
    
    valueBox(round(fns(),3), "False negatives",icon = icon("remove-sign", lib = "glyphicon"), color = "red",
             href = NULL)
    
  })
  
  # Prob Function
  pr2 <- Vectorize(function (p0 = 0.6,r=4,k=1) {
    
    m = floor(r/2) + 1
    
    i <- seq(from = m,
             to = r,
             by = 1) #%>% min()
    
    
    kronecker_delta <- function(a,b) {if (a == b) return(1) else if (a != b) return(0)}
    
    
    sum (choose(r,i) * p0^i * (1-p0)^(r-i)) + k* kronecker_delta(r/2 , floor(r/2)) * choose(r,floor(r/2)) * p0^(r/2) * (1-p0)^(r/2)
    
  }, "p0")
  
  output$tipping_dygraph <- 
    renderDygraph ({
      
      #input_r <-input$tipping_r  # input_r = 4
      #input_k <- input$tipping_k # input_k = 0.4
      
      y1 <- accu.linear[,2]
      y2 <- (seq(from = 0,1,length.out = 98))
      ts <- data.frame(seq(from = 0,1,length.out = 98),
                       y1)#,
                       #y2)
      
      dygraph(ts,
              main = paste0("Accuracy for different splitting rates"),
              ylab = "Percentage of Data used as Training Data",
              xlab = "Accuracy of Predictions") %>%
        dyLegend(  show = "follow",hideOnMouseOut = FALSE)  %>% 
        dySeries("y1", fillGraph = TRUE, color = "red",label = "accuracy") #%>% 
        #dySeries("y2", fillGraph = TRUE, color = "blue", strokePattern = "dashed",label = "p(t+1)") 
      
    }) 
  
  output$DTT <- 
    renderDataTable(data.frame(predicted = svm.predict(),
                               svm.predictprob(),
                               time2(),
                               testDF()))
  
  df <- reactive({
    data.frame(predicted = svm.predict(),
             prob = svm.predictprob()[,2],
             testDF())
  })
  
  output$plot1 <- 
    renderPlot({
      
      ggplot(data = df()) + 
        geom_dumbbell(mapping = aes(xend = prob,x = 0.5,y = date,colour = Target))+
        geom_vline(xintercept = 0.5,linetype = "dashed")+
        coord_flip(xlim = NULL, ylim = NULL, expand = TRUE)+
        theme(axis.text.x = element_text(angle = 270,size = 5))
      
      
      
    }
    )
}

# Preview the UI in the console
shinyApp(ui = ui, server = server)