## Loading of Data and pre-processing


## Load packages

library(plyr)
library(tidyverse)
library(ggplot2)

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
  dashboardSidebar(sliderInput("slider", "Number of observations:", 1, 500, 50)),
  body
)

# Server
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    lala <- histdata[seq_len(input$slider)] 
    ggplot(data.frame(x = lala)) + geom_histogram(aes(x = x),bins = 30)
  })
}

# Preview the UI in the console
shinyApp(ui = ui, server = server)