library(tidyverse)
library(shiny)

# Define UI for displaying current time ----

ui <- fluidPage(
  # App title ----
  titlePanel("Merkayla's Timer for challenge 8"),
  h2(textOutput("currentTime")),
  p("Making changes to the timer"),
  p("struggling"),
  em("testing testing"),
  br(),  
  
  img(src = "ticktock.png", height = 400, width = 400),
  
)

# Define server logic to show current time, update every second ----
server <- function(input, output, session) {
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)