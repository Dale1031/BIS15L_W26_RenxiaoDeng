library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)

ui <- 
  dashboardPage(
    
    dashboardHeader(title = "Age or Height by Sex"),
    
    dashboardSidebar(
      selectInput("x",
                  "Select Age or Height",
                  choices = c("age","height"))
      
    ), 
    dashboardBody(
      plotOutput("plot", width="600px", height="500px")
    )
  )

server <- function(input, output, session) {
  output$plot <- renderPlot({
    elephants %>%
      ggplot(aes(y = .data[[input$x]],x=sex,fill=sex)) +
      geom_boxplot()
  })
}

shinyApp(ui, server)
