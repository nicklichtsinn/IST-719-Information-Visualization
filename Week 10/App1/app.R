#########################################################
#
# Author Nick Lichtsinn
#Lab 10: App 1
#
#########################################################

library(shiny)
library(lubridate)

server <- function(input, output) { 
  
  
}

ui <- fluidPage(
  mainPanel(paste("Jeff's Shiny app at", now()))
)

shinyApp(ui, server)

