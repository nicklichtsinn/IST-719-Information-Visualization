#########################################################
#
# Author Nick Lichtsinn
#Lab 10: App 2
#
#########################################################

library(shiny)
fname <- "C:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Homeworks/Week 2 Homework/art.csv"


artserver <- function(input, output) { 
  art <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)
  # plotOutput("yearlyReceipts")
  output$yearlyReceipts <- renderPlot({
    print("Inside yearlyReceipts") # instrumenting, will print here that the code is inside the function yearly receitps
    my.title <- "Number of Sales per Year"
    barplot(table(art$year), main = my.title, border = "white", col = "chartreuse4")
  })
}

artui <- fluidPage(
  titlePanel("ACME Art Company Dashboard"),
  mainPanel(plotOutput("yearlyReceipts"))
)

shinyApp(ui = artui, server = artserver)