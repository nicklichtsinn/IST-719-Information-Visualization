#########################################################
#
# Author Nick Lichtsinn
#Lab 10: App 3
#
#########################################################

library(shiny)
fname <- "C:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Homeworks/Week 2 Homework/art.csv"


artserver <- function(input, output) { 
  art <- read.csv(fname, header = TRUE, stringsAsFactors = FALSE)
  watercolor.col <- "cadetblue1"
  drawing.col <- "antiquewhite"
  
  # plotOutput("yearlyReceipts")
  output$yearlyReceipts <- renderPlot({
    print("Inside yearlyReceipts") # instrumenting, will print here that the code is inside the function yearly receipts
    my.title <- "Number of Sales per Year"
    barplot(table(art$year), main = my.title, border = "white", col = "chartreuse4")
  })
  
  output$storePaper <- renderPlot({
    print("Inside storePaper")
    if (input$store != "None") {
      print(paste("storePaper:: store:", input$store))
      sub.index <- which(art$store == input$store)
      tmp.data <- art[sub.index, ]
      pie(table(tmp.data$paper), col = c(watercolor.col,drawing.col), border = NA)
    }
  })
}

artui <- fluidPage(
  titlePanel("ACME Art Company Dashboard"),
 
  sidebarLayout(
    sidebarPanel(
      plotOutput("yearlyReceipts"),
      selectInput("store", "Select Store:"
                  , choices = c("None", "Portland", "Davenport", "Syracuse", "Dublin"))
    ),
     mainPanel(
       plotOutput("storePaper"),
      )
  )
)

shinyApp(ui = artui, server = artserver)