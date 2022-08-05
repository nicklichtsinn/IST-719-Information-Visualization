#########################################################
#
# Author Nick Lichtsinn
# Lab 10: App 4
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
    print("yearlyReceipts:: start") # instrumenting, will print here that the code is inside the function yearly receipts
    my.title <- "Number of Sales per Year"
    barplot(table(art$year), main = my.title, border = "white", col = "chartreuse4")
  })
  
  output$storePaper <- renderPlot({
    print("storePaper:: start")
    if (input$store != "None") {
      print(paste("storePaper:: store:", input$store))
      sub.index <- which(art$store == input$store)
      # sub.index <- which(art$store == "Dublin")
      tmp.data <- art[sub.index, ]
      #pie(table(tmp.data$paper), col = c(watercolor.col,drawing.col), border = NA)
      if (input$year != "All") {
        print(paste("storePaper:: year:", input$year))
        sub.index.2 <- which(tmp.data$year == as.numeric(input$year))
        tmp.data <- tmp.data[sub.index.2, ]
      }
      sales.by.paper <- tapply(tmp.data$total.sale, list(tmp.data$paper), sum)
      barplot(sales.by.paper, beside = TRUE, main = "Income by Paper Type"
              , col = c(watercolor.col,drawing.col), border = NA)
    }
  })
}

artui <- fluidPage(
  titlePanel("ACME Art Company Dashboard"),
 
  sidebarLayout(
    sidebarPanel(
      plotOutput("yearlyReceipts"),
      selectInput("store", "Select Store:"
                  , choices = c("None", "Portland", "Davenport", "Syracuse", "Dublin")),
      selectInput("year", "Select Year:"
                  , choices = c("All", "2012", "2013", "2014", "2015"))
      
    ),
     mainPanel(
       plotOutput("storePaper"),
      )
  )
)

shinyApp(ui = artui, server = artserver)