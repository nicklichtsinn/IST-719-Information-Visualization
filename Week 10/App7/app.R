#########################################################
#
# Author Nick Lichtsinn
# Lab 10: App 7
#
#########################################################
library(shiny)

server <- function(input, output) {
  
  piefunction <- function(x) {
    par(mar = c(.5,.5,.5,.5))
    pie(1:x)
  }
  
  output$plot1 <- renderPlot({
    A <- sample(LETTERS[3:(2+input$slider2)], input$slider1, replace = TRUE)
    B <- sample(LETTERS[12:(11+input$slider3)], input$slider1, replace = TRUE)
    barplot(table(A, B), beside = TRUE)
  })
  output$plot2 <- renderPlot({ 
    c.num.letters <- input$slider5
    d.num.letters <- input$slider6
    print(c.num.letters, d.num.letters)
    
    C <- sample(LETTERS[7:(6 + c.num.letters)], input$slider4, replace = TRUE)
    D <- sample(LETTERS[17:(16 + d.num.letters)], input$slider4, replace = TRUE)
    barplot(table(C,D), beside = TRUE)
  })
}

ui <- fluidPage(
  titlePanel("Helly Shiny!"),
  
  wellPanel(
    fluidRow(
      column(6, 
      sliderInput("slider1", "P1 Observations", min = 10, max = 1000, value = 20),
      sliderInput("slider2", "A cats", min = 2, max = 5, value = 2),
      sliderInput("slider3", "B cats", min = 2, max = 5, value = 3)
    ),
      column(6, 
           sliderInput("slider4", "P2 Observations", min = 10, max = 1000, value = 20),
           sliderInput("slider5", "A cats", min = 2, max = 5, value = 2),
           sliderInput("slider6", "B cats", min = 2, max = 5, value = 3)
    )
  ),
  fluidRow(
    column(6, plotOutput("plot1")),
    column(6, plotOutput("plot2"))
  )
 )
)

shinyApp(ui, server)









































