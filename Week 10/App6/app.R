#########################################################
#
# Author Nick Lichtsinn
# Lab 10: App 6
#
#########################################################
library(shiny)
library(leaflet) #raster
library(ggmap)

# available to both server and ui
my.dir <- "C:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Week 7/"
libs <- read.csv(paste0(my.dir, "newyorklibraries.csv"), stringsAsFactors = FALSE, quote = "\"", header = TRUE)
ny.libs <- nrow(libs)

server <- function(input, output, session) {
  print("server:: start")
  points <- eventReactive(input$num.libs, {
    index <- sample(1:nrow(libs), input$num.libs)
    addys <- paste(libs$ADDRESS[index], libs$CITY[index], libs$STABR[index], sep = ", ")
    g.codes <- geocode(addys, source = "dsk")
    df <- data.frame(lon = g.codes$lon, lat = g.codes$lat, addy = addys)
    df
  }, ignoreNULL = FALSE) # if someone interacts with webpage we will know
  
  output$mymap <- renderLeaflet({
    M <- leaflet()
    M <- addProviderTiles(M, providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE))
    df <- points()
    addMarkers(M, lng = df[, 1], lat = df[, 2], popup = df[, 3])
  })
}

ui <- fluidPage(
  leafletOutput("mymap"),
  numericInput("num.libs", "Number of Libraries", 10, min = 1, max = ny.libs)
)

shinyApp(ui, server)














































