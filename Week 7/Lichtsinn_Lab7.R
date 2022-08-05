##############################################
#
# Author Nick Lichtsinn
#Lab 7: maps
#
#################################################

my.dir <- "C:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Week 7/"
df <- read.csv(file = paste0(my.dir, "MapLectureData.csv"), header = TRUE, stringsAsFactors = FALSE)

plot(df$x, df$y)
polygon(df$x, df$y, col = "firebrick1", border = NA)

#install.packages("maps")
library(maps)
#install.packages("mapproj")
library(mapproj)

map(database = "world")
map(database = "world", regions = 'India')
map(database = "world", regions = 'China')
map(database = "world", regions = c('India', "Pakistan"), fill = TRUE, col = c("orange", "brown"))
map(database = "world", regions = 'Finland')

m <- map("state")

map("state", fill = TRUE, col = c("orange", "red", "yellow"))
map("county", region = "New York", fill = T, col = terrain.colors(20))

#install.packages("devtools") # I guess you also need this
#devtools::install_github("ropensci/rnaturalearthhires")
library("rnaturalearth")


india <- ne_states(country = "India")

map(india)
india

attributes(india)
names(india)

india$name
map(india, namefield = "name", regions = c("Gujarat", "Rajasthan", "Madhya", "Pradesh")
    , fill = TRUE
    , col = c("orangered", "white", "springgreen4"))

#install.packages("raster")
library(raster)
# two dots say look into this package and get this information using this function
# use ISO standard 3 letter country code
india <- raster::getData("GADM", country = "IND", level = 1)
map(india)

map(india, namefield = "NAME_1", region = "Gujarat")

# grabbing level 2 data
india <- raster::getData("GADM", country = "IND", level = 2)
map(india)

map(india, namefield = "NAME_2", region = "North 24 Parganas", fill = TRUE, col = "springgreen4")

#####################################################################
# Chloropleth map problem
#####################################################################

my.dir <- "C:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Week 7/"
fname <- paste0(my.dir, "shootings.Rda")
load(fname)

# which state has the most mass shooting victims in the us?
View(shootings)
shootings$Total.Number.of.Victims

# have to clean the data to aggregate
sort(shootings$State)
tmp.vec <- gsub("^\\s+|\\s+$", "", shootings$State) 
# carat says to make a match at start of string
# \\s says look for white space
# + says match one or more of these spaces
# | says and or
# \\s+$ says ignore space in the middle and look at the end
sort(tmp.vec)

shootings$State <- tmp.vec
agg.dat <- aggregate(shootings$Total.Number.of.Victims, list(shootings$State), sum) 

colnames(agg.dat) <- c("state", "victims")

# dont go over 255 bc there is only 255 shades
num.cols <- 10
my.color.vec <- rev(heat.colors(num.cols))

pie(rep(1, num.cols), col = my.color.vec)

library(plotrix)
agg.dat$index <- round(rescale(x = agg.dat$victims, c(1, num.cols)))

agg.dat$color <- my.color.vec[agg.dat$index]

m <- map("state")
m$names
state.order <- match.map(database = "state", regions = agg.dat$state
                         , exact = FALSE, warn = TRUE)
# look to see if washingtons matched up
cbind(m$names, agg.dat$state[state.order])

map("state", col = agg.dat$color[state.order], fill = TRUE
    , resolution = 0, lty = 1, projection = "polyconic", border = "tan")

#####################################################################
# points and geocoding
#####################################################################


my.dir <- "C:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Week 7/"
libs <- read.csv(file = paste0(my.dir, "NewYorkLibraries.csv"), header = TRUE, quote = "\"", stringsAsFactors = FALSE)

map("world")
points(0,0,col = "red", cex = 3, pch = 8)
abline(h = 43, col = "blue", lty = 3) # lat
abline(v = -76, col = "blue", lty = 3) # long

# what are top cities in us in terms of population?

us.cities
map("state")
my.cols <- rep(rgb(1,.6, .2, .7), length(us.cities$name))
my.cols[us.cities$capital > 0] <- rgb(.2, .6, 1, .9)
points(us.cities$long, us.cities$lat, col = my.cols, pch = 16, cex = rescale(us.cities$pop, c(.5, 7)))

#install.packages("ggmap")
library(ggmap)

#install.packages("tmaptools")
library(tmaptools)
geocode_OSM('Syracuse, NY'
            ,return.first.only=T
            ,server = "http://nominatim.openstreetmap.org"
)

table(libs$CITY)
index <- which(libs$CITY %in% c("SYRACUSE", "DEWITT", "FAYETTEVILLE"))

addy <- paste(libs$ADDRESS[index], libs$CITY[index], libs$STABR[index], sep = ", ")

map("county", "new york", fill = TRUE, col = "orange")
g.codes <- geocode_OSM('Syracuse, NY'
                       ,return.first.only=T
                       ,server = "http://nominatim.openstreetmap.org"
)
points(g.codes$coords, g.codes$coords, col = "blue", cex = 1.1, pch = 16)

#####################################################################
# rworld maps
#####################################################################
#install.packages("rworldmap")
library(rworldmap)
my.dir <- "C:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Week 7/"
countries <- read.delim(paste0(my.dir, "countries.csv"), header = TRUE, quote = "\"", sep = ";", stringsAsFactors = FALSE)

# getting rid of countries with 0.0 life expectancy
range(countries$Life.expectancy)
#zap <- which(countries$Life.expectancy == 0.0)
rm(zap)
countries <- countries[-zap, ]
# r color world matches on iso 3 letter country abbr
num.cat <- 10
iso3.codes <- tapply(countries$Country..en., 1:length(countries$Country..en.)
                     , rwmGetISO3)

df <- data.frame(country = iso3.codes, labels = countries$Country..en., life = countries$Life.expectancy)
df.map <- joinCountryData2Map(df, joinCode = "ISO3", nameJoinColumn = "country")

par(mar = c(0,0,1,0))
mapCountryData(df.map
               , nameColumnToPlot = "life"
               , numCats = num.cat
               , catMethod = c("pretty", "fixedWidth", "diverging", "quantiles")[4]
               , colourPalette = colorRampPalette(c("orangered", "palegoldenrod", "forestgreen")
                                                  )(num.cat)
                                                  , oceanCol = "royalblue4"
                                                  , borderCol = "peachpuff4"
                                                  , mapTitle = "Life Expectancy"
               )


#####################################################################
# ggmaps
#####################################################################

my.dir <- "C:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Week 7/"
reported <- read.csv(file = paste0(my.dir, "IndiaReportedRapes.csv")
                 , header = TRUE, quote = "\"", stringsAsFactors = FALSE)

india <- raster::getData("GADM", country = "IND", level = 1)
cbind(unique(reported$Area_Name), india$NAME_1)

india$NAME_1[india$NAME_1 == "NCT of DElhi"] <- "Delhi"
india$NAME_1 <- gsub(" and ", " & ", india$NAME_1)

map <- fortify(india, region = "NAME_1") # fortify will not work cannot get the plot to work
head(map)
crimes <- aggregate(reported$Cases, list(reported$Area_Name), sum)
colnames(crimes) <- c("id", "ReportedRapes")
crimes[order(crimes$ReportedRapes)]

my.map <- merge( x = map, y = crimes, by = "id")

ggplot() + geom_map(data = my.map, map = my.map) + aes(x = long, y = lat, map_id = id, group = group
                                                       , fill = ReportedRapes
                                                       , theme_minimal() + ggtitle("Reported Rapes in India"))


#####################################################################
# ggmaps
#####################################################################

shape.dat.dir <- "C:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Week 7/shapefiles/"

library(stringr)
#install.packages("rgdal")
library(rgdal)
library(raster)
#install.packages("TeachingDemos")
library(TeachingDemos)

bikes <- readRDS(paste0(shape.dat.dir, "bikes.rds"))
nypp <- readOGR(paste0(shape.dat.dir, "nyct2010_17a"), "nyct2010", stringsAsFactors = FALSE)

syr.neighborhood <- readOGR(paste0(shape.dat.dir, "syracuse-neighborhoods_ny.geojson"))

par(mar = c(.5,.5,.5,.5))
plot(nypp, border = "bisque4", lwd = .5)
zoomplot(c(978000, 999800), ylim = c(185000, 225000))

df <- data.frame(lat = bikes$start.station.latitude, lon = bikes$start.station.longitude)
head(df)

point.tab <- sort(table(paste(df$lat, df$lon)), decreasing = TRUE)
point.tab[1:3]

df2 <- data.frame(lat = as.numeric(word(names(point.tab),1))
                  , lon = as.numeric(word(names(point.tab),2)))
df2$size <- as.numeric(point.tab)

coordinates(df2) <- ~lon + lat
crs(df2) <- CRS("+proj=longlat + datum=WGS84")
df2 <- spTransform(df2, crs(nypp))

tmp.size <- .2 + (2*df2$size/max(df2$size))
points(df2$lon, df2$lat, col = "red", pch = 19, cex = tmp.size)























plot(sales$income)






plot(, namefield = "name", regions = c("Gujarat", "Rajasthan", "Madhya", "Pradesh")
    , fill = TRUE
    , col = c("orangered", "white", "springgreen4"))






















