# Author Nicholas Lichtsinn
# Purpose Homework Week 2
####################################

##############################################################################################################
##############################################################################################################
# PART 1
##############################################################################################################
##############################################################################################################



##############################################################################################################
# Creating a bar chart
##############################################################################################################

hotdogs <- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv", sep = ",", header = TRUE)

# original boxplot
barplot(hotdogs$Dogs.eaten)

# adding year names to the bars
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year)

# adding axis labels and changing the colors and borders
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col = "red", border = NA, xlab = "Year", ylab = "Hot dogs and buns (HDB) eaten")

# shading bars when the US when the contest
# creating the color filter loop
fill_colors <- c()
for (i in 1:length(hotdogs$Country) ) {
  if (hotdogs$Country[i] == "United States") {
    fill_colors <- c(fill_colors, "#821122")
  } else {
      fill_colors <- c(fill_colors, "#cccccc")
  }
}

# passing fill_colors through the boxplot parameters
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col = fill_colors, border = NA, xlab = "Year", ylab = "Hot dogs and buns (HDB) eaten")

# creating fill_colors when a record was broken
fill_colors <- c()
for (i in 1:length(hotdogs$New.record) ) {
  if (hotdogs$New.record[i] == 1) {
    fill_colors <- c(fill_colors, "#821122")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}

# passing fill_colors through the boxplot parameters
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col = fill_colors, border = NA, xlab = "Year", ylab = "Hot dogs and buns (HDB) eaten")

# increasing the spacing of the plot and adding a title
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col = fill_colors, border = NA, space = 0.3, xlab = "Year", ylab = "Hot dogs and buns (HDB) eaten", main = "Nathan's Hot Dog Eating Contest Results, 1980-2010")


##############################################################################################################
# Creating a stacked bar chart
##############################################################################################################

#reading in placing data
hot_dog_places <- read.csv("http://datasets.flowingdata.com/hot-dog-places.csv", sep = ",", header = TRUE)

# renaming columns without the X
names(hot_dog_places) <- c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010")

# converting dataframe into a matrix
hot_dog_matrix <- as.matrix(hot_dog_places)

barplot(hot_dog_matrix, border = NA, space = 0.25, ylim = c(0, 200), xlab = "Year", ylab = "Hot dogs and buns (HDBs) eaten", main = "Hot Dog Eating Contest Results, 1980-2010")


##############################################################################################################
# creating a scatterplot
##############################################################################################################

subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv", sep = ",", header = TRUE)

# plotting the subscribers column
plot(subscribers$Subscribers)

# settting type to point and range of vertical axis
plot(subscribers$Subscribers, type = "p", ylim = c(0, 30000))

# combining vertical lines and points on the plot
plot(subscribers$Subscribers, type = "h", ylim = c(0, 30000), xlab = "Day", ylab = "Subscribers")
points(subscribers$Subscribers, pch = 19, col = "black")


##############################################################################################################
# creating a time series chart
##############################################################################################################

population <- read.csv("http://datasets.flowingdata.com/world-population.csv", sep = ",", header = TRUE)

# creating the plot
plot(population$Year, population$Population, type = "l", ylim = c(0, 7000000000), xlab = "Year", ylab = "Population")



##############################################################################################################
# Creating a Step Chart
##############################################################################################################

# reading in csv
postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv", sep = ",", header = TRUE)

#plotting step chart
plot(postage$Year, postage$Price, type = "s")

# adding labels and title
plot(postage$Year, postage$Price, type = "s", main = "US Postage Rates for Letters, First Ounce, 1991-2010", xlab = "Year", ylab = "Postage Rate (Dollars)")



##############################################################################################################
##############################################################################################################
# PART 2: Simple Distributions
##############################################################################################################
##############################################################################################################

# reading in the csv
art <- read.csv("/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Homeworks/art.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

d_paper <- art[art$paper == "drawing", ]
w_paper <- art[art$paper == "watercolor", ]

# setting plot space
par(mfrow = c(2, 2))
plot(art$total.sale, main = "Distribution of total.sales", col = "red", xlab = "Frequency", ylab = "Total Sales (Dollars)", pch = "N")
boxplot(art$total.sale, main = "Distribution of total.sales", col = "purple", ylab = "Total Sales (Dollars)")

hist(w_paper$total.sale, main = "distribution of the totals sales for watercolor paper", col = "green", ylab = "Frequency", xlab = "Total Sales (Dollars)")
hist(d_paper$total.sale, main = "distribution of the totals sales for drawing paper", col = "blue", ylab = "Frequency", xlab = "Total Sales (Dollars)")


##############################################################################################################
##############################################################################################################
# PART 3: Grouping and Multidimension Plots
##############################################################################################################
##############################################################################################################

par(mfrow = c(1, 3))

plot(art$units.sold, art$unit.price, type = "h", main = "Unit Price vs Units Sold", ylab = "Unit Price (Dollars)", xlab = "Units Sold", lwd = 5, xlim = c(0, 25), ylim = c(0, 100))

# there is a definite relationship between unit price and units sold with art goods with lower prices being bought more frequently with the good costing $100 being bought only once and cheaper options being bought over 20 times

df_grouped <- aggregate(art$units.sold, list(art$paper), sum)
df_grouped

colnames(df_grouped) <- c("Paper_Type", "Units_Sold")

barplot(df_grouped$Units_Sold, names.arg = df_grouped$Paper_Type, main = "Units Sold of Drawing and Watercolor Paper", xlab = "Paper Type", ylab = "Units Sold", col = c("purple", "green"), ylim = c(0, 16000))
 # the company sold many more units of watercolor than drawing paper

df_grouped2 <- aggregate(art$total.sale, list(art$paper), sum)
df_grouped2

colnames(df_grouped2) <- c("Paper_Type", "Income")

barplot(df_grouped2$Income, names.arg = df_grouped$Paper_Type, main = "Income from Sales of Drawing and Watercolor Paper", xlab = "Paper Type", ylab = "Income (Dollars)", col = c("orange", "lightblue"), ylim = c(0, 120000))
# the company made more money from selling watercolor than drawing paper, but the different is not as much as units sold

unemployment <- read.csv("http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv")

unemployment[1:10,]
# plain scatter plot
plot(1:length(unemployment$Value), unemployment$Value)

#Loess curve
scatter.smooth(x=1:length(unemployment$Value), y=unemployment$Value)

#Loess curve - fitted
scatter.smooth(x=1:length(unemployment$Value), y=unemployment$Value, ylim = c(0,11), degree = 2, col = "#CCCCCC", span = 0.5)













