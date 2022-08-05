######################################
#
# Author: Nick Lichtsinn
# Homework 4
#
######################################

crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.csv", sep = ",", header = TRUE)

crime2 <- crime[crime$state != "District of Columbia",]
crime2 <- crime2[crime2$state != "United States",]

# figure 6-10
pairs(crime2[,2:9], panel = panel.smooth, col = "lightblue2")

#figure 6-15
symbols(crime$murder, crime$burglary, circles = crime$population)

radius <- sqrt( crime$population/ pi)

symbols(crime$murder, crime$burglary, circles = radius, inches = 0.35, fg = "white", bg = "red", xlab = "Murder Rate", ylab = "Burglary Rate", xlim = c(0,10))

# figure 6-24
birth <- read.csv("http://datasets.flowingdata.com/birth-rate.csv", sep = ",", header = TRUE)

hist(birth$X2008, main = "Global Distribution of Birth Rates", col = "purple")


#figure 6-32

birth2008 <- birth$X2008[!is.na(birth$X2008)]

d2008 <- density(birth2008)

plot(d2008, type = "n", main = "GLOBAL DISTRIBUTION OF BIRTH RATES IN 2008", xlab = "Live births per 1,000 population", ylab = "Density")
polygon(d2008, col = "#821122", border = "#cccccc")



# my own multiple dimension plot
my.dir <- "C:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Week 3/"
sales <- read.csv(file = paste0(my.dir, "sales.csv"), header = TRUE, stringsAsFactors = FALSE)

hist(sales$income)
boxplot(sales$income)
str(sales)
# getting rid of outliers
sales <- sales[sales$income < 300, ]

# setting breaks
breaks <- seq(0, 300, by = 50)

#set layout
par(mfrow = c(3,2))

hist(sales[sales$wine == "Merlot",]$income, breaks = breaks, main = "Merlot Sales", xlab = "Income", col = "dark red")
hist(sales[sales$wine == "Riesling",]$income, breaks = breaks, main = "Riesling Sales", xlab = "Income", col = "light yellow")
hist(sales[sales$wine == "Cabernet Sauvignon",]$income, breaks = breaks, main = "Cabernet Sauvignon Sales", xlab = "Income", col = "dark red")
hist(sales[sales$wine == "Pinot Gris",]$income, breaks = breaks, main = "Pinot Gris Sales", xlab = "Income", col = "light yellow")
hist(sales[sales$wine == "Shiraz",]$income, breaks = breaks, main = "Shiraz Sales", xlab = "Income", col = "dark red")
hist(sales[sales$wine == "Chardonnay",]$income, breaks = breaks, main = "Chardonnay Sales", xlab = "Income", col = "light yellow")




hist(sales$unit.price)
boxplot(sales$unit.price)
str(sales)
# getting rid of outliers
sales <- sales[sales$unit.price <= 14, ]

# setting breaks
breaks <- seq(0, 14, by = .5)

#set layout
par(mfrow = c(3,2))

hist(sales[sales$wine == "Merlot",]$unit.price, breaks = breaks, main = "Merlot Price", xlab = "Price", col = "dark red")
hist(sales[sales$wine == "Riesling",]$unit.price, breaks = breaks, main = "Riesling Price", xlab = "Price", col = "light yellow")
hist(sales[sales$wine == "Cabernet Sauvignon",]$unit.price, breaks = breaks, main = "Cabernet Sauvignon Price", xlab = "Price", col = "dark red")
hist(sales[sales$wine == "Pinot Gris",]$unit.price, breaks = breaks, main = "Pinot Gris Price", xlab = "Price", col = "light yellow")
hist(sales[sales$wine == "Sauvignon Blanc",]$unit.price, breaks = breaks, main = "Sauvignon Blanc Price", xlab = "Price", col = "light yellow")
hist(sales[sales$wine == "Chardonnay",]$unit.price, breaks = breaks, main = "Chardonnay Price", xlab = "Price", col = "light yellow")

