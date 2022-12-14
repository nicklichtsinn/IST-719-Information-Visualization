# ##############################################
# #
# # Author Nick Lichtsinn
# #Lab 6: ggplot
# #
# #################################################
# 
# my.dir <- "C:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Week 3/"
# sales <- read.csv(file = paste0(my.dir, "sales.csv"), header = TRUE, stringsAsFactors = FALSE)

library(ggplot2)

p <- ggplot(sales)
p

class(p)
attributes(p)
p$data
p$layers
p$scales
summary(p)
view(p$data)

ggplot(sales) + aes(x = expenses)
range(sales$expenses)

plot(sales$expenses)

ggplot(sales) + aes(x = expenses, y = income) + geom_point()

ggplot(sales) + 
  aes(x = expenses, y = income) + 
  geom_point()

p <- ggplot(sales) + aes(x = expenses, y = income)
p + geom_point(color = "red")


ggplot(sales) + aes(x = expenses, y = income, color = type) + geom_point()
ggplot(sales) + aes(x = expenses, y = income, color = unit.price > 14) + geom_point()


ggplot(sales) + aes(x = expenses, y = income) + geom_point(color = ifelse(sales$unit.price > 14, "red", "green"))

ggplot(sales) + aes(x = expenses, y = income, color = unit.price) + geom_point()

ggplot(sales) + aes(x = expenses, y = income
                    , color = rep.region
                    , shape = type
                    , alpha = unit.price
                    , size = units.sold) + 
  geom_point()

p1 <- ggplot(sales)
p2 <- ggplot(sales) + aes(y = income, x = expenses, shape = rep.region)

summary(p1)
summary(p2)

attributes(p1)
attributes(p2)

p1$labels
p2$labels

p1$mapping
p2$mapping

# add a prediction line

ggplot(sales) + aes(x = expenses, y = income) + geom_point() + geom_rug()

income.pred <- predict(lm(sales$income~sales$expenses))
# put line inside the aes to tell it to only use it for this part of the plot
ggplot(sales) + aes(y = income, x = expenses) + geom_point() + geom_line(aes(y = income.pred), color = "red", lwd = 3) +geom_rug()

ggplot(sales) + aes(y = income, x = expenses) + geom_point(color = "pink") + 
  geom_rug() +
  geom_line(aes(y = income.pred)) +
  geom_line(aes(y = income.pred + 150)) +
  geom_vline(xintercept = 10, color = "blue") +
  geom_hline(yintercept = 500, color = "orange") +
  geom_abline(intercept = 50, slope = 100, color = "red", lty = 3, lwd = 2)
    
ggplot(sales) + aes(x = expenses, y = income) + geom_point() + geom_smooth()

ggplot(sales) + aes(x = expenses, y = income) + geom_bin2d(bins = 50)

price <- ifelse(sales$unit.price > 14, "expensive", "moderate")
price[sales$unit.price < 9] <- "cheap"

ggplot(sales) + aes(x = expenses, y = income, color = price) + geom_bin2d(bins = 50)


df <- aggregate(sales$units.sold, list(year = sales$year), sum)
df2 <- aggregate(sales$units.sold, list(year = sales$year, region = sales$rep.region), sum)

ggplot(sales) + aes(x = income) + geom_blank()
ggplot(sales) + aes(x = income) + geom_histogram(binwidth = 10)
hist(sales$income)

ggplot(sales) + aes(x = income) + geom_histogram(binwidth = 10, fill = "orange") +
  geom_vline(aes(xintercept = mean(income)), color = "blue", linetype = "dashed", size = 1)

ggplot(sales) + aes(x = income) + geom_histogram(binwidth = 10, fill = "orange", alpha = 0.9) + aes(y = ..density..) + 
  geom_density(alpha = 0.3, fill = "blue", color = "blue")

ggplot(sales) + aes(x = rep.region, y = income) +geom_boxplot()

ggplot(df) + aes(x = year, y = x) + geom_line() + ylim(c(0, 40000))

ggplot(df) + aes(x = year, y = x) + geom_step() + ylim(c(0, 40000))

ggplot(df) + aes(x = year, y = x) + geom_ribbon(aes(ymin = x - 1000, ymax = x + 1000, fill = "yellow")) + geom_line() + ylim(c(0, 40000))

ggplot(df2) + aes(x = year, y = x, color = region) + geom_line() + ylim(c(0,10000))

ggplot(df2) + aes(x = year, y = x, color = region) + geom_rect(xmin = 10000, xmax = 10000, ymin = 10000, ymax = 10000)


###############################
# geom bar
###############################

df <- aggregate(sales$units.sold, list(region = sales$rep.region), sum)
colnames(df)[2] <- "sales"

ggplot(sales) + aes(x = rep.region) + geom_bar(fill = "orange", width = .5) +
  ggtitle("Number of Sales by Region")

ggplot(sales) + aes(x = rep.region, fill = type) + geom_bar(position = "fill")
df
ggplot(df) + aes(x = region, y = sales, fill = region) + 
  geom_bar(stat = "identity")

ggplot(df) + aes(x = " ", y = sales, fill = region) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 45)

hist(log(sales$income))

p <- ggplot(sales) + aes(x = income)
p + geom_histogram() + stat_bin(binwidth = 20)
p + stat_density()

ggplot(sales) + aes(y = income) + geom_boxplot() + stat_boxplot()

ggplot(sales) + aes(y = income) + stat_boxplot()

ggplot(sales) + aes(x = sales$expense, y = income) + stat_bin2d() + stat_density2d(col = "red")

ggplot(sales) + aes(x = rep.region) + stat_count()

df
ggplot(df) + aes(x = region, y = sales) + geom_bar(stat = "identity")
ggplot(df) + aes(x = region, y = sales) + geom_bar() + stat_identity()

ggplot(sales) + aes(x = income) + 
  geom_histogram(aes(fill = ..count..)) + 
  aes(y = ..density..) + 
  geom_density(fill = "yellow", alpha = .1)

memory.size()

