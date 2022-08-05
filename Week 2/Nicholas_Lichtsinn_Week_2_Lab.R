#
# Author Nicholas Lichtsinn
# Lab 2, second week - data interogation or data exploration and distributions
#

fname <- file.choose()
tips <- read.csv(file = fname
                 , header = TRUE
                 , stringsAsFactors = FALSE)

colnames(tips)
fix(tips)
View(tips)

str(tips)


tips[1,] # first row
tips[ , 1] # first column
tips[3,3] # value at 3rd row and 3rd column
tips[1:3, ] # first 3 rows

length(tips[1:3, 2]) # length of the first 3 rows of the second column

dim(tips) # dimensions in the dataset
dim(tips)[1] # look at first dimension

tips$time # look at time column
tips[, "time"]

plot(tips$total_bill) # plot the distribution
plot(sort(tips$total_bill))

par(mfrow = c(2,2))
boxplot(tips$total_bill)
hist(tips$total_bill)

# plotting the shape of the data

d <- density(tips$total_bill)
plot(d)
polygon(d, col = "purple")

#install.packages("vioplot")
library(vioplot)
vioplot(tips$total_bill)

# distribution of Tip column
plot(tips$tip)

unique(tips$sex)

tips_M <- tips[tips$sex == "Male", ]
tips_F <- tips[tips$sex == "Female", ]

par(mfrow = c(2,1), mar = c(2,3,1,2))
boxplot(tips_F$tip, horizontal = T, ylim = c(1,10))
boxplot(tips_M$tip, horizontal = T, ylim = c(1,10))


fname2 <- file.choose()
library(jsonlite)

raw_tweet <- fromJSON(fname2, flatten = FALSE)
str(raw_tweet)


View(raw_tweet)
names(raw_tweet)

raw_tweet$text
raw_tweet$user$followers_count

raw_tweet[["user"]]$followers_count
raw_tweet[["user"]][["followers_count"]]

fname3 <- file.choose()
con <- file(fname3, open = "r")

tweets <- stream_in(con)
close(con)

dim(tweets)

tweets$text[1:3]

boxplot(log10(tweets$user$followers_count), horizontal = TRUE)

# grouping functions in r

task_time <- c(rnorm(n = 30, mean = 30, sd = 2.25)
               , rnorm(n = 30, mean = 25, sd = 1.5))

hist(task_time)

status <- c(rep("AMA", 30), rep("PRO", 30))

df <- data.frame(time = task_time, status = status)

df_grouped <- aggregate(df$time, list(df$status), mean)

df_grouped
colnames(df_grouped) <- c("status", "time")

barplot(df_grouped$time, names.arg = df_grouped$status)

M_grouped <- tapply(df$time, list(df$status), mean)
M_grouped

tapply(df$time, list(df$status), range)


aggregate(df$time, list(df$status), summary)

table(df$status) # just counts does not group
table(df$time) # table on continuous data is useless unless you round

df$sex <- sample(c("M", "F"), 60, replace = T)

aggregate(df$time, list(df$status, df$sex), mean)

M <- tapply(df$time, list(df$sex, df$status), mean)
barplot(M)
barplot(M, beside = TRUE)

########################################################
# reshaping data with tidyr
# gather() makes wide data longer
# spread() makes long data wider
# separate() splits a single column into multiple columns
# unite() combines multiple columns into a single column

library(tidyr)

n <- 5
year <- 2001:(2000 + n)
q1 <- runif(n = n, min = 100, max = 120)
q2 <- runif(n = n, min = 103, max = 130)
q3 <- runif(n = n, min = 105, max = 140)
q4 <- runif(n = n, min = 108, max = 150)

df_wide <- data.frame(year, q1, q2, q3, q4)

gather(df_wide, qt, sales, q1:q4)

df_long <- df_wide %>% gather(qt, sales, q1:q4)

o <- order(df_long$year, df_long$qt)
df_long <- df_long[o, ]

df <- data.frame(cat = rep(c("tap", "reg", "zed", "vum"), 3)
                 , group = rep(letters[7:9], 4)
                 , x = 1:12)

spread(df, cat, x)


#######################################################
# using rect function to build a custom plot
#install.packages("plotrix")
library(plotrix)
n <- 7000
age.min <- 1
age.max <- 90
age.range <- c(age.min, age.max)
m <- round(rescale(rbeta(n, 5, 2.5), age.range), 0)
f <- round(rescale(rbeta(n, 5, 2.0), age.range), 0)
x <- age.min:age.max
f.y <- m.y <- rep(0, length(x))

m.tab <- table(m)
m.y[as.numeric((names(m.tab)))] <- as.numeric(m.tab)

f.tab <- table(f)
f.y[as.numeric((names(f.tab)))] <- as.numeric(f.tab)

age.freqs <- data.frame(ages = x, males = m.y, females = f.y)

max.x <- round(1.2 * max(age.freqs[ ,2:3]), 0)
plot(c(-max.x, max.x), c(0,100), type = "n", bty = "n", xaxt = "n", ylab = "age", xlab = "freq", main = "sample age distribution")


grid()
last.y <- 0
for (i in 1:90) { 
  rect(xleft = 0, ybottom = last.y, xright = -age.freqs$males[i], ytop = age.freqs$ages[i], col = "lightblue2", border = NA)
  rect(xleft = 0, ybottom = last.y, xright = age.freqs$females[i], ytop = age.freqs$ages[i], col = "lightpink", border = NA)
  
  last.y <- age.freqs$ages[i]
  }












