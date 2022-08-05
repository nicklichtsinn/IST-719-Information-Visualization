##############################################
#
# Author Nick Lichtsinn
#Lab 5
#
#################################################

my.dir <- "C:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Week 5/"
tweets <- read.csv(file = paste0(my.dir, "climatetweets_useforlecture_25k.csv")
                   , header = TRUE
                   , quote = "\""
                   , stringsAsFactors = FALSE)
view(tweets)

y.media <- tweets$media
y.media

table(y.media)
y.media[y.media == ""] <- "text only"
y.media <- gsub("\\|photo", "", y.media)

pie(100 * round(table(y.media)/sum(table(y.media)), 4))

tweets$created_at[1:3]

"Wed Jul 06 03:35:37 +0000 2016"

conversion.string <- "%a %b %d %H:%M:%S +0000 %Y"

tmp <- strptime(tweets$created_at, conversion.string)

tmp
class(tmp)
any(is.na(tmp))

rm(tmp)
tweets$date <- strptime(tweets$created_at, conversion.string)

tmp <- "10AM and 27 minutes, on June 22, 1999"
strptime(tmp, "%H%p and %M minutes, on %B %d, %Y")

min(tweets$date)
max(tweets$date)
range(tweets$date)
summary(tweets$date)

difftime(min(tweets$date), max(tweets$date))
difftime(min(tweets$date), max(tweets$date), units = "min")
difftime(min(tweets$date), max(tweets$date), units = "weeks")

install.packages("lubridate")
library(lubridate)

wday(tweets$date[1:3], label = TRUE, abbr = TRUE)

barplot(table(wday(tweets$date, label = TRUE, abbr = TRUE)))

tmp <- tweets$user_utc_offset
tweets$date[7:10] + tmp[7:10]

known.times <- tweets$date + tweets$user_utc_offset

index <- which(is.na(known.times))
known.times[index]

known.times <- known.times[-index]

barplot(table(hour(known.times)))


tmp <- "2018.08.30-16.24.49"
strptime(tmp, "%Y.%B.%d-%H.%M.%S")


start.date <- as.POSIXct("2016-06-24 23:59:59")
end.date <- as.POSIXct("2016-06-26 00:00:00")

index <- which((tweets$date > start.date) & (tweets$date < end.date))

tweets.25th <- tweets$date[index]
format.Date(tweets.25th, "%Y%m%d%H%M")

tmp.date <- as.POSIXct(strptime(format.Date(tweets.25th, "%Y%m%d%H%M"), "%Y%m%d%H%M"))

plot(table(tmp.date))

length(table(tmp.date))
24*60

tmp.tab <- table(tmp.date)

plot(as.POSIXct(names(tmp.tab)), as.numeric(tmp.tab), type = "h")

x <- seq.POSIXt(from = start.date + 1, to = end.date - 1, by = "min")
length(x)
y <- rep(0, length(x))
y[match(names(tmp.tab), as.character(x))] <- as.numeric(tmp.tab)

plot(x,y, type = "p", pch = 16, cex = .4)
plot(x,y, type = "l")


# hashtag wordcloud

tweets$text[5:10]

library(stringr)

tags <- str_extract_all(tweets$text, "#\\S+", simplify = FALSE)
tags

tags <- tags[lengths(tags) > 0]
tags <- unlist(tags)
tags

tags <- tolower(tags)
tags <- gsub("#|[[:punct:]]", "", tags)

tags.tab <- sort(table(tags), decreasing = TRUE)
tags.tab[1:10]

zap <- which(tags.tab < 3)
tags.tab <- tags.tab[-zap]

boxplot(as.numeric(tags.tab))
plot(as.numeric(tags.tab))

df <- data.frame(words = names(tags.tab), count = as.numeric(tags.tab), stringsAsFactors = FALSE)

par(mfrow = c(3,3))
plot(df$count, main = "raw")
y <- df$count/max(df$count)
plot(y, main = "0-1")
plot(df$count^2, main = "^2")
plot(df$count^(1/2), main = "^(1/2")
plot(df$count^(1/5), main = "^(1/5")
plot(log10(df$count), main = "log10")
plot(log(df$count), main = "log")
# log10(c(1,10,100,1000,1237,10000))

# wordcloud
library(wordcloud)
myPal <- colorRampPalette(c("purple", "blue", "gold"))

gc()

df
my.counts <- (df$count[index])^(1/2)
index <- which(df$count > 9)
par(mar=c(0,0,0,0), bg = "black")
wordcloud(df$words[index], my.counts, scale = c(6, 0.7)
          , min.freq = 1
          , max.words = Inf, random.order = FALSE
          , random.color = FALSE, ordered.colors = TRUE
          , rot.per = 0, colors = myPal(length(df$words[index])))

##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################

my.dir <- "C:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Week 3/"
sales <- read.csv(file = paste0(my.dir, "sales.csv"), header = TRUE, stringsAsFactors = FALSE)

#install.packages("alluvial")
library(alluvial)
dat <- as.data.frame(Titanic, stringsAsFactors = FALSE)
alluvial(dat[,1:4], freq = dat$Freq)

alluv.df <- aggregate(sales$units.sold, list(sales$rep.region, sales$type), sum)

colnames(alluv.df) <- c("reg", "type", "units.sold")

alluvial(alluv.df[,1:2], freq = alluv.df$units.sold)
my.cols <- rep("gold", nrow(alluv.df))
my.cols[alluv.df$type == "red"] <- "red"

alluvial(alluv.df[,1:2], freq = alluv.df$units.sold, col = my.cols)

alluvial(alluv.df[,1:2], freq = alluv.df$units.sold, col = ifelse(alluv.df$type == "red", "red", "gold"))

options(stringsAsFactors = FALSE)

alluv.df <- aggregate(sales$units.sold, list(sales$rep.region
                                             , sales$type
                                             , sales$wine)
                      , sum)
colnames(alluv.df) <- c("reg", "type", "wine", "units.sold")

alluvial(alluv.df[,1:3], freq = alluv.df$units.sold, col = ifelse(alluv.df$type == "red", "red", "gold"), border = 3, gap.width = .25)


library(RColorBrewer)
#install.packages("treemap")
library(treemap)

colnames(sales)


treemap(sales, index = c("rep.region")
        , vSize = "income"
        , fontsize.labels = 18
        , palette = "Greens")



treemap(sales, index = c("rep.region")
        , vSize = "income"
        , vColor = "units.sold"
        , type = "dens"
        , fontsize.labels = 18
        , palette = "Greens")

treemap(sales, index = c("rep.region")
        , vSize = "income"
        , vColor = "units.sold"
        , type = "value"
        , fontsize.labels = 18
        , palette = "OrRd")

treemap(sales, index = c("rep.region", "sales.rep", "type")
        , vSize = "income"
        , vColor = "units.sold"
        , type = "index"
        , fontsize.labels = 18
        , palette = brewer.pal(8, "Set1"))


##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################

#install.packages("riverplot")
library(riverplot)
river <- riverplot.example()
par(mfrow = c(2,1), mar = c(1,1,1,1))
plot(river, srt = 90, lty = 1)
class(river)

x <- river
x$edges
x$nodes
x$edges$Value[2] <- 45
x$edges$Value[1] <- 15
x$nodes$x[5] <- 5

plot(x, srt = 90, lty = 1)

df <- aggregate(sales$income, list(type = sales$type, wine = sales$wine), sum)
df <- df[order(df$type, df$x), ]

node.name <- c("wine", unique(df$type), df$wine)
node.position <- c(1,2,2,3,3,3,3,3,3,3)
node.color <- rep("gray", length(node.name))
node.color <- c("deepskyblue", "red", "yellow"
                , "brown4", "firebrick3", "deeppink4"
                , "khaki1", "lightgoldenrod1", "gold", "goldenrod1")

node <- data.frame(ID = node.name, x = node.position, col = node.color, stringsAsFactors = FALSE)

parent.nodes <- c("wine", "wine", df$type)
child.nodes <- c("red", "white", df$wine)

value <- c(sum(df$x[df$type == "red"]), sum(df$x[df$type == "white"]), df$x)
edges <- data.frame(N1 = parent.nodes, N2 = child.nodes, Value = value)

r <- makeRiver(node, edges)
par(mar = c(0,0,0,0))
plot(r)


##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################


dat <- tapply(sales$units.sold, list(sales$type, sales$rep.region), sum)

barplot(dat, beside = TRUE, col = c("brown", "gold"), main = "Units Sold by region by Type")































