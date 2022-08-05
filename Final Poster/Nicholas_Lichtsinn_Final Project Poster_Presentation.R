########################################################################
# Author Nicholas Lichtsinn
# Final Project: Premier League Spending & Results
########################################################################
library(dplyr)
library(ggplot2)


#importing the premier league dataset
PL <- read.csv("c:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Homeworks/Week 3 Homework/stats.csv")
Spend <- read.csv("c:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Homeworks/Week 3 Homework/PL_Spent.csv")
# adding money spent data to pl dataframe
PL$spend <- Spent$ï..Spent..M.
# Description: This data shows the Premier League results and stats for each team from the 2006/07 to the 2017/18 seasons.


# str function
str(PL)

# dataset size calculation = (42(columns)*4)*(240(rows)/100) = 403.2

# Single-dimension Plots

# Plot 1
hist(PL$penalty_save, main = "Frequency of Penalty Saves per Team per Season", xlab = "Penalty Saves", ylab = "Frequency", col = "Purple")
mtext('https://www.kaggle.com/datasets/zaeemnalla/premier-league?resource=download', side=1, line=3, at=5)

#Plot 2
table <- table(PL$team)
labels <- paste(names(table), "\n", table, sep="")
pie(table, labels = labels, main = "Number of Seasons Each Team Was in the Premier League (2006-2018)", col = 1:100, border = "white", clockwise = TRUE)
mtext('https://www.kaggle.com/datasets/zaeemnalla/premier-league?resource=download', side=1, line=1, at=1)

# Plot 3
boxplot(PL$wins, breaks = 5, col = "lightblue", main = "Frequency of Wins per Team per Season", xlab = "Wins", ylab = "Frequency", density = 20)
mtext('https://www.kaggle.com/datasets/zaeemnalla/premier-league?resource=download', side=1, line=4, at=100)

boxplot(PL$spend, breaks = 5, col = "lightblue", main = "Frequency of Spend per Team per Season", xlab = "Spend (Millions)", ylab = "Frequency", density = 20)

# Multi dimensional plots
# Plot 3
grouped <- tapply(PL$wins, list(PL$season), mean)
barplot(grouped, main = "Average Total Premier League Wins per Season", xlab = "Season", ylab = "Wins", col = c(1:12), ylim = c(0, 60))
mtext('https://www.kaggle.com/datasets/zaeemnalla/premier-league?resource=download', side=1, line = 4, at = 15)

#Plot 4
t <- subset(PL, team = "Manchester United", select = c(wins, goals))
plot(t, main = "Manchester United Wins vs Goals", xlab = "Wins", ylab = "Goals Scored", col = "Red", pch = "M")
mtext('https://www.kaggle.com/datasets/zaeemnalla/premier-league?resource=download', side=1, line=4, at=25)



# number of times each team was in the premier league
PL %>% count(team, sort = TRUE)

# teams who were in the league every season
t10 <- PL %>%
  dplyr::group_by(team) %>%
  dplyr::filter(n() > 9)
# bottom teams
b10 <- PL %>%
  dplyr::group_by(team) %>%
  dplyr::filter(n() < 4)

# top teams pie chart
t10$count <- 1
t10_counts <- t10 %>%
  group_by(team) %>%
  summarise_at(vars(count), list(count = sum))

ggplot(t10_counts, aes(x = "", y = count, fill = reorder(team, count))) + 
  geom_bar(width = 1, stat = "identity", color = "black") + 
  labs(title = "Top Teams Number of Seasons in the Premier League (2006-2018)") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0) +
  theme_void()

# bottom teams pie chart
b10$count <- 1
b10_counts <- b10 %>%
  group_by(team) %>%
  summarise_at(vars(count), list(count = sum))

ggplot(b10_counts, aes(x = "", y = count, fill = reorder(team, count))) + 
  geom_bar(width = 1, stat = "identity", color = "black") + 
  labs(title = "Bottom Teams Number of Seasons in the Premier League (2006-2018)") +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0) +
  theme_void()

#spend by top teams
t10_spend <- t10 %>%
  group_by(team) %>%
  summarise_at(vars(spend), list(spend = sum))

ggplot(t10_spend, aes(x = reorder(team, -spend), y = spend, fill = team)) + 
  geom_bar(width = 1, stat = "identity", color = "white") + 
  labs(title = "Top Teams Spend (Millions)") +
  geom_text(aes(label = spend), vjust = -.3, color = "white", size = 3.5) +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_hline(yintercept = 847.4908, color = "white")

mean(t10_spend$spend)

#spend by bottom teams
b10_spend <- b10 %>%
  group_by(team) %>%
  summarise_at(vars(spend), list(spend = sum))

ggplot(b10_spend, aes(x = reorder(team, -spend), y = spend, fill = team)) + 
  geom_bar(width = 1, stat = "identity", color = "white") + 
  labs(title = "Bottom Teams Spend (Millions)") +
  geom_text(aes(label = spend), vjust = -.3, color = "white", size = 3.5) +
  theme_minimal() +
  theme(legend.position = "none") + 
  geom_hline(yintercept = 59.235, color = "white")

mean(b10_spend$spend)

# total spend by season
#install.packages("areaplot")
library(areaplot)
total_spend <- PL %>%
  group_by(season) %>%
  summarise_at(vars(spend), list(spend = sum))

t10_total_spend <- t10 %>%
  group_by(season) %>%
  summarise_at(vars(spend), list(spend = sum))

b10_total_spend <- b10 %>%
  group_by(season) %>%
  summarise_at(vars(spend), list(spend = sum))
# adding 0 observation to create compiled data
adding <- data.frame("2008-2009", 0.00)
names(adding) <- c("season", "spend")
b10_total_spend <- rbind(b10_total_spend, adding)
b10_total_spend <- b10_total_spend[order(b10_total_spend$season),]

seasons <- c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)

combined <- data.frame(seasons, total_spend$spend, b10_total_spend$spend, t10_total_spend$spend)
# total spend over time
ggplot(spend_combined, aes(x = seasons)) +
  geom_line(aes(y = total_spend.spend), size = 5, color = "blue") +
  geom_line(aes(y = b10_total_spend.spend), size = 5, color = "red") +
  geom_line(aes(y = t10_total_spend.spend), size = 5, color = "green") +
  xlab("Year") +
  ylab("Money Spend") +
  xlim(2007,2018) +
  ylim(0,2500)

# wins by season
total_wins <- PL %>%
  group_by(season) %>%
  summarise_at(vars(wins), list(wins = sum))

t10_wins <- t10 %>%
  group_by(season) %>%
  summarise_at(vars(wins), list(wins = sum))

b10_wins <- b10 %>%
  group_by(season) %>%
  summarise_at(vars(wins), list(wins = sum))
# adding 0 observation to create compiled data
adding <- data.frame("2008-2009", 0)
names(adding) <- c("season", "wins")
b10_wins <- rbind(b10_wins, adding)
b10_wins <- b10_wins[order(b10_wins$season),]

combined_wins <- data.frame(seasons, total_wins$wins, b10_wins$wins, t10_wins$wins)
# total spend over time
ggplot(combined_wins, aes(x = seasons)) +
  geom_line(aes(y = total_wins.wins), size = 3, color = "blue") +
  geom_line(aes(y = b10_wins.wins), size = 3, color = "red") +
  geom_line(aes(y = t10_wins.wins), size = 3, color = "green") +
  xlab("Year") +
  ylab("Wins") +
  xlim(2007,2018) +
  ylim(0,350)

# price per win

price_win <- PL %>%
  group_by(team) %>%
  summarise_at(vars(spend, wins), sum)

price_win$price_per_win <- price_win$spend/price_win$wins

t10_price <- t10 %>%
  group_by(team) %>%
  summarise_at(vars(spend, wins), sum)
t10_price$price_per_win <- t10_price$spend/t10_price$wins

b10_price <- b10 %>%
  group_by(team) %>%
  summarise_at(vars(spend, wins), sum)
b10_wins$price_per_win <- b10_wins$spend/b10_wins$wins

price_win$lat <- c( 50.735278, 51.555, 52.509167, 52.475833, 53.728611, 53.804722, 53.580556, 50.861822
                    , 53.789167, 51.472778, 51.486389, 51.481667, 51.398333, 52.915, 53.438889, 51.475
                    , 53.654167, 53.746111, 52.623611, 53.430833, 53.483056, 53.463056, 54.564167, 54.975556
                    , 52.622222, 50.796389, 51.509167, 51.422222, 53.370278, 50.914722, 52.988333, 54.914444
                    , 51.642778, 51.604722, 51.65, 52.509167, 51.538611, 53.5475, 52.590278)
price_win$lon <- c(-1.838333, -0.108611,  -1.884722, -1.868056, -2.489167, -3.048056, -2.535556, -0.083278
                   , -2.230278, -3.203056, 0.036389, -0.191111, -0.085556, - 1.447222, -2.966389, -0.221667
                   , -1.768333, -0.3675, -1.140556, -2.960833, -2.200278, -2.291389, -1.246944, -1.621667
                   , 1.309167, -1.063889, -0.232222, -0.982778, -1.470833, -1.413056, -2.175556, -1.388333
                   , -3.934722, -0.066389, -0.401667,  -1.963889, -0.016389, -2.654167, -2.130278)


#building the map
library(maps)
library(mapproj)
library(ggrepel)

UK <- map_data("world") %>% filter(region == "UK")
ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=price_win %>% arrange(wins) %>% tail(6), aes(x=lon, y=lat, size=price_per_win, color=wins)) +
  scale_size_continuous(range=c(1,12)) +
  theme_void() + ylim(50,59) + coord_map() 
  geom_text_repel( data=price_win %>% arrange(wins) %>% tail(8), aes(x=lon, y=lat, label=team), size=5) 
  #geom_point( data=price_win %>% arrange(wins) %>% tail(10), aes(x=lon, y=lat), color="red", size=3)
ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=price_win %>% arrange(wins) %>% head(12), aes(x=lon, y=lat, size=price_per_win, color=wins)) +
  scale_size_continuous(range=c(1,12)) +
  theme_void() + ylim(50,59) + coord_map() +
  geom_text_repel( data=price_win %>% arrange(wins) %>% head(12), aes(x=lon, y=lat, label=team), size=5)





















