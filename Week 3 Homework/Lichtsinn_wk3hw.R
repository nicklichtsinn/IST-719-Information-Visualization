# Author Nicholas Lichtsinn
# Purpose Homework Week 3
####################################

#importing the premier league dataset
PL <- read.csv("c:/Users/nickl/Documents/Syracuse/IST 719 - Information Visualization/Homeworks/Week 3 Homework/stats.csv")

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
pie(table, labels = labels, main = "Number of Seasons Each Team Was in the Premier League (2006-2018)", col = 1:20, border = "white", order = TRUE)
mtext('https://www.kaggle.com/datasets/zaeemnalla/premier-league?resource=download', side=1, line=1, at=1)

# Plot 3
hist(PL$goals_conceded, breaks = 5, col = "green", main = "Frequency of Goals Conceded per Team per Season", xlab = "Goals Conceded", ylab = "Frequency", density = 20)
mtext('https://www.kaggle.com/datasets/zaeemnalla/premier-league?resource=download', side=1, line=4, at=100)

# Multi dimensional plots
# Plot 3
grouped <- tapply(PL$goals, list(PL$season), mean)
barplot(grouped, main = "Average Total Premier League Goals per Season", xlab = "Season", ylab = "Goals", col = c(1:12), ylim = c(0, 60))
mtext('https://www.kaggle.com/datasets/zaeemnalla/premier-league?resource=download', side=1, line = 4, at = 15)

#Plot 4
t <- subset(PL, team = "Manchester United", select = c(wins, goals))
plot(t, main = "Manchester United Wins vs Goals", xlab = "Wins", ylab = "Goals Scored", col = "Red", pch = "M")
mtext('https://www.kaggle.com/datasets/zaeemnalla/premier-league?resource=download', side=1, line=4, at=25)


plot(rnorm(100), col = rgb(.3, .7, 1), pch = 16, cex = 3)

tmp <- c("2014, Aug, Fri the 16 at 18:40", "2014, Jun, Sat the 24 at 11:51", "2014, Jun, Sun the 25 at 7:22") 
strptime(tmp, "%Y, %b, %a the %d at %H:%M")

