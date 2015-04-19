# Reproducible Research: Peer Assignment 1
# ------------------------
# load the data
library(lubridate)
rawd <- read.csv("activity.csv", colClasses=c("numeric", "character", "numeric"), header=TRUE)
names(rawd)
rawd$date <- as.Date(rawd$date, "%Y-%m-%d")
rawd$Weekday <- wday(rawd$date, label=TRUE, abbr=FALSE)

aggStep <- aggregate(steps ~ date, data = rawd, sum, na.rm= TRUE)
# need a plot here? plot of aggregate steps per day
aggMean <- aggregate(steps ~ date,data=rawd, mean, na.rm=TRUE)
# histogram of total steps each day
plot(aggMean, type="h")
# need mean and median
mean(aggStep$steps)
median(aggStep$steps)

avgDay <- aggregate(steps ~ interval, data=rawd, mean, na.rm=TRUE)
# average daily activity pattern
plot(avgDay, type="l")

int5 <- tapply(rawd$steps, rawd$interval, mean, na.rm=TRUE)
# which 5 min interval has the max num of steps?
maxInterval <- which.max(int5)
maxInterval[1]
#
NA.rawd <- subset(rawd, !complete.cases(rawd))
table(NA.rawd$date)
#
# missing values v1 - fill in a zero
fillNA <- numeric()
for(j in 1:nrow(rawd)) {
  obs <- rawd[j,]
  if (is.na(obs$steps)) {
    steps <- 0
  } else {
    steps <- obs$steps
  }
  fillNA <- c(fillNA, steps)
}

# and histogram it
sum(is.na(fillNA))
nactiv <- rawd
nactiv$steps <- fillNA
steps2 <- aggregate(steps ~ date, data=nactiv, FUN=sum)
hist(steps2$steps)
# using zero really skewed the results.
mean(steps2$steps)
median(steps2$steps)
#
# ok, filling a zero skewed things too much
#
# lets try to use the avg for a day
fillNA <- numeric()
gg <- aggregate(steps ~ Weekday, data = rawd, mean)
for(j in 1:nrow(rawd)) {
  obs <- rawd[j,]
  if (is.na(obs$steps)) {
    steps <- gg [ gg$Weekday == rawd[j,]$Weekday,]$steps
  } else {
    steps <- obs$steps
  }
  fillNA <- c(fillNA, steps)
}
# and histogram it
sum(is.na(fillNA))
nactiv <- rawd
nactiv$steps <- fillNA
steps2 <- aggregate(steps ~ date, data=nactiv, FUN=sum)
hist(steps2$steps)
# using zero really skewed the results.
mean(steps2$steps)
median(steps2$steps)
#
#
#


days <- weekdays(rawd$date)
dayOfWeek <- vector()
for (j in 1:nrow(rawd)) {
  if (days[j] == 'Saturday') {
    dayOfWeek[j] <- "Weekend"
  } else if (days[j] == 'Sunday') {
    dayOfWeek[j] <- "Weekend"
  } else {
    dayOfWeek[j] <- "Weekday"
  }
}
rawd$dayOfWeek <- factor(dayOfWeek)
stepsPerDay <- aggregate(steps ~ interval + dayOfWeek, data=rawd, mean)
xyplot(steps ~ interval | dayOfWeek, stepsPerDay, type="l", layout=c(1, 2))
