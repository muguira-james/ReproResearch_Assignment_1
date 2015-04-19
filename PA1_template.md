
Reproducible Research: Peer Assignment 1
=======================================================

This document demonstrates a small example of how to employ 
Literate Statistical Programming. 

This assignment makes use of data from a personal activity montoring device. This device collects data in 5 minute intervals throughout the day. The data was collected from an anonymous individual during the months of October and November, 2012.

## Loading and processing the data

The processing assumes the file has been downloaded to the local directory and unzipped.

```{r}
library(lubridate)
rawd <- read.csv("activity.csv", colClasses=c("numeric", "character", "numeric"))
rawd$date <- as.Date(rawd$date, "%Y-%m-%d")
rawd$Weekday <- wday(rawd$date, label=TRUE, abbr=FALSE)
```


## What is the mean total number of steps taken per day?

```{r}
aggMean <- aggregate(steps ~ date,data=rawd, mean, na.rm=TRUE)

plot(aggMean, type="h")

```


## Show a histogram of the total number of steps per day.

```{r}
aggStep <- aggregate(steps ~ date, data=rawd, FUN=sum, na.rm=TRUE)
hist(aggStep$steps, main="Total steps per day", xlab="day")
```

Show the mean and median number of steps for the collection period and histogram the data.

```{r}
mean(aggStep$steps)
median(aggStep$steps)
```

## What is the average daily pattern.  Show a time series plot of the 5 minute interval (x axis) and the average number of steps taken (y axis)

```{r}
avgDay <- aggregate(steps ~ interval, data=rawd, mean, na.rm=TRUE)

plot(avgDay, type="l", main="Number of steps averaged over the sample", xlab="5 min interval", ylab="Average across the days")
```

### Which interval has the most steps?

```{r}
int5 <- tapply(rawd$steps, rawd$interval, mean, na.rm=TRUE)
# which 5 min interval has the max num of steps?
maxInterval <- which.max(int5)
names(maxInterval)[1]
```

## Handle Missing values

### Show the total number of missing values in the data set

total number of 'NA' values

```{r}
sum(is.na(rawd$steps))
```

Show the days with missing values

```{r}
NA.missing <- subset(rawd, !complete.cases(rawd))
table(NA.missing$date)
```

## This section will fill in the missing values in the file with two different strategies.  

The first strategy fills in a ZERO for 'NA'.  This result reduces the mean and median values and skews the histogram to the left.

The second strategy fills in the 'NA' values with the average of the day of the week.  This strategy has less impact on the data and histogram.

### Strategy 1 = fill 'NA' with zero

```{r}

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
hist(steps2$steps, main="Strategy 1: NA = 0", xlab="day")
# using zero really skewed the results.
mean(steps2$steps)
median(steps2$steps)

```

### Strategy 2 = use the average for that day of the week

```{r}
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
hist(steps2$steps, main="Strategy 2: NA = avg for that weekday", xlab="day")
# using zero really skewed the results.
mean(steps2$steps)
median(steps2$steps)
```

## Differences in activity patterns for weekday and weekend?

```{r}
library(lattice)
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

```
