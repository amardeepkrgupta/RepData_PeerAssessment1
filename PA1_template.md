Reproducible Research: Peer Assessment 1
=========================================

## Loading and processing the data

1. Load the data (i.e. `read.csv()`)

2. Process/transform the data (if necessary) into a format suitable for your analysis



```r
unzip("activity.zip")
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
data$month <- as.numeric(format(data$date, "%m"))
cdata <- na.omit(data)
```


## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day.


```r
totalstepspday <- tapply( cdata$steps, cdata$date, FUN = sum)
totalstepspday
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##        126      11352      12116      13294      15420      11015 
## 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##      12811       9900      10304      17382      12426      15098 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 
##      10139      15084      13452      10056      11829      10395 
## 2012-10-21 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 
##       8821      13460       8918       8355       2492       6778 
## 2012-10-27 2012-10-28 2012-10-29 2012-10-30 2012-10-31 2012-11-02 
##      10119      11458       5018       9819      15414      10600 
## 2012-11-03 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-11 
##      10571      10439       8334      12883       3219      12608 
## 2012-11-12 2012-11-13 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##      10765       7336         41       5441      14339      15110 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 
##       8841       4472      12787      20427      21194      14478 
## 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      11834      11162      13646      10183       7047
```

2. Make a histogram of the total number of steps taken each day.

```r
library(ggplot2)
ggplot(cdata, aes(date, steps)) + 
    geom_bar(stat="identity", width = 0.8) + 
    labs(title = "Histogram of the total number of steps taken each day",  
         x = "Date", y ="Total number of steps taken")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day.


```r
mean(totalstepspday)
```

```
## [1] 10766.19
```

```r
median(totalstepspday)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
avgdailypattn <- aggregate(list(steps=data$steps), list(interval = as.numeric(as.character(data$interval))), FUN="mean", na.rm=TRUE)
ggplot(avgdailypattn, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("Average number of steps taken")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?.

```r
avgdailypattn[which.max(avgdailypattn$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

```r
sum(is.na(data))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset.
* Use the mean for that 5-minute interval to fill each NA value in the steps column.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
ndata <- data 
for (i in 1:nrow(ndata)) {
    if (is.na(ndata$steps[i])) {
        ndata$steps[i] <- avgdailypattn[which(ndata$interval[i] == avgdailypattn$interval), ]$steps
    }
}

head(ndata)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
sum(is.na(ndata))
```

```
## [1] 0
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
ggplot(ndata, aes(date, steps)) + 
    geom_bar(stat="identity", width = 0.8) + 
    labs(title = "Histogram of the total no. of steps taken each day(no missing values)",  
         x = "Date", y ="Total number of steps taken")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
ntotalstepspday <- tapply( ndata$steps, ndata$date, FUN = sum)
mean(ntotalstepspday)
```

```
## [1] 10766.19
```

```r
median(ntotalstepspday)
```

```
## [1] 10766.19
```

* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?.

Mean total number of steps taken per day:

```r
newmean <- mean(ntotalstepspday)
oldmean <- mean(totalstepspday)
```
Median total number of steps taken per day:

```r
newmedian <- median(ntotalstepspday)
oldmedian <- median(totalstepspday)
```
Compare them with the two before imputing missing data:

```r
newmean - oldmean
```

```
## [1] 0
```

```r
newmedian - oldmedian
```

```
## [1] 1.188679
```
So, after imputing the missing data, the new mean of total steps taken per day is the same as that of the old mean; the new median of total steps taken per day is greater than that of the old median.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
head(ndata)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
for (i in 1:nrow(ndata)) {
    if (weekdays(ndata$date[i]) %in% c("Saturday", "Sunday")) {
        ndata$weekdays[i] <- c("weekend")
    }
    else {ndata$weekdays[i] <- c("weekday")}
}

head(ndata)
```

```
##       steps       date interval month weekdays
## 1 1.7169811 2012-10-01        0    10  weekday
## 2 0.3396226 2012-10-01        5    10  weekday
## 3 0.1320755 2012-10-01       10    10  weekday
## 4 0.1509434 2012-10-01       15    10  weekday
## 5 0.0754717 2012-10-01       20    10  weekday
## 6 2.0943396 2012-10-01       25    10  weekday
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
navgdailypattn <- aggregate(list(steps=ndata$steps), 
                            list(interval = as.numeric(as.character(ndata$interval)), 
                                 weekdays = ndata$weekdays), FUN="mean")

library(lattice)
xyplot(navgdailypattn$steps ~ navgdailypattn$interval | navgdailypattn$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

