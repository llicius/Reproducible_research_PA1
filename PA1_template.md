Reproducible Research
=====================

## Loading and preprocessing the data

The data is stored in a csv file and the working directory is set to the file location.

```r
activity <- read.csv("~/Desktop/COURSERA R/Reproducible Research/activity.csv", stringsAsFactors=TRUE)
```
---
# What is mean total number of steps taken per day?
---
Histogram of the total number of steps taken each day:
---

```r
activity$steps<-as.integer(activity$steps)
hist(with(activity, tapply(steps, list(date), sum)),breaks=61)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 
Mean of steps taken each day:
---

```r
tapply(activity$steps, activity$date,mean)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA     0.4375    39.4167    42.0694    46.1597    53.5417 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##    38.2465         NA    44.4826    34.3750    35.7778    60.3542 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##    43.1458    52.4236    35.2049    52.3750    46.7083    34.9167 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##    41.0729    36.0938    30.6285    46.7361    30.9653    29.0104 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##     8.6528    23.5347    35.1354    39.7847    17.4236    34.0938 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##    53.5208         NA    36.8056    36.7049         NA    36.2465 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##    28.9375    44.7326    11.1771         NA         NA    43.7778 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##    37.3785    25.4722         NA     0.1424    18.8924    49.7882 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##    52.4653    30.6979    15.5278    44.3993    70.9271    73.5903 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##    50.2708    41.0903    38.7569    47.3819    35.3576    24.4688 
## 2012-11-30 
##         NA
```

```r
activityNoNA<-activity[complete.cases(activity),]
meanNA<-tapply(activityNoNA$steps, activityNoNA$date,mean)
totalmeanwithNA<-mean(meanNA[complete.cases(meanNA)])
totalmeanwithNA
```

```
## [1] 37.38
```
Median number of steps taken each day:
---

```r
medianNA<-tapply(activityNoNA$steps, activityNoNA$date,median)
totalmedianwithNA<-median(medianNA[complete.cases(medianNA)])
totalmedianwithNA
```

```
## [1] 0
```
# What is the average daily activity pattern?
---
Time series plot of the average number of steps taken (averaged across all days) versus the 5-minute intervals (5-minute interval (x-axis),average number of steps taken, averaged across all days (y-axis)):
---

```r
activityNoNA$interval<-as.factor(activityNoNA$interval)
activityNoNA$averageacrossdays<-tapply(activityNoNA$steps, activityNoNA$interval,mean)
plot(activityNoNA$interval,activityNoNA$averageacrossdays, type="l", xlab="5-minute interval", ylab="number of steps averaged across all days", col="green", lwd=2)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
5-minute interval that, on average, contains the maximum number of steps:
---

```r
activityNoNA$interval[which.max(activityNoNA$averageacrossdays)]
```

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```
# Imputing missing values
----
Total number of missing values in the dataset (i.e. the total number of rows with NAs)
---

```r
nrow(activity)- nrow(activityNoNA)
```

```
## [1] 2304
```
# Strategy for imputing missing data:
Replace each NA with the average number of steps taken across all days for the specific interval. 
actReplNA is a new dataset thaat is equal to the original data but with the missing data filled in.
---

```r
actReplNA<-activity
actReplNA$interval<- as.factor(actReplNA$interval)
actReplNA$steps<-ifelse(is.na(actReplNA$steps),activityNoNA$averageacrossdays,actReplNA$steps)
summary(is.na(actReplNA$steps))
```

```
##    Mode   FALSE    NA's 
## logical   17568       0
```
Histogram of the total number of steps taken each day after missing values were imputed:
---

```r
hist(with(actReplNA, tapply(steps, list(date), sum)),breaks=61)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 
Calculate and report the mean total number of steps taken per day:
---

```r
meanReplNA<-tapply(actReplNA$steps, actReplNA$date,mean)
totalmeanReplNA<-mean(meanReplNA)
totalmeanReplNA
```

```
## [1] 37.38
```
Calculate and report the median total number of steps taken per day:
---

```r
medianReplNA<-tapply(actReplNA$steps, actReplNA$date,median)
totalmedianReplNA<-median(medianReplNA)
totalmedianReplNA
```

```
## [1] 0
```

Do these values differ from the estimates from the first part of the assignment? 
---

```r
DifferMean<-meanReplNA-meanNA
DifferMean
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA          0          0          0          0          0 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##          0         NA          0          0          0          0 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##          0          0          0          0          0          0 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##          0          0          0          0          0          0 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##          0          0          0          0          0          0 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##          0         NA          0          0         NA          0 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##          0          0          0         NA         NA          0 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##          0          0         NA          0          0          0 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##          0          0          0          0          0          0 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##          0          0          0          0          0          0 
## 2012-11-30 
##         NA
```

```r
DifferMedian<-medianReplNA-medianNA
DifferMedian
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA          0          0          0          0          0 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##          0         NA          0          0          0          0 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##          0          0          0          0          0          0 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##          0          0          0          0          0          0 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##          0          0          0          0          0          0 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##          0         NA          0          0         NA          0 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##          0          0          0         NA         NA          0 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##          0          0         NA          0          0          0 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##          0          0          0          0          0          0 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##          0          0          0          0          0          0 
## 2012-11-30 
##         NA
```
No differences were observed by replacing NA's in mean or median. But it was possible to estimate the total daily number of steps for the dates without steps data:
---

```r
sumwithNA<-tapply(activityNoNA$steps, activityNoNA$date,sum)
sumwithNA
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015         NA      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414         NA      10600      10571         NA      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219         NA         NA      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336         NA         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##         NA
```

```r
sumReplNA<-tapply(actReplNA$steps, actReplNA$date,sum)
sumReplNA
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##      10766        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015      10766      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414      10766      10600      10571      10766      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219      10766      10766      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336      10766         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##      10766
```
---
# Are there differences in activity patterns between weekdays and weekends?
---
Yes, there are:

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends:
---

```r
actReplNA$daytype<-weekdays(as.Date(actReplNA$date))
actReplNA$daytype<-as.factor(actReplNA$daytype)
actReplNA$day<-factor(c("Weekday","Weekend"))
for (i in 1:17568)
        {
        daytype <- actReplNA$daytype[i]
        
        if ((daytype =="sábado") | (daytype=="domingo"))
                {
                actReplNA$day[i]<-"Weekend"
                }
        else 
                {
                actReplNA$day[i]<-"Weekday"
                 }
      }  
dataW<-split(actReplNA, actReplNA$day)
Weekday<-(dataW[[1]])
Weekend<-(dataW[[2]])
Weekday$averageacross<-tapply(Weekday$steps, Weekday$interval,mean)

Weekend$averageacross<-tapply(Weekend$steps, Weekend$interval,mean)
Data<-rbind(Weekday,Weekend)

library(lattice)
xyplot(averageacross~interval | day, data=Data, layout=c(1,2), type="l", xlab="5-minute interval", ylab="number of steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 
