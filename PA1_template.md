# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
1. Activity file is read in.

```r
activity <- read.csv("activity.csv")
```
2. Intervals are transformed to factors  

```r
activity$interval <- as.factor(activity$interval)
```

## What is mean total number of steps taken per day?
1. Calculate and display total number of steps for each day

```r
totalSteps <- with(activity, tapply(steps, date, sum))
hist(totalSteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
  
2. Mean number of steps: 

```r
mean(totalSteps,na.rm=TRUE)
```

```
## [1] 10766.19
```
  
and median number of steps: 

```r
median(totalSteps,na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Calculate the mean for each interval, then plot that time series 

```r
meanInterval <- with(activity, tapply(steps, interval, mean, na.rm=TRUE))
plot(meanInterval,type="l", main="Mean steps across days within 5 minute intervals", xlab="Interval",ylab="Mean steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
  
2. The interval with the maximum number of steps:  

```r
names(which(meanInterval==max(meanInterval)))
```

```
## [1] "835"
```
  
## Imputing missing values
1. The total number of missing values across the dataset: 

```r
sum(is.na(activity))
```

```
## [1] 2304
```
  
2. The strategy for filling in the missing values across the dataest is to fill in NAs with the median value for that interval
  
3. Creating a new dataset with the NAs filled in with the median across the interval

```r
medianInterval <- with(activity, tapply(steps, interval, median, na.rm=TRUE))
activity$steps <- with(activity, mapply(function(x,y) {ifelse(is.na(x),y, x)},x= steps, y=rep(medianInterval,61)))
```
4. Histogram of the total steps taken each day

```r
totalSteps <- with(activity, tapply(steps, date, sum))
hist(totalSteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
  
Mean steps across days:

```r
mean(totalSteps,na.rm=TRUE)
```

```
## [1] 9503.869
```
  
Median steps across days:

```r
median(totalSteps,na.rm=TRUE)
```

```
## [1] 10395
```

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor for whether the day is a weekend or weekday

```r
dayOfWeek <- as.factor(weekdays(strptime(activity$date,"%Y-%m-%d")))
levels(dayOfWeek) <- c("weekday", "weekday", "weekend", "weekend", "weekday","weekday","weekday")
activity$dayType <- dayOfWeek
```
2. Create a two panel plot comparing average over days for intervals weekend versus weekdays

```r
library(lattice)
library(reshape2)
dayTypeSums <- as.data.frame(with(activity, tapply(steps, list(interval, dayType), mean)))
dayTypeSums<- melt(data.frame(interval=as.numeric(rownames(dayTypeSums)), dayTypeSums),id="interval")
xyplot(value~interval | variable, data=dayTypeSums,layout=c(1,2),main="Mean steps for weekend vs weekday",ylab="Number of steps",type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

