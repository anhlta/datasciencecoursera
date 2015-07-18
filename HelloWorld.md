# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
1. Load the data
```r
d$date <-  as.Date(d$date, format = "%Y-%m-%d")
str(d)
```
2. Convert data column to Date type: 
```r
d$date <-  as.Date(d$date, format = "%Y-%m-%d")
str(d)
```
```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?
1. Calculate total number of steps taken each day

```r
spd <- tapply(d$steps, d$date, sum, na.rm = TRUE)
head(spd)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420
```

2. Plot histogram of the total number of steps taken each day

```r
hist(spd,  main = "Total # of steps taken per day", xlab = "# of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

3. Calculate mean and median total number of steps taken per day

```r
mean(spd)
```

```
## [1] 9354.23
```

```r
median(spd)
```

```
## [1] 10395
```
## What is the average daily activity pattern?
1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
sp5m <- tapply(d$steps, d$interval, mean, na.rm = TRUE)
plot(names(sp5m), sp5m, type="l", ylab = "# of steps", xlab = "Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

2. Use which.max function to get the 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

```r
names(which.max(sp5m))
```

```
## [1] "835"
```

## Imputing missing values
Since there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Total number of missing values in the dataset 

```r
sum(is.na(d$steps))
```

```
## [1] 2304
```

2. We fill in all of the missing values with the mean for that 5-minute interval (from sp5m: we have calculated above)

```r
for (i in 1:nrow(d)){ if (is.na(d[i,1])){ d[i,1] <- sp5m[as.character(d[i,3])]; } }
```

3. Now all missing values are filled.

```r
head(d)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

4. Re-calculate total number of steps taken each day and plot new histogram 

```r
spd2 <- tapply(d$steps, d$date, sum)
hist(spd2,  main = "Total # of steps taken per day (removed NAs)", xlab = "# of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

5. Re-calculate mean and median

```r
mean(spd2)
```

```
## [1] 10766.19
```

```r
median(spd2)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
day_type <- ifelse(weekdays(d$date) %in% c("Sunday","Saturday") ,"weekend", "weekday")
head(day_type)
```

```
## [1] "weekday" "weekday" "weekday" "weekday" "weekday" "weekday"
```

2. Calculate 5-minute interval with day_type as factor

```r
sp5m2 <- setNames(aggregate(d$step ~ d$interval + day_type, data = d, mean), c("Interval", "DayType", "Steps"))
sp5m2 <- transform(sp5m2, DayType = factor(DayType))
```

3. Show a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
library(lattice)
xyplot(data = sp5m2, Steps ~ Interval | DayType, layout = c(1,2), type = "l", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png) 
