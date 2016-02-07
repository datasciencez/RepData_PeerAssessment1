---
title: "Assignment:Course Project 1"
output: html_document
---

This is an R Markdown file that describes the analysis of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


First, we load the lattice library. This library is required for xyplot to work

```r
library(lattice)
```

##Reading-in data and pre-processing
Read the activity.csv using read.csv() and inspect a sample of the data using head(). Then convert the date column to date class using as.Date().

```r
activitydata <- read.csv("activity.csv")
head(activitydata)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
activitydata$date <- as.Date(activitydata$date, "%Y-%m-%d")
```

##Histogram of steps taken each day - mean and median
Split the data by date using tapply(), plot a histogram using hist() and calculate mean and median.


```r
totalsteps <- tapply(activitydata$steps, activitydata$date, sum)
hist(totalsteps, col = "grey", xlab = "Total Steps taken each day", ylab = "Frequency", 
    main = "Histogram of Total Steps taken each day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-3-1.png" title="" alt="" width="672" />

```r
mean(totalsteps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(totalsteps, na.rm = TRUE)
```

```
## [1] 10765
```

##Time series plot of the average number of steps taken
Split the data by interval. Next,  plot the 5-minute interval time series "type="1" plot of the average number of steps taken, averaged across all days.


```r
meansteps <- tapply(activitydata$steps, activitydata$interval, mean, na.rm = TRUE)
plot(row.names(meansteps), meansteps, type = "l", xlab = "Time Intervals (5-minute)", 
    ylab = "Mean number of steps taken", main = "Average Steps Taken at 5 minute Intervals", col = "grey")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-4-1.png" title="" alt="" width="672" />

##Interval that contains the most amount of steps


```r
interval_num <- which.max(meansteps)
interval_max_steps <- names(interval_num)
interval_max_steps
```

```
## [1] "835"
```

##Strategy for imputing missing data
Compute the number of NA values in the activity dataset


```r
num_na_values <- sum(is.na(activitydata))
num_na_values
```

```
## [1] 2304
```
Fill in missing values using the average interval value across all days

```r
na_indices <- which(is.na(activitydata))
imputed_values <- meansteps[as.character(activitydata[na_indices, 3])]
names(imputed_values) <- na_indices
for (i in na_indices) {
    activitydata$steps[i] = imputed_values[as.character(i)]
}
sum(is.na(activitydata))
```

```
## [1] 0
```

Create a histogram of the total number of steps taken each day after imputing the missing values.

```r
totalsteps <- tapply(activitydata$steps, activitydata$date, sum)
hist(totalsteps, col = "grey", xlab = "Total Steps per Day", ylab = "Frequency", 
    main = "Histogram of Total Steps taken per day")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-8-1.png" title="" alt="" width="672" />

##comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Create a new factor variable with two levels - one for weekdays and another for weekends. Next, construct a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
days <- weekdays(activitydata$date)
activitydata$day_type <- ifelse(days == "Saturday" | days == "Sunday", "Weekend", 
    "Weekday")
meansteps <- aggregate(activitydata$steps, by = list(activitydata$interval, activitydata$day_type), 
    mean)
names(meansteps) <- c("interval", "day_type", "steps")
xyplot(steps ~ interval | day_type, meansteps, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps", col=c("grey"))
```

<img src="PA1_template_files/figure-html/unnamed-chunk-9-1.png" title="" alt="" width="672" />

