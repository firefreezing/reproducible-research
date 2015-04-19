---
title: "Exploratory Data Analysis Report on an Activity Data"
author: "firefreezing"
date: "Saturday, April 18, 2015"
output: html_document
---

First load several R packages that we need for this report:

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

### 1. Loading and preprocessing the data
Load the csv data:

```r
activity <- read.csv("C:/project1/activity.csv")
```

This is a quick snapshot of the data:

```r
head(activity, 10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

We find that there are some missing values in the `steps` variable. 

Check the data format of each variable and notice that the `date` variable needs to be converted to the appropriate format.

```r
activity$date <- as.Date(activity$date)
```

### 2. What is mean total number of steps taken per day?

For this part of the analysis, we ignore the missing values in the dataset.

The total number of steps taken per day is

```r
ActByDay <- activity %>%
  group_by(date) %>%
  summarise(total = sum(steps))

ActByDay
```

```
## Source: local data frame [61 x 2]
## 
##          date total
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## ..        ...   ...
```


To see the distribution of the daily activity, plot the histogram of the total number of steps taken each day:


```r
hist(ActByDay$total, xlab = "total steps per day", breaks = 10,
     main = "The Distribution of Total Number of Steps Taken Each Day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 



The mean total number of steps taken per day is 1.0766 &times; 10<sup>4</sup> and the median is 
1.0765 &times; 10<sup>4</sup>.

### 3. What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
ActByMinIntv <- activity %>%
  group_by(interval) %>%
  summarise(ave = mean(steps, na.rm = T))
```


```r
plot(ActByMinIntv$interval, ActByMinIntv$ave, type = "l", 
     xlab = "5-minute interval", 
     ylab = "average number of steps",
     main = "Average number of steps taken across all days in each 5-minute interval")
points(x = ActByMinIntv$interval[which.max(ActByMinIntv$ave)],
       y = ActByMinIntv$ave[which.max(ActByMinIntv$ave)],
       pch = 20, col = "red")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


On average across all the days in the dataset, the interval 835 contains the maximum number of steps.


### 4. Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

First, we can calculate the total number of missing values in the dataset:


```r
sum(is.na(activity$steps) == T)
```

```
## [1] 2304
```

Next, impute the missing values. Based a quick check of the original dataset. We decide to use the median of steps within a given time interval across different days to impute the missing records that were measured in the same time interval.   


```r
tempData <- activity %>%
  group_by(interval) %>%
  mutate(dtAve = median(steps, na.rm = T))
```

3.Create a new dataset `imputeData` that equals to the original dataset but with the missing data filled in:


```r
imputeData <- activity
imputeData$steps[is.na(imputeData$steps) == T] <- tempData$dtAve[is.na(imputeData$steps) == T]
```

Make a histogram of the total number of steps taken each day:

```r
ActByDay2 <- imputeData %>%
  group_by(date) %>%
  summarise(total = sum(steps))
```


```r
hist(ActByDay2$total, xlab = "total steps per day", breaks = 10,
     main = "The Distribution of Total Number of Steps Taken Each Day")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 





The mean total number of steps taken per day, after imputing the missing values, is 9504 and the median is 1.0395 &times; 10<sup>4</sup>. Both the mean and median get smaller than the data before imputing the missing values. 

### 5. Explore the differences in activity patterns between weekdays and weekends.

First, create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day:


```r
imputeData$wkdy <- ifelse(weekdays(imputeData$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

head(imputeData)
```

```
##   steps       date interval    wkdy
## 1     0 2012-10-01        0 weekday
## 2     0 2012-10-01        5 weekday
## 3     0 2012-10-01       10 weekday
## 4     0 2012-10-01       15 weekday
## 5     0 2012-10-01       20 weekday
## 6     0 2012-10-01       25 weekday
```

Next, make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
ActByMinIntv.weekday <- imputeData[imputeData$wkdy == "weekday", ] %>%
  group_by(interval) %>%
  summarise(ave = mean(steps, na.rm = T))
```


```r
ActByMinIntv.weekend <- imputeData[imputeData$wkdy == "weekend", ] %>%
  group_by(interval) %>%
  summarise(ave = mean(steps, na.rm = T))
```


```r
par(mfrow = c(2,1), mar = c(4,4,1.5,1))
with(ActByMinIntv.weekday,
     plot(interval, ave, type = "l", 
     xlab = "5-minute interval", 
     ylab = "average number of steps",
     main = "Weekday"))

with(ActByMinIntv.weekend,
     plot(interval, ave, type = "l", 
     xlab = "5-minute interval", 
     ylab = "average number of steps",
     main = "Weekend"))
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20.png) 
