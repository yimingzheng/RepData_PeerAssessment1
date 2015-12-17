---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data   
### 1. Loading data  

```r
zipFileName <- "activity.zip"    
csvFileName <- "activity.csv"  
activity <- read.csv(unz(zipFileName, csvFileName), colClasses=c("numeric", "character","numeric")) 
```
### 2. Process/transform the data  

```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")  
good <- complete.cases(activity$steps)   
activity_complete <- activity[good,]   
```
## What is mean total number of steps taken per day?  

```r
good <- complete.cases(activity$steps)  
activity_complete <- activity[good,]
library(dplyr)
date_steps <- activity_complete %>% group_by(date) %>% summarize(sum_steps = sum(steps))
date_steps
```

```
## Source: local data frame [53 x 2]
## 
##          date sum_steps
##        (date)     (dbl)
## 1  2012-10-02       126
## 2  2012-10-03     11352
## 3  2012-10-04     12116
## 4  2012-10-05     13294
## 5  2012-10-06     15420
## 6  2012-10-07     11015
## 7  2012-10-09     12811
## 8  2012-10-10      9900
## 9  2012-10-11     10304
## 10 2012-10-12     17382
## ..        ...       ...
```

```r
hist(date_steps$sum_steps, 
     breaks=10,  
     main="Total Number of Steps Per Day",  
     xlab="Number of Steps" ,  
     ylab="Frequency" )  
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
