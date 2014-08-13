# Reproducible Research: Peer Assessment 1


This report belongs to the first peer assessment of the MOOC Reproducable
Research which is held by
[Johns Hopkins Bloomberg School of Public Health](http://www.jhsph.edu/)
and delivered via [Coursera Inc.](https://www.coursera.org/).

For the whole report numbers will be presented in fixed notation (i.e.
scientific notation is globally disabled).


```r
options(scipen = 999)
```


## Loading and preprocessing the data


Before loading the data it eventually has to be extracted out of the zip file.


```r
sourceZip <- "activity.zip"
file <- "activity.csv"
if ( !file %in% dir() ) {
    message(paste("extracting", file, "out of", sourceZip))
    unzip(sourceZip, file)
} else {
    message(paste("found", file, "in working directory"))
} 
```

```
## found activity.csv in working directory
```

Now the data can be loaded and the date field converted into type Date.


```r
activities <- read.csv(file)
activities$date <- as.Date(activities$date)
summary(activities)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```


## What is mean total number of steps taken per day?


The following plot shows a histogram of the total number of steps taken each
day. NAs are being ignored.


```r
stepsPerDay <- tapply(activities$steps, activities$date, sum)
hist(stepsPerDay)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

The mean total number of steps taken per day is
**10766.19**, the median is
**10765**.


## What is the average daily activity pattern?


To illustrate the average daily activity pattern the dataset is reshaped into
a dataset with two columns with one column containing the 5-minute interval and
the other column containing the mean of all steps taken in this 5-minute
interval across all days. NAs are being ignored.


```r
library(reshape2)
activityMelt <- melt (
    data            = activities,
    na.rm           = TRUE,
    id.vars         = "interval",
    measure.vars    = "steps"
)
means <- dcast (
    data            = activityMelt,
    formula         = interval ~ variable,
    fun.aggregate   = mean
)
```

The following figure is a time series plot of the reshaped data set.


```r
plot(means, type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

On average the maximum number of steps (206) is taken
during the **835th** 5-minute interval.


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
