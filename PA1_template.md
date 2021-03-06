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

The mean and median total number of steps taken per day is calculated as
follows.


```r
library(xtable)
averages <- data.frame (
    Type = c (
        "mean",
        "median"
    ),
    Value = c (
        mean(stepsPerDay, na.rm = TRUE),
        median(stepsPerDay, na.rm = TRUE)
    )
)
xtAverages <- xtable(averages)
print(xtAverages, type = "html")
```

<!-- html table generated in R 3.0.3 by xtable 1.7-3 package -->
<!-- Fri Aug 15 17:38:05 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Type </TH> <TH> Value </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> mean </TD> <TD align="right"> 10766.19 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> median </TD> <TD align="right"> 10765.00 </TD> </TR>
   </TABLE>


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

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


```r
maxMean <- which.max(means$steps)
maxMeanInterval <- means[maxMean, "interval"]
maxMeanInterval
```

```
## [1] 835
```

On average the maximum number of steps (206) is taken
during the **835th** 5-minute interval.


## Imputing missing values


The source dataset contains several rows with missing values.


```r
nas <- data.frame (
    Field.Name = c (
        "steps",
        "date",
        "interval"
    ),
    NA.Count = c (
        sum(is.na(activities$steps)),
        sum(is.na(activities$date)),
        sum(is.na(activities$interval))
    )
)
xtNas <- xtable(nas)
print(xtNas, type = "html")
```

<!-- html table generated in R 3.0.3 by xtable 1.7-3 package -->
<!-- Fri Aug 15 17:38:05 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Field.Name </TH> <TH> NA.Count </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> steps </TD> <TD align="right"> 2304 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> date </TD> <TD align="right">   0 </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> interval </TD> <TD align="right">   0 </TD> </TR>
   </TABLE>

Only the steps field contains missing values. These will be be imputed with the
mean for that particular 5-minute interval across all rows without NAs. The
source code for that transformation is based on a [Stackoverflow post](http://stackoverflow.com/questions/9322773/how-to-replace-na-with-mean-by-subset-in-r-impute-with-plyr).


```r
library(plyr)
impute.mean <- function(x) {
    replace(x, is.na(x), mean(x, na.rm = TRUE))
}
imputedActivities <- ddply (
    activities,
    ~ interval,
    transform,
    steps = impute.mean(steps)
)
summary(imputedActivities)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 27.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355
```

In order to analyze the effect of the imputation the following figure shows a
comparison of histograms of the total number of steps taken each day for the
original and the imputed datasets.


```r
stepsPerDayImputed <- tapply(imputedActivities$steps, imputedActivities$date, sum)
par(mfcol = c(1, 2))
hist (
    stepsPerDay,
    main = "Original data",
    xlab = "Steps per Day",
    ylim = c(0, 35)
)
hist (
    stepsPerDayImputed,
    main = "Imputed data",
    xlab = "Steps per Day",
    ylim = c(0, 35)
)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

Both plots look quite simmilar, except the middle chunk which has a higher
frequency in the imputed dataset. The mean and median total number of steps
taken per day for the imputed dataset is calculated and compared to the
averages of the original dataset as follows.


```r
averagesImputed <- data.frame (
    Type = c (
        "mean",
        "median"
    ),
    Value.Original = averages$Value,
    Value.Imputed = c (
        mean(stepsPerDayImputed, na.rm = TRUE),
        median(stepsPerDayImputed, na.rm = TRUE)
    )
)
xtAveragesImputed <- xtable(averagesImputed)
print(xtAveragesImputed, type = "html")
```

<!-- html table generated in R 3.0.3 by xtable 1.7-3 package -->
<!-- Fri Aug 15 17:38:05 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Type </TH> <TH> Value.Original </TH> <TH> Value.Imputed </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> mean </TD> <TD align="right"> 10766.19 </TD> <TD align="right"> 10766.19 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD> median </TD> <TD align="right"> 10765.00 </TD> <TD align="right"> 10766.19 </TD> </TR>
   </TABLE>

Whereas the mean is the same for both datasets, the median for the imputed
dataset is higher than the median for the original dataset, in fact it
happens to be the same as the mean.


## Are there differences in activity patterns between weekdays and weekends?

In order to make date functions (like weekdays()) return the same values
regardless of the locality they are called in the locale is set to en_US.UTF-8.


```r
Sys.setlocale("LC_ALL", "en_US.UTF-8")
```

```
## [1] "en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/de_DE.UTF-8"
```

Now a new factor variable typeOfDay can be created in the imputed dataset. It
contains two levels: weekday and weekend.


```r
imputedActivities$typeOfDay <- sapply (
    X   = imputedActivities$date,
    FUN = function(x) {
        if (weekdays(x, abbreviate = TRUE) %in% c("Sat", "Sun"))
            "weekend"
        else
            "weekday"
    }
)
imputedActivities$typeOfDay <- as.factor(imputedActivities$typeOfDay)
str(imputedActivities)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps    : num  1.72 0 0 47 0 ...
##  $ date     : Date, format: "2012-10-01" "2012-10-02" ...
##  $ interval : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ typeOfDay: Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 2 2 1 1 1 ...
```

The following figure displays two time series plots of the 5-minute interval and
the mean number of steps averaged across the type of day.


```r
library(ggplot2)
meansPerIntervalAndTypeOfDay <- aggregate (
    formula = steps ~ interval + typeOfDay,
    data    = imputedActivities,
    FUN     = mean
)
g <- ggplot(meansPerIntervalAndTypeOfDay) + aes(interval, steps) + geom_line()
g <- g + facet_grid(typeOfDay ~ .) + xlab("Interval") + ylab("Number of Steps")
print(g)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 
