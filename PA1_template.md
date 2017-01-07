# Reproducible Research: Peer Assessment 1
Sudhakar Athuru  

## Loading and preprocessing the data
### read the data form the .csv file


```r
setwd("C:/Sudhakar/coursera/05_ReproducibleResearch")
actdata <- read.csv("./data/activity.csv")
head(actdata)
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
names(actdata)
```

```
## [1] "steps"    "date"     "interval"
```

```r
summary(actdata)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

### process data


### process data to aggregate steps per day

```r
steps_by_day <- aggregate(steps ~ date, actdata, sum)
head(steps_by_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

## What is mean total number of steps taken per day?
The following histogram shows number of total steps taken per day

```r
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Compute and print mean and median steps per day:

```r
smean <- as.integer(mean(steps_by_day$steps))
smedian <- as.integer(median(steps_by_day$steps))
```
Mean steps per day are 10766. Median steps per day are 10765.


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
