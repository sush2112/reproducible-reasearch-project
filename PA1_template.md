# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
# Unsipping the data
unzip("E:/Backup/Jijo/Jijo/Coursera/Data Science Specialization/Reproducible Research/Project 1/RepData_PeerAssessment1/activity.zip", exdir = "E:/Backup/Jijo/Jijo/Coursera/Data Science Specialization/Reproducible Research/Project 1/RepData_PeerAssessment1" ) 

# Loading the data
activity <- read.csv("activity.csv")

#Preprocessing
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity)
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

```r
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
sumSteps <- aggregate(activity$steps ~ activity$date,FUN = sum)
colnames(sumSteps) <- c("Date","Total_Steps")
ggplot(sumSteps, aes(Total_Steps)) + geom_histogram(col = "red",bins = 8)
```

![](PA1_template_files/figure-html/mean total number of steps-1.png)<!-- -->



```r
# Mean number of steps
mean(sumSteps$Total_Steps, na.rm = TRUE) 
```

```
## [1] 10766.19
```

```r
# Median number of steps
median(sumSteps$Total_Steps,na.rm = TRUE)
```

```
## [1] 10765
```

The average mean number of steps taken each day was 10766.19 steps.

The median number of steps taken each day was 10765 steps.

# What is the average daily activity pattern?

## Time Series of averaged number of steps across different intervals


```r
meanInterval <- aggregate(steps ~ interval,data = activity, FUN = mean)
ggplot(meanInterval, aes(x = interval, y = steps)) + geom_line()
```

![](PA1_template_files/figure-html/time series-1.png)<!-- -->


## Imputing missing values
We will be imputing the missing values with the mean in each interval. 

```r
activity[is.na(activity)] <- tapply(activity$steps, activity$interval,mean,na.rm =TRUE)
summary(activity)  
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

## Histogram after imputation

```r
sumSteps2 <- aggregate(activity$steps ~ activity$date , FUN= sum)
colnames(sumSteps2) <- c("Date","Total_Steps")
ggplot(sumSteps2, aes(Total_Steps)) + geom_histogram(col = "red",bins = 8, fill = "blue")
```

![](PA1_template_files/figure-html/Histogram after imputation-1.png)<!-- -->

## Mean and median number of steps after imputation 

```r
# Mean number of steps
mean(sumSteps2$Total_Steps, na.rm = TRUE) 
```

```
## [1] 10766.19
```

```r
# Median number of steps
median(sumSteps2$Total_Steps,na.rm = TRUE)
```

```
## [1] 10766.19
```

There is no change in the mean of the values since we impputed the missing values with the mean. The median however has increased. 

## Are there differences in activity patterns between weekdays and weekends?


```r
activity$weekCategory  <- ifelse(weekdays(activity$date) %in% c("Satuday","Sunday"),"Weekend","Weekday")
weekPattern <- aggregate(activity$steps ~ activity$interval + activity$weekCategory, FUN= mean)
colnames(weekPattern) <- c("interval","weekCategory","steps")
ggplot(weekPattern,aes(x= interval, y = steps), color = weekCategory) + geom_line() +
  labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") +
       facet_wrap(~weekCategory, ncol = 1, nrow=2)
```

![](PA1_template_files/figure-html/Difference in weekend and weekday patterns-1.png)<!-- -->

During the weekdays, the person is more active in the early hours of the day and then he is having lighter motion. In the weekends, he is active throughout the daytime.
