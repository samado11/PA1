---
title: "Reproducible Research Week 2 Assignment"
output:
  html_document:
    self_contained: no
    keep_md: true 
---



## Load the data 



```r
zipfile_url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
download.file(zipfile_url, destfile = ".\\zip_file.zip")
unzip('zip_file.zip')
file_name <- 'activity.csv'
data <- read.csv(file_name)
```


## Process the data into a format suitable for your analysis
Create new column with date as a Date object. 

```r
data$date_d <- as.Date(as.character(data$date))
```


## mean total number of steps taken per day


```r
steps_Day <- with(data, tapply(steps, date, sum, na.rm = T)) 
```
Check that there are 61 rows (days).

```r
nrow(steps_Day) 
```

```
## [1] 61
```

## Make a histogram of the total number of steps taken each day.

```r
hist(steps_Day, breaks=seq(0,22500, 2500), xaxt = 'n', xlab = "Total Steps Taken", 
     ylab = "Number of Days", ylim = range(seq(0,20, 5)), main = '', col = 'powderblue')
axis(side=1, at=seq(0,22500, 2500), labels=seq(0,22500, 2500))
```

![](Figures/histogram-1.png)<!-- -->

## Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps <- round(mean(steps_Day), digits = 3)
median_steps <- round(median(steps_Day), digits = 3)
mean_steps
```

```
## [1] 9354.23
```

```r
median_steps
```

```
## [1] 10395
```

## calculate the average daily activity pattern  


```r
steps_byInt <- with(data, tapply(steps, interval, mean, na.rm = T)) 
```

Check that there are 288 rows (1440 minutes in a day, split into 5 minute intervals).

```r
nrow(steps_byInt) 
```

```
## [1] 288
```

Create plot.

```r
plot(steps_byInt, type = 'l', xlab = 'Interval Number', ylab = 'Average Number of Steps Taken')
```

![](Figures/linePlot-1.png)<!-- -->

## Which 5-minute interval, on average across all the days in the dataset?

```r
max_int <- match(max(steps_byInt), steps_byInt)
max_int
```

```
## [1] 104
```
The interval with the maximum number of steps, on average, is interval 104, which corresponds to minute 835.

## Calculate and report the total number of missing values in the dataset 

```r
missing <- sum(is.na(data$steps))
missing
```

```
## [1] 2304
```
The total number of missing values is 2304.


```r
data_filled <- data
for(i in 1:nrow(data_filled)){
    if(is.na(data_filled$steps[i])){
        int <- as.character(data_filled$interval[i])
        num <- steps_byInt[[int]]
        data_filled$steps[i] <- num
    }
}
```


## Make the histogram 

```r
steps_byDay_2 <- with(data_filled, tapply(steps, date, sum)) 
```

Check that there are 61 rows (days).

```r
nrow(steps_byDay_2) 
```

```
## [1] 61
```

```r
hist(steps_byDay_2, breaks=seq(0,22500, 2500), xaxt = 'n', xlab = "Total Steps Taken", 
     ylab = "Number of Days", ylim = range(seq(0, 30, 5)), main = '', col = 'powderblue')
axis(side=1, at=seq(0,22500, 2500), labels=seq(0,22500, 2500))
```

![](Figures/histogram2-1.png)<!-- -->

## Calculate and report the mean and median total number of steps taken per day

```r
mean_steps2 <- round(mean(steps_byDay_2), digits = 3)
median_steps2 <- round(median(steps_byDay_2), digits = 3)
mean_steps2
```

```
## [1] 10766.19
```

```r
median_steps2
```

```
## [1] 10766.19
```
The mean total number of steps per day is 10766.189. The median total number of steps per day is 10766.189.


**Are there differences in activity patterns between weekdays and weekends?**  
**For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.**  

**1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**


```r
weekend <- c('Saturday','Sunday')
for(i in 1:nrow(data_filled)){
    day <- weekdays(data_filled$date_d[i])
    if(day %in% weekend){
        data_filled$day_type[i] <- 'weekend'
    } else {
        data_filled$day_type[i] <- 'weekday'
    }
}
data_filled$day_type <- as.factor(data_filled$day_type) 
```
Check new variable and data.

```r
class(data_filled$day_type)
```

```
## [1] "factor"
```

```r
str(data_filled)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ date_d  : Date, format: "2012-10-01" "2012-10-01" ...
##  $ day_type: Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

**2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).**  



```r
weekday_data <- subset(data_filled, data_filled$day_type == 'weekday')
weekday_steps_data <- with(weekday_data, tapply(steps, c(interval), mean, na.rm = T)) 

weekend_data <- subset(data_filled, data_filled$day_type == 'weekend')
weekend_steps_data <- with(weekend_data, tapply(steps, c(interval), mean, na.rm = T)) 
```

Check data; nrows should be 288.

```r
head(weekday_steps_data)
```

```
##          0          5         10         15         20         25 
## 2.25115304 0.44528302 0.17316562 0.19790356 0.09895178 1.59035639
```

```r
head(weekend_steps_data)
```

```
##           0           5          10          15          20          25 
## 0.214622642 0.042452830 0.016509434 0.018867925 0.009433962 3.511792453
```

```r
nrow(weekday_steps_data)
```

```
## [1] 288
```

```r
nrow(weekend_steps_data)
```

```
## [1] 288
```

Create plot.

```r
par(mfrow=c(1,2))
plot(weekday_steps_data, type = 'l', xlab = 'Interval Number', ylab = 'Average Number of Steps Taken', 
     main = 'Weekdays', ylim = c(0,250))
plot(weekend_steps_data, type = 'l', xlab = 'Interval Number', ylab = 'Average Number of Steps Taken', 
     main = 'Weekends',ylim = c(0,250))
```

![](Figures/panelPlot-1.png)<!-- -->
