# Reproducible Research: Peer Assessment 1
pvasilyev  

To reproduce results of this assessment please use following dependencies:
```r
library(plyr)
```

## Loading and preprocessing the data

Simply read .csv file and make initial data processing
```r
data <- read.csv(file = "activity.csv", 
    header = TRUE, 
    na.strings = "NA", 
    colClasses = c("numeric", "Date", "numeric")
)

# transform records like 815 into 0815 (08:15)
data$interval <- as.character(formatC(data$interval, width=4, format='d', digits=2, flag='0'))
data_no_na <- na.omit(data)
```


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day.
```r
total_steps_by_date <- aggregate(steps ~ date, data_no_na, sum)
```
2. Make a histogram of the total number of steps taken each day.
```r
hist(total_steps_by_date$steps, col = "steelblue3", 
     xlab = "Total Steps Per Day", 
     main = "Histogram of total steps per day", 
     breaks = 10
)
```
3. Calculate and report the mean and median of the total number of steps taken per day.
```r
mean_total_steps <- mean(total_steps_by_date$steps)
median_total_steps <- median(total_steps_by_date$steps)
```
Values of the variables are as follows:
```r
> mean_total_steps
[1] 10766.19
> median_total_steps
[1] 10765
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
```r
# Let's aggregate data by 'interval' column using mean() function:
mean_steps_by_interval <- aggregate(steps ~ interval, data_no_na, mean)
# Series plot:
plot(strptime(mean_steps_by_interval$interval, "%H%M"), 
    mean_steps_by_interval$steps, 
    type="l", 
    xlab = "Time Interval", 
    ylab = "Mean Steps"
)
```
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```r
> mean_steps_by_interval[which.max(mean_steps_by_interval$steps), ]
    interval    steps
104     0835 206.1698
```
Which means, that interval 08:35 is the most active.

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)
```r
> missing_row_count <- nrow(data) - nrow(data_no_na)
> missing_row_count
[1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```r
# Let's use the mean-day strategy.
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```r
data_filled_na <- data
relaplce_with_median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
data_filled_na <- ddply(na.omit(data_no_na), ~ date, transform, steps = relaplce_with_median(steps))
total_steps_by_date_imputed <- aggregate(steps ~ date, data_filled_na, sum)
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```r
plot(steps ~ date, 
    total_steps_by_date_imputed, 
    type = "h", 
    xlab = "Date", 
    ylab = "Total Steps per day", 
    main = "Total Steps made per day"
)

mean_total_steps_imputed <- mean(total_steps_by_date_imputed$steps)
median_total_steps_imputed <- median(total_steps_by_date_imputed$steps)
> mean_total_steps_imputed
[1] 10766.19
> median_total_steps_imputed
[1] 10765
```
As we see the numbers don't differ from the ones calculated during step 2.3.


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```r
data_filled_na$weekday <- with(data_filled_na, 
    ifelse(weekdays(date) == "Saturday" | weekdays(date) == "Sunday", 
    "Weekend", 
    "Weekday")
)
data_filled_weekday <- data_filled_na[data_filled_na$weekday == "Weekday", ]
data_filled_weekend <- data_filled_na[data_filled_na$weekday != "Weekday", ]
mean_steps_by_weekday <- aggregate(steps ~ interval, data_filled_weekday, mean)
mean_steps_by_weekend <- aggregate(steps ~ interval, data_filled_weekend, mean)
```
2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```r
par(mfrow = c(2, 1))
plot(strptime(mean_steps_by_weekday$interval, "%H%M"), mean_steps_by_weekday$steps, type = "l", xlab = "Time Interval", main = "Week Day", ylab = "Total steps made", col = "blue")
plot(strptime(mean_steps_by_weekend$interval, "%H%M"), mean_steps_by_weekend$steps, type = "l", xlab = "Time Interval", main = "Week End", ylab = "Total steps made", col = "blue")
par(mfrow = c(1, 1))
```
