data <- read.csv(file = "activity.csv", header = TRUE, na.strings = "NA", colClasses = c("numeric", "Date", "numeric"))
data$interval <- as.character(formatC(data$interval, width=4, format='d', digits=2, flag='0'))
data_no_na <- na.omit(data)

total_steps_by_date <- aggregate(steps ~ date, data_no_na, sum)
hist(total_steps_by_date$steps, col = "steelblue3", 
     xlab = "Total Steps Per Day", main = "Histogram of total steps per day", breaks = 10)
mean_total_steps <- mean(total_steps_by_date$steps)
median_total_steps <- median(total_steps_by_date$steps)

mean_steps_by_interval <- aggregate(steps ~ interval, data_no_na, mean)
plot(strptime(mean_steps_by_interval$interval, "%H%M"), mean_steps_by_interval$steps, type="l", xlab = "Time Interval", ylab = "Mean Steps")
mean_steps_by_interval[which.max(mean_steps_by_interval$steps),]

missing_row_count <- nrow(data) - nrow(data_no_na)
data_filled_na <- data
library(plyr)
relaplce_with_median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
data_filled_na <- ddply(na.omit(data_no_na), ~ date, transform, steps = relaplce_with_median(steps))
total_steps_by_date_imputed <- aggregate(steps ~ date, data_filled_na, sum)
plot(steps ~ date, total_steps_by_date_imputed, type = "h", xlab = "Date", ylab = "Total Steps per day", main = "Total Steps made per day")

mean_total_steps_imputed <- mean(total_steps_by_date_imputed$steps)
median_total_steps_imputed <- median(total_steps_by_date_imputed$steps)

data_filled_na$weekday <- with(data_filled_na, ifelse(weekdays(date) == "Saturday" | weekdays(date) == "Sunday", "Weekend", "Weekday"))
data_filled_weekday <- data_filled_na[data_filled_na$weekday == "Weekday", ]
data_filled_weekend <- data_filled_na[data_filled_na$weekday != "Weekday", ]
mean_steps_by_weekday <- aggregate(steps ~ interval, data_filled_weekday, mean)
mean_steps_by_weekend <- aggregate(steps ~ interval, data_filled_weekend, mean)

par(mfrow = c(2, 1))
plot(strptime(mean_steps_by_weekday$interval, "%H%M"), mean_steps_by_weekday$steps, type = "l", xlab = "Time Interval", main = "Week Day", ylab = "Total steps made", col = "blue")
plot(strptime(mean_steps_by_weekend$interval, "%H%M"), mean_steps_by_weekend$steps, type = "l", xlab = "Time Interval", main = "Week End", ylab = "Total steps made", col = "blue")
par(mfrow = c(1, 1))
