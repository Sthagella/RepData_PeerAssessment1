---
title: 'Reproducible Research: Peer Assessment 1'
output:
  word_document: default
  html_document: default
---

* The questions and the following answers of the assignment are listed in the commented sections of the code

* Setting the directory to where the file was downloaded
getwd("/../..")
setwd("/../..")

```{r echo = TRUE}
# Loading and preprocessing the data
# Reading the file and transforming the date field to be a factor and then sum steps by date
library(dplyr)
library(lattice)
act_data <- read.csv("activity.csv")
head(act_data)
st_data <- group_by(act_data, dt = as.factor(date))

#What is the mean total number of steps taken per day

#Total number of steps taken per day
steps_by_day <- summarise(st_data, st = sum(steps, na.rm = TRUE))

#Histogram of sum of steps by date
hist(steps_by_day$st, xlab = "Number of Steps", ylab = "Number of Days", main = "Total Number of Steps Taken Per Day")

#Used data in steps_by_day to get the mean and median
steps_sum <- summarise(steps_by_day, st_mean = mean(steps_by_day$st), st_median = median(steps_by_day$st))
steps_sum

#What is the average daily activity pattern

#Transforming the interval field to be a factor and then took the mean of steps by interval which spans multiple dates
intdata <- group_by(act_data, int = as.factor(interval))
intstdata <- summarise(intdata, Avg_Daily_Steps = mean(steps, na.rm = TRUE))

#Converting the mean steps data to time series data
stseries <- ts(intstdata)

#5-minute interval across all days that contains the maximum number of steps
stseries_frame <- as.data.frame(stseries)
head(stseries_frame)

#Plot of interval and Avg Daily Steps
plot(stseries_frame, xlab = "5 Min Intervals", main = "Avg Daily Steps in 5 min Intervals", type = "l")

#Getting interval with max avg daily steps
maxst <- max(stseries_frame$Avg_Daily_Steps)
max_num_steps <- filter(stseries_frame, Avg_Daily_Steps == maxst)
max_num_steps

#The 104th interval has the max avg daily steps (206.2). This interval corresponds to 830.

#Imputing Missing Values

#Total number of rows with NAs
narows <- subset(act_data, is.na(act_data$steps))
head(narows)
count(narows)

#Imputing the data and replacing the NAs with means by interval
narows1 <- which(is.na(act_data$steps))
mean_steps <- rep(mean(act_data$steps, na.rm = TRUE), times = length(narows1))
act_data[narows1, "steps"] <- mean_steps
rm(mean_steps, narows1)

#Modified data set
head(act_data)

#New data set and histograme of new data
st_data_new <- group_by(act_data, dt_new = as.factor(date))
steps_by_day_new <- summarise(st_data_new, st_new = sum(steps))
hist(steps_by_day_new$st_new, xlab = "Number of Steps", ylab = "Number of Days", main = "Total Number of Steps Taken Per Day After Replacing NAs")

#Mean and median of the new data set
steps_sum_new <- summarise(steps_by_day_new, st_mean_new = mean(steps_by_day_new$st_new, na.rm = TRUE), st_median_new = median(steps_by_day_new$st_new, na.rm = TRUE))
steps_sum_new

# Yes the values differ. The impact is that the means and medians are larger now as more data has been added

#Are there differences in activity patterns between weekdays and weekends?

#The new data set with filled in values is act_data which has been modified in the steps above
#Weekdays creation
wd_data <- data.frame(date = as.Date(act_data$date, format = "%m/%d/%y"), steps = act_data$steps, interval = act_data$interval)
wd_data1 <- data.frame(date = wd_data$date, steps = wd_data$steps, interval = wd_data$interval, wkday = weekdays(wd_data$date))
wd_data1$wkday <- ifelse(weekdays(wd_data1$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
wkd_agg <- aggregate(steps ~ interval + wkday, data = wd_data1, mean)

#Time series plot by weekend and weekdays
xyplot(steps ~ interval | wkday, data = wkd_agg, type = "l", lwd = 2, layout = c(1,2), xlab = "Number of 5-min Intervals", ylab = "Avg Number of Steps", main = "Avg Num of Steps during weekdays and weekends")
```
