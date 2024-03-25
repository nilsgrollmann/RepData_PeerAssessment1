---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
activity_data <- read.csv("C:/Users/M313748/Desktop/Coursera/activity.csv", header = TRUE, stringsAsFactors = FALSE)
str(activity_data)
head(activity_data)
total_steps_per_day <- aggregate(steps ~ date, data = activity_data, FUN = sum, na.rm = TRUE)
hist(total_steps_per_day$steps, main = "Histogram of Total Steps per Day", 
     xlab = "Total Steps per Day", ylab = "Frequency", col = "lightblue")

## What is mean total number of steps taken per day?
mean_steps_per_day <- mean(total_steps_per_day$steps)
median_steps_per_day <- median(total_steps_per_day$steps)
cat("Mean steps per day:", mean_steps_per_day, "\n")
cat("Median steps per day:", median_steps_per_day, "\n")


## What is the average daily activity pattern?
average_steps_per_interval <- aggregate(steps ~ interval, data = activity_data, FUN = mean, na.rm = TRUE)
plot(average_steps_per_interval$interval, average_steps_per_interval$steps, 
     type = "l", col = "blue", xlab = "5-Minute Interval", ylab = "Average Number of Steps", 
     main = "Average Daily Activity Pattern")


## Imputing missing values
max_interval <- average_steps_per_interval[which.max(average_steps_per_interval$steps), "interval"]
max_steps <- max(average_steps_per_interval$steps)
cat("Interval with the maximum average number of steps:", max_interval, "\n")
cat("Maximum average number of steps in this interval:", max_steps, "\n")



## Are there differences in activity patterns between weekdays and weekends?
library(dplyr)
library(ggplot2)
activity_data$date <- as.Date(activity_data$date)
activity_data$day_type <- ifelse(weekdays(activity_data$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
average_steps_per_interval_day_type <- activity_data %>%
        group_by(interval, day_type) %>%
        summarise(average_steps = mean(steps, na.rm = TRUE))
ggplot(average_steps_per_interval_day_type, aes(x = interval, y = average_steps, group = day_type, color = day_type)) +
        geom_line() +
        labs(x = "5-Minute Interval", y = "Average Number of Steps", title = "Average Daily Activity Pattern by Day Type") +
        theme_minimal()
        
