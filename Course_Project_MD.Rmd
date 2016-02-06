---
title: "Coursera Reproducible Research Project 1"
author: "Ethan Triplett"
date: "February 5, 2016"
output: html_document
---

## 1. Code for reading in the dataset and/or processing the data

```{r, echo=FALSE}
setwd("C:/Users/et86694/Desktop/Coursera/5_Reproducible_Research/Course_Project_1/Data")
```


```{r, echo=TRUE}
data <- read.csv("activity.csv", sep = ",", header = TRUE)
```

## 2. Histogram of the total number of steps taken each day

```{r, echo=TRUE}
library(dplyr)
data_by_day <- group_by(data, date)
data_by_day <- summarise(data_by_day, steps = sum(steps))
hist(data_by_day$steps)
```


## 3. Mean and median number of steps taken each day

```{r, echo=TRUE}

mean <- mean(data_by_day$steps,na.rm = TRUE)
mean

median <- median(data_by_day$steps, na.rm = TRUE)
median
```

## 4.Time series plot of the average number of steps taken

```{r, echo=TRUE}
by_interval_1 <- group_by(data,interval)
by_interval_mean <- summarise(by_interval_1, steps = mean(steps, na.rm=TRUE))
plot(by_interval_mean$interval, by_interval_mean$steps)
```


## 5.The 5-minute interval that, on average, contains the maximum number of steps

```{r, echo=TRUE}
df <- data.frame(by_interval_mean)
df[which.max(df[,2]),1]
```

## 6. Code to describe and show a strategy for imputing missing data

```{r, echo=TRUE}

## Assignment algorithm: for observations that are "NA", set them to the average number of steps in the given interval
add_interval_mean <- merge(data, by_interval_mean, by.x = "interval", by.y = "interval")
add_interval_mean$steps <- ifelse(is.na(add_interval_mean$steps.x), add_interval_mean$steps.y, add_interval_mean$steps.x)

Imputed_table <- add_interval_mean[c(3,1,5)]
```

## 7. Histogram of the total number of steps taken each day after missing values are imputed

```{r, echo=TRUE}
imputed_data_by_day <- group_by(Imputed_table, date)
imputed_data_by_day <- summarise(imputed_data_by_day, steps = sum(steps))
hist(imputed_data_by_day$steps)
```

## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r, echo=TRUE}

Imputed_table$date <- as.Date(Imputed_table$date)
Imputed_table$day_of_week <- weekdays(Imputed_table$date)
Imputed_table$day_type <- ifelse(Imputed_table$day_of_week %in% c("Saturday", "Sunday")
                                 , "Weekend", "Weekday")

by_interval_imputed <- group_by(Imputed_table, interval, day_type)
by_interval_imputed_mean <- summarise(by_interval_imputed, steps = mean(steps))

weekend_df <- by_interval_imputed_mean[by_interval_imputed_mean$day_type == "Weekend",]
weekday_df <- by_interval_imputed_mean[by_interval_imputed_mean$day_type == "Weekday",]

par(mfrow = c(2,1))

plot(weekend_df$interval, weekend_df$steps, type = "l", xlab = "Interval", ylab = "Number of steps", cex.lab = 1.0, main = "Weekend")
plot(weekday_df$interval, weekday_df$steps, type = "l", xlab = "Interval", ylab = "Number of steps", cex.lab = 1.0, main = "Weekday")
```



