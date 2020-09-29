---
title: "PA1_template.Rmd"


author: "Andres Yrigoyen"

date: "9/27/2020"

output: html_document


# Reproducible Research: Peer Assessment 1


This assignment makes use of data from a personal activity monitoring device.
This device collects data at 5 minute intervals through out the day. 
The data consists of two months of data from an anonymous individual
collected during the months of October and November, 2012 and 
include the number of steps taken in 5 minute intervals each day.
Data is already loaded in local project data base 
Directory was assigned at such database


## Question 1.Loading and preprocessing the data

It is required to show any code required for Loading and preprocessing the data
Also Process/transform the data (if necessary) into a format suitable for
your analysis
```{r load activity, echo = TRUE}
activity <- read.csv("activity.csv", header=TRUE, na.strings = "NA")
```
Loading packages
```{r packages, echo = TRUE}
library(dplyr)
library(lubridate)
library(ggplot2)
library(httpuv)
```


Cleaning up the date class
```{r activity,echo = TRUE }
activityperdate <- ymd(activity$date)
```
Removing NA values from data base
```{r NA removing, echo = TRUE}
activity_noNAs <- na.omit(activity)
```
data review and visualization
```{r data visualization, echo = TRUE}
summary(activity_noNAs)
str(activity_noNAs)
head(activity_noNAs)
```
## Question 2. What is mean total number of steps taken per day?


1. Calculate the total number of steps taken per day
```{r number of steps, echo = TRUE}
activity_noNAs2 <- summarise(group_by(activity_noNAs,date),daily.step=sum(steps))
activity_noNAs2
```
2. Make a histogram of the total number of steps taken each day
Data preparation for ggplot


Histogram construction
```{r histogram1, echo = TRUE}
mean_activity <- as.integer(mean(activity_noNAs2$daily.step))
mean_activity
median_activity <- as.integer(median(activity_noNAs2$daily.step))
median_activity
steps_day_plot <- ggplot(activity_noNAs2, aes(x=daily.step)) + 
    geom_histogram(binwidth = 1200, fill="green")    + 
    geom_vline(xintercept=mean_activity, colour="yellow", linetype="dashed", size=1.5) +
    geom_vline(xintercept=median_activity, colour="red" , linetype="dotted", size=1.5) +
    labs(title="Histogram of Number of Steps taken per day", y="Frequency", x="Daily Number of Steps") 
steps_day_plot
```


3. Calculate and report the mean and median of the total number of steps taken per day


### Mean total number of steps taken per day
```{r mean, echo = TRUE}
mean_activity <- as.integer(mean(activity_noNAs2$daily.step))
mean_activity
```
### Median total number of steps taken per day
```{r median, echo = TRUE}
median_activity <- as.integer(median(activity_noNAs2$daily.step))
median_activity
```
### the mean total number of steps per day is 10766
### the median total number of steps per day is 10765
Note: Results (mean and median)show that the data behave as a normal distribution
## Question 3. What is the average daily activity pattern?


1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all days (y-axis)
Prepare data for plootting using ggplot


```{r daily, echo = TRUE}
activity_noNAs3 <- activity_noNAs %>% group_by(interval) %>% summarise(mean.step=mean(steps))
plot_steps_perday <- ggplot(activity_noNAs3, aes(x=interval,y=mean.step)) + 
    geom_line(color="blue", size=1.5) + 
    labs(title="Average Number of Steps Taken vs 5-min Interval", y="Average Number of Steps", x="5-min Interval Times Series")
plot_steps_perday
```


2. Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?


```{r max steps, echo = TRUE}
MaxStep_Int <- which.max(activity_noNAs3$mean.step)
MaxStep_Int_step <- activity_noNAs3$interval[MaxStep_Int]
sprintf("Maximum number of steps is found in the %gth 5-min interval", MaxStep_Int_step)
```


Maximum number of steps is found in the 835th 5-min interval
## Question 4.Imputing missing values. 


Note that there are a number of days/intervals where there are missing values
(coded as NA). The presence of missing days may introduce bias into some 
calculations or summaries of the data.


1. Calculate and report the total number of missing values in the dataset 
(i.e. the total number of rows with NAs)
the total number of missing values in the dataset can be compute by
```{r na activity, echo = TRUE}
sum(is.na(activity))
```
##The answer is [1] 2304


2. Devise a strategy for filling in all of the missing values in the dataset.
The strategy does not need to be sophisticated. For example, you could use
the mean/median for that day, or the mean for that 5-minute interval, etc
```{r impute1, echo = TRUE}

impute_na_activity <- activity
impute_na_activity$steps[is.na(impute_na_activity$steps)] <- mean(impute_na_activity$steps,na.rm=TRUE)
impute_na_activity$steps <- as.numeric(impute_na_activity$steps)
impute_na_activity$interval <- as.numeric(impute_na_activity$interval)
colSums(is.na(impute_na_activity))
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r summary inpute1, echo = TRUE}
summary(impute_na_activity)
```
4.Make a histogram of the total number of steps taken each day and Calculate
and report the mean and median total number of steps taken per day. Do these 
values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total 
daily number of steps?


```{r impute histogram2, echo = TRUE}
library(Hmisc)
impute_na_activity_noNAs2 <- summarise(group_by(impute_na_activity,date),daily.step=sum(steps))

mean_na_impute   <- as.integer(mean(impute_na_activity_noNAs2$daily.step))
mean_na_impute
median_na_impute <- as.integer(median(impute_na_activity_noNAs2$daily.step))
median_na_impute
steps_day_hist <- ggplot(impute_na_activity_noNAs2, aes(x=daily.step)) + 
    geom_histogram(binwidth = 1200, aes(y=..count.., fill=..count..)) + 
    geom_vline(xintercept=mean_na_impute, colour="yellow", linetype="dashed", size=1.5) +
    geom_vline(xintercept=median_na_impute, colour="red" , linetype="dotted", size=1.5) +
    labs(title="Histogram of Number of Steps taken per day (impute data)", y="Frequency", x="Daily Steps")
steps_day_hist
```


### Answer Mean total number of steps taken per day (after imputing na values) is 10766.
### Answer Median total number of steps taken per day (after imputing na values) is 10766.
Note: when the mean and median are the same means that the data is normally 
distributed. Replacing na values in the original dataset with mean and median
values just confirm that data is normally distributed


##Question 5.Are there differences in activity patterns between weekdays and weekends?


1. Create a new factor variable in the dataset with two levels - "weekday" and 
"weekend" indicating whether a given date is a weekday or weekend day.
```{r impute weekday, echo = TRUE}
impute_na_activity$day <- ifelse(weekdays(as.Date(impute_na_activity$date)) %in% c("Saturday","Sunday"), "weekday", "weekend")
```
2.Make a panel plot containing a time series plot (i.e. type = "l") of the
5-minute interval (x-axis) and the average number of steps taken, averaged 
across all weekday days or weekend days (y-axis)


```{r impute new data frame, echo = TRUE}
impute.df <- impute_na_activity %>% group_by(interval,day) %>% summarise(mean.step=mean(steps))

week_weekday_interval_plot<- ggplot(impute.df, aes(x=interval, y=mean.step, color=day)) + 
    facet_grid(day~.) +
    geom_line(size=1.5) + 
    labs(title="Average Number of Steps Taken vs 5-min Interval on Weekday/Weekend", y="Average Number of Steps", x="5-min Interval Times Series")
week_weekday_interval_plot
```