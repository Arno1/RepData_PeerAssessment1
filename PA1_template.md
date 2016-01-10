# Reproducible Research: Peer Assessment 1

Here we load the data and pre-process them.


```r
# Loads the data into an "activity" Data Frame
activity <- read.csv("activity.csv",header=TRUE)

# Converts the date column to a Date object/format
activity[,2] <- as.Date(activity[,2])

# Calculate the total number of steps taken per day, removing the NAs
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
by_day <- group_by(activity,date)
total_steps_per_day <- summarize(by_day,TotalSteps=sum(steps,na.rm=TRUE))
```

Now let's plot the histogram of the total number of steps taken each day :


```r
# Makes a histogram of the total number of steps per day
hist(total_steps_per_day$TotalSteps,xlab="Nb of steps",main="Histogram of total number of steps taken per day")
```

![](ActivityProject_files/figure-html/unnamed-chunk-2-1.png)\

Mean of nb of total number of steps taken per day : 9354.

Median of nb of total number of steps taken per day : 10395.

Now let's plot the time series of the average nb of steps taken, averaged across all days, as a function of the 5-minute intervals :


```r
by_interval <- group_by(activity,interval)
avg_steps_per_interval <- summarize(by_interval,AvgNbSteps=mean(steps,na.rm=TRUE))
plot(avg_steps_per_interval,xlab="Time/interval during the day", ylab =" Avg nb of steps across all days",type="l")
```

![](ActivityProject_files/figure-html/unnamed-chunk-3-1.png)\

Total nb of missing values in the dataset : 2304.

Let's add to the main Data Frame a column of imputed values, equal to the mean accross all days for the 5-mns interval whose value is missing, and a column of steps including these imputed values if they are necessary (i.e. if the original value is an NA) :



```r
activity$imputedvalues <- rep(round(avg_steps_per_interval$AvgNbSteps,digits=0),61)
activity$imputedsteps <- replace(activity$steps,is.na(activity$steps),activity$imputedvalues)
```

```
## Warning in replace(activity$steps, is.na(activity$steps), activity
## $imputedvalues): number of items to replace is not a multiple of
## replacement length
```

Now let's plot the histogram of the new total number of steps taken each day :


```r
# Makes a histogram of the total number of steps per day
by_day <- group_by(activity,date)
total_steps_per_day <- summarize(by_day,TotalSteps=sum(imputedsteps,na.rm=TRUE))
hist(total_steps_per_day$TotalSteps,xlab="Nb of steps",main="Histogram of nb of steps taken per day w/ imputed values for NAs")
```

![](ActivityProject_files/figure-html/unnamed-chunk-5-1.png)\

Mean of nb of total number of steps taken per day with imputed values for NAs : 1.0765639\times 10^{4}.

Median of nb of total number of steps taken per day with imputed values for NAs : 1.0762\times 10^{4}.

Let's now create a new factor binary variable (T/F) indicating whether a given day is a weekday or not. We'll then make a panel with two plots : average nb of steps averaged across all weekday days (plot 1) or weekend days (plot 2)  :


```r
# Loads the timeDate package a creates a new factor binary variable (T/F) indicating whether a given day is a weekday or not
library(timeDate)
```

```
## Warning: package 'timeDate' was built under R version 3.2.3
```

```r
activity$Weekday <- isWeekday(activity$date)

#  Makes a panel with the required plots
by_weekday <- group_by(activity,interval,Weekday)
avg_steps_per_weekday <- summarize(by_weekday,Steps=mean(imputedsteps,na.rm=TRUE))
library(lattice)
xyplot(Steps ~ interval | Weekday, data=avg_steps_per_weekday, type="l", ylab = "Nb of steps",strip=strip.custom(factor.levels=c("Weekends","Weekdays")), layout=c(1,2))
```

![](ActivityProject_files/figure-html/unnamed-chunk-6-1.png)\

