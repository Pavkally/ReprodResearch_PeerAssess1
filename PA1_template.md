# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("./activity.zip")  # contains activity.csv
data <- read.csv("./activity.csv")
head(data)
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


## What is mean total number of steps taken per day?

First, let's see a histogram.

```r
steps.each.day <- tapply(data$steps, data$date, sum)

library(ggplot2)
bwidth <- diff(range(steps.each.day, na.rm = TRUE))/30
qplot(steps.each.day, geom = "histogram", binwidth = bwidth) + geom_rug(color = "blue", 
    alpha = 0.7) + labs(x = "Steps per day", y = "Frequency", title = "Histogram of total steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r

mu <- mean(steps.each.day, na.rm = TRUE)
med <- median(steps.each.day, na.rm = TRUE)
mu
```

```
## [1] 10766
```

```r
med
```

```
## [1] 10765
```


The mean number of total steps per day is **1.0766 &times; 10<sup>4</sup>** and the median
is **10765**.


## What is the average daily activity pattern?

The daily activity pattern averaged within each five minute interval across all
days:

```r
library(plyr)
interval.means <- ddply(data, "interval", summarise, mean = mean(steps, na.rm = TRUE))
ggplot(interval.means, aes(interval, mean)) + geom_path() + labs(title = "Mean # Steps by 5-minute Time Interval", 
    x = "Interval")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r

idx <- which.max(interval.means$mean)
interval.means[idx, ]
```

```
##     interval  mean
## 104      835 206.2
```


The interval with the maximum number of steps when averaged across all days is
**interval 835**.

## Imputing missing values

```r
incomplete.rows <- sum(!complete.cases(data))
incomplete.rows
```

```
## [1] 2304
```

It looks like we have **2304** rows with NAs in our dataset.
Before we start with imputation, let's try to get a feel for where the missing
values are. We can start by counting how many NA values exist within each five
minute interval across all days using ddply:


```r
interval.nas <- ddply(data, "interval", summarise, na.count = sum(is.na(steps)))
head(interval.nas)
```

```
##   interval na.count
## 1        0        8
## 2        5        8
## 3       10        8
## 4       15        8
## 5       20        8
## 6       25        8
```


Now isn't that interesting? The first few values are 8. I wonder how many
intervals have 8 NAs, could it be all of them?


```r
all(interval.nas$na.count == 8)
```

```
## [1] TRUE
```


Indeed! My guess is that this means that there were 8 days where no data were
collected. Let's confirm. We'll use ddply again to count the total
number of NA observations by day:


```r
intervals.per.day <- 24 * 60/5  # how many 5 minute intervals in a day (288)
intervals.per.day
```

```
## [1] 288
```

```r
date.nas <- ddply(data, "date", summarise, na.count = sum(is.na(steps)))
head(date.nas)
```

```
##         date na.count
## 1 2012-10-01      288
## 2 2012-10-02        0
## 3 2012-10-03        0
## 4 2012-10-04        0
## 5 2012-10-05        0
## 6 2012-10-06        0
```

```r
sum(date.nas$na.count == intervals.per.day)
```

```
## [1] 8
```


As expected, there are eight days with no data, and furthermore these eight
days contain the entire set of NAs:


```r
dates.with.nas <- unique(date.nas$date[date.nas$na.count != 0])
length(dates.with.nas)
```

```
## [1] 8
```


Given what we have learned so far, I believe that a reasonable imputation
strategy would be to populate the days that are missing data with the mean
values for that day of the week.

To get started, let's add a **weekday** column to our original data frame:


```r
data$weekday <- factor(weekdays(as.Date(data$date)), levels = c("Sunday", "Monday", 
    "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
head(data)
```

```
##   steps       date interval weekday
## 1    NA 2012-10-01        0  Monday
## 2    NA 2012-10-01        5  Monday
## 3    NA 2012-10-01       10  Monday
## 4    NA 2012-10-01       15  Monday
## 5    NA 2012-10-01       20  Monday
## 6    NA 2012-10-01       25  Monday
```

```r
head(data[data$interval == 0, ])
```

```
##      steps       date interval   weekday
## 1       NA 2012-10-01        0    Monday
## 289      0 2012-10-02        0   Tuesday
## 577      0 2012-10-03        0 Wednesday
## 865     47 2012-10-04        0  Thursday
## 1153     0 2012-10-05        0    Friday
## 1441     0 2012-10-06        0  Saturday
```

```r
weekdays.with.na <- unique(data[data$date %in% dates.with.nas, ]$weekday)
weekdays.with.na
```

```
## [1] Monday    Thursday  Sunday    Friday    Saturday  Wednesday
## Levels: Sunday Monday Tuesday Wednesday Thursday Friday Saturday
```


Looks good, now we need to compute the mean within each five minute interval
separately for each weekday:


```r
means.sd <- ddply(data, c("interval", "weekday"), summarise, steps = mean(steps, 
    na.rm = TRUE))
```


Now let's question whether or not computing the mean by weekday is going to be
any more informative than just substituting the overall mean across all days
(this is a bit of foreshadowing for the next question):


```r
ggplot(means.sd, aes(interval, steps)) + geom_line() + facet_grid(weekday ~ 
    .) + labs(x = "Interval", y = "Steps", title = "Daily activity")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


There seems to be enough variability between days to support imputation by
weekday. Let's fill in the missing values.


```r
# Separate the complete and incomplete cases
complete.ind <- complete.cases(data)
complete <- data[complete.ind, ]
incomplete <- data[!complete.ind, ]
# Merge the incomplete cases with the per-interval means by weekday
merged <- merge(incomplete[, c("weekday", "interval", "date")], means.sd, by = c("interval", 
    "weekday"))
# Bind the complete and newly imputed values together
imputed <- rbind(complete, merged)
sum(!complete.cases(imputed))  # should be 0 incomplete cases now
```

```
## [1] 0
```


Now we can make another histogram and recalculate the mean and median:

```r
imp.steps.each.day <- tapply(imputed$steps, data$date, sum)

bwidth <- diff(range(imp.steps.each.day, na.rm = TRUE))/30
qplot(imp.steps.each.day, geom = "histogram", binwidth = bwidth) + geom_rug(color = "blue", 
    alpha = 0.7) + labs(x = "Steps per day", y = "Frequency", title = "Histogram of total steps per day (post imputation)")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

```r

imp.mu <- mean(imp.steps.each.day, na.rm = TRUE)
imp.med <- median(imp.steps.each.day, na.rm = TRUE)
imp.mu
```

```
## [1] 10821
```

```r
imp.med
```

```
## [1] 11015
```


So the mean is now **1.0821 &times; 10<sup>4</sup>** and the median is **1.1015 &times; 10<sup>4</sup>**,
which are both different from the values we obtained before imputation
(1.0766 &times; 10<sup>4</sup>, 10765, respectively).


## Are there differences in activity patterns between weekdays and weekends?

We'll add an boolean indicator variable *weekend* to our data.frame that
includes imputed values and make a plot to answer the question.


```r
wkend.days <- c("Saturday", "Sunday")
day.type <- factor(imputed$weekday %in% wkend.days)
imputed$day.type <- mapvalues(day.type, from = c("FALSE", "TRUE"), to = c("Weekday", 
    "Weekend"))
imputed.means <- ddply(imputed, .(interval, day.type), summarise, mean = mean(steps, 
    na.rm = TRUE))
head(imputed.means)
```

```
##   interval day.type  mean
## 1        0  Weekday 2.311
## 2        0  Weekend 0.000
## 3        5  Weekday 0.450
## 4        5  Weekend 0.000
## 5       10  Weekday 0.175
## 6       10  Weekend 0.000
```

```r
ggplot(imputed.means, aes(interval, mean)) + geom_line() + facet_grid(day.type ~ 
    .) + labs(x = "Interval", y = "Steps", title = "Activity by day type")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 


There appears to be a difference. For one thing, our subject appears to sleep
a bit later on the weekends (much like myself).
