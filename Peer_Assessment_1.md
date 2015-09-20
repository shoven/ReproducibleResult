# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activity.data <- read.csv("activity.csv", header=TRUE, sep=",", colClasses=c("numeric","Date"
                                                                             ,"numeric"))
activity.data$interval <- as.factor(activity.data$interval)
str(activity.data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

```r
stepsperday <- aggregate(steps ~ date, activity.data, sum)
colnames(stepsperday) <- c("date","steps")
stepsperday <- aggregate(steps ~ date, activity.data, sum)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```


## What is mean total number of steps taken per day?


```r
activity.data$interval <- as.factor(activity.data$interval)
str(activity.data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

```r
stepsperday <- aggregate(steps ~ date, activity.data, sum)
colnames(stepsperday) <- c("date","steps")
stepsperday <- aggregate(steps ~ date, activity.data, sum)

ggplot(stepsperday, aes(x = steps)) + 
  geom_histogram(fill = "blue", binwidth = 1000) + 
  labs(title="Steps Taken per Day", 
       x = "Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 
```

![](Peer_Assessment_1_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean_steps <- mean(stepsperday$steps, na.rm=TRUE)
median_steps <- median(stepsperday$steps, na.rm=TRUE)
```

## What is the average daily activity pattern?

```r
stepsperinterval <- aggregate(activity.data$steps, 
                              by = list(interval = activity.data$interval),
                              FUN=mean, na.rm=TRUE)
stepsperinterval$interval <- 
  as.integer(levels(stepsperinterval$interval)[stepsperinterval$interval])
colnames(stepsperinterval) <- c("interval", "steps")
ggplot(stepsperinterval, aes(x=interval, y=steps)) + geom_line(color="red", size=1) +  
  labs(title="Activity Pattern", x="Interval", y="steps") +  
  theme_bw()
```

![](Peer_Assessment_1_files/figure-html/unnamed-chunk-3-1.png) 

## Imputing missing values
### Replace missing value with mean
### We can see the difference between mean before NA values and after NA values as shown below which is minimal


```r
noofmissingvalues <- sum(is.na(activity.data$steps))
naposition <- which(is.na(activity.data$steps))
meanvector <- rep(mean(activity.data$steps, na.rm=TRUE), times=length(naposition))
activity.data[naposition, "steps"] <- meanvector
noofmissingvalues <- sum(is.na(activity.data$steps))
noofmissingvalues
```

```
## [1] 0
```

```r
stepsperday <- aggregate(steps ~ date, activity.data, sum)
ggplot(stepsperday, aes(x = steps)) + 
  geom_histogram(fill = "blue", binwidth = 1000) + 
  labs(title="Steps Taken per Day After replacing missing values with Mean", 
       x = "Steps per Day", y = "Number of times in a day(Count)") + theme_bw()
```

![](Peer_Assessment_1_files/figure-html/unnamed-chunk-4-1.png) 

```r
mean_steps_after_removing_NA <- mean(stepsperday$steps, na.rm=TRUE)
median_steps_after_removing_NA <- median(stepsperday$steps, na.rm=TRUE)

meandifferencafterimputingNAs <- mean_steps_after_removing_NA - mean_steps
mediandifferencafterimputingNAs <- median_steps_after_removing_NA - median_steps

meandifferencafterimputingNAs
```

```
## [1] 0
```

```r
mediandifferencafterimputingNAs
```

```
## [1] 1.188679
```



## Are there differences in activity patterns between weekdays and weekends?

```r
activity.data$weekdays <- factor(format(activity.data$date, "%A"))
levels(activity.data$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(activity.data$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(activity.data$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(activity.data$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

```r
weekdayweekendavg <- aggregate(activity.data$steps, 
                          list(interval = as.numeric(as.character(activity.data$interval)), 
                               weekdays = activity.data$weekdays),
                          FUN = "mean")
names(weekdayweekendavg)[3] <- "meanOfSteps"
library(lattice)
weekendweekdayplot <- xyplot(weekdayweekendavg$meanOfSteps ~ weekdayweekendavg$interval | weekdayweekendavg$weekdays, 
                layout = c(1, 2), type = "l",  col=c("blue"),
                xlab = "Interval", ylab = "steps")
print(weekendweekdayplot)
```

![](Peer_Assessment_1_files/figure-html/unnamed-chunk-5-1.png) 
## Looking at the graph above, there is a difference between weekday and weekend activities.




