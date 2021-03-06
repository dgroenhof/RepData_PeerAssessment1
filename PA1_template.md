# Reproducible Research: Peer Assessment 1

### Introduction
This document contains my solution for 'Course Project 1' of the 'Reproducible Research' by Johns Hopkins University on the Coursera platform. This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The dataset for this assignment can be downloaded here: [Activity Monitoring Data](https://github.com/dgroenhof/RepData_PeerAssessment1/blob/master/activity.zip)

For more information on the assignment checkout the [README.md](https://github.com/dgroenhof/RepData_PeerAssessment1/blob/master/README.md).

**Note:**
While creating this document I assumed that the `activity.zip` file, needed to finish this assignment, is in the current working directory of your R environment. The remainder of this document will describe the R-code written to deliver the requested results and the output it provides.

### Loading Libraries
The first thing the script does is to load the additional three libraries needed for the rest of the R script to work properly. The libraries are needed for data manipulation and for the creation of on of the graphs.


```r
library(dplyr)
library(data.table)
library(lattice)
```


### Loading and preprocessing the data
After loading the libraries, the source file `activity.zip` is unzipped, the unzipped file `activity.csv` is read and the `date` variable is changed from factor to normal date variable.

```r
# unzip the activity.zip file
unzip("activity.zip")

# read the activity.csv file into the 'data' date.frame
data <- read.csv("activity.csv")

# change the factors into real date values
data$date <- as.Date(data$date, format = "%Y-%m-%d")
```
We can then move on to answer the actual questions.

### What is mean total number of steps taken per day?
First we subset the data by only selecting the number of steps and the data, we then group the dataset by date and do a sum of all the steps per day.

```r
sum.steps <-    subset(data, select = c(steps, date)) %>%  
                group_by(date) %>% 
                summarise(total.steps = sum(steps))
```

Next we create a histogram of the summarised data:

```r
hist(sum.steps$total.steps, 
     breaks = 10, 
     col="#4CACD6", 
     main = "Histogram Total number of steps taken per day", 
     xlab = "Total number of steps taken per day",
     mar = c(5,6,4,2), 
     las = 1, 
     cex.axis = 0.9, 
     cex.main = 1.2, 
     cex.lab = 1.0
         )
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

The mean of the total number of steps per day is calculated and displayed here:

```r
orig.mean <- mean(sum.steps$total.steps, na.rm = TRUE)
orig.mean
```

```
## [1] 10766.19
```
The median of the total number of steps per day is calculated and displayed here:

```r
orig.median <- median(sum.steps$total.steps, na.rm = TRUE)
orig.median
```

```
## [1] 10765
```

### What is the average daily activity pattern?
We use the original dataset and create a subset by selecting the steps and the interval. We then group the dataset by interval and calculate the average (mean) of the number of steps per interval.
After that we calculate the maximum number of steps (stored in `max.steps`) and lookup which 5-minute interval contains that maximum number of steps.

```r
itv.steps <-   subset(data, select = c(steps, interval)) %>%  
                    group_by(interval) %>% 
                    summarise(average.steps = mean(steps, na.rm=TRUE))

max.steps <- max(itv.steps$average.steps)
max.steps.itv <- as.numeric(itv.steps[itv.steps$average.steps == max.steps,1])
```

We then create a time series plot and add a horizontal line showing the maximum number of steps and a vertical line to show which 5-minute interval contains that maximum number of steps.

```r
with(itv.steps, plot(interval, 
                     average.steps, 
                     type="l", 
                     lwd = 2, 
                     col="#4CACD6",
                     mar = c(5,6,4,2), 
                     main = "Time Series - Average number of steps per interval", 
                     xlab = "5-minute Interval",
                     ylab = "Average number of steps",
                     las = 1, 
                     cex.axis = 0.9, 
                     cex.main = 1.2, 
                     cex.lab = 1.0
                     )
     )

abline(h = max.steps, lwd=1, lty=2, col="#666666")
abline(v = max.steps.itv, lwd=1, lty=2, col="#666666")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

### Imputing missing values
First we calculate and display the total number of missing values:

```r
sum.na = sum(is.na(data$steps))
cat("Total number of missing values = ", sum.na)
```

```
## Total number of missing values =  2304
```

For each of these missing values, we choose the strategy to replace the missing values with the mean value for the particular 5-minute interval. We first join the `data` variable with the `itv.steps` variable, which were created in previous steps of the assignment. We store this in a new dataset called `data.imputed`. We then set the interval to the average value for that interval, only if the original interval value is missing.


```r
# Use mean for the interval if value for steps is missing (na)
data.imputed <- inner_join(data, itv.steps, by="interval")

data.imputed[is.na(data.imputed$steps), 1] <- data.imputed[is.na(data.imputed$steps), 4]
```

With the new dataset we recalculate the total number of steps taken each day, now including the imputed data. We do this creating a subset containing the number of steps and the date, then grouping it by date and finally calculating the sum of the number of steps per date.

```r
sum.steps.imputed <- subset(data.imputed, select = c(steps, date)) %>%  
    group_by(date) %>% 
    summarise(total.steps = sum(steps))
```

The new calculation of the mean of the total number of steps per day is calculated and displayed here:

```r
imp.mean <- mean(sum.steps.imputed$total.steps, na.rm = TRUE)
imp.mean
```

```
## [1] 10766.19
```

The new calculation of the median of the total number of steps per day is calculated and displayed here:

```r
imp.median <- median(sum.steps.imputed$total.steps, na.rm = TRUE)
imp.median
```

```
## [1] 10766.19
```

With these new variables we create a histogram, showing the total number of steps taken per day.

```r
hist(sum.steps.imputed$total.steps, 
     breaks = 10, 
     col="#4CACD6", 
     main = "Histogram Total number of steps taken per day\n(including imputed data)", 
     xlab = "Total number of steps taken per day",
     mar = c(5,6,4,2), 
     las = 1, 
     cex.axis = 0.9, 
     cex.main = 1.2, 
     cex.lab = 1.0)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)

To compare the differences between the datasets with missing data and with imputed data, we look at mean and median values in both datasets.

The mean in the original dataset is **10766.19**.<br>
The mean in the imputed dataset is **10766.19**.

The median in the original dataset is **10765**.<br>
The median in the imputed dataset is **10766.19**.

So the impact of the used strategy of imputing data is that the mean value doesn't change, whereas the median value has changed and has moved towards the mean value.

### Are there differences in activity patterns between weekdays and weekends?
To answer this question we first create a vector using the `weekdays()` function to determine whether a date is either a weekend day (Saturday and Sunday) or a weekday (all other days). We then add this vector to the imputed dataset and recalculate the average (mean) values per interval, this time grouped by interval and daytype.

```r
daytype <- ifelse(weekdays(data.imputed$date) %in% c("Saturday","Sunday"), "weekend", "weekday")

data.imputed <- cbind(data.imputed, daytype)

itv.steps.imputed <-   subset(data.imputed, select = c(steps, interval, daytype)) %>%  
    group_by(interval, daytype) %>% 
    summarise(average.steps = mean(steps, na.rm=TRUE))
```

We then create a lineplot with 2 panels, showing the average number of steps per 5-minute interval, where the top panel shows the weekend values and the bottom panel shows the weekday values.

```r
xyplot(average.steps ~ interval | daytype, 
       itv.steps.imputed, 
       type = "l", 
       lwd = 1, 
       main = "Time Series - Average number of steps per interval",
       xlab = "5-minute interval", 
       ylab = "Average number of steps", 
       layout = c(1,2)
      )
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)
