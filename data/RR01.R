library(dplyr)
library(data.table)
library(lattice)

# Set the working directories
work.dir <- getwd()

unzip("activity.zip")
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, format = "%Y-%m-%d")

sum.steps <-    subset(data, select = c(steps, date)) %>%  
                group_by(date) %>% 
                summarise(total.steps = sum(steps))

par(mar = c(5,6,4,2), 
    las = 1, 
    cex.axis = 0.9, 
    cex.main = 1.2, 
    cex.lab = 1.0, 
    bg = "#F2F2F2", 
    col.axis ="#666666", 
    col.main = "#666666", 
    col.lab = "#666666"
    )

hist(sum.steps$total.steps, 
     breaks = 10, 
     col="#4CACD6", 
     main = "Histogram Total number of steps taken per day", 
     xlab = "Total number of steps taken per day"
     )

cat("Mean: ", mean(sum.steps$total.steps, na.rm = TRUE))

cat("Median: ", median(sum.steps$total.steps, na.rm = TRUE))

itv.steps <-   subset(data, select = c(steps, interval)) %>%  
                    group_by(interval) %>% 
                    summarise(average.steps = mean(steps, na.rm=TRUE))

max.steps <- max(itv.steps$average.steps)
max.steps.itv <- as.numeric(itv.steps[itv.steps$average.steps == max.steps,1])

with(itv.steps, plot(interval, average.steps, type="l", lwd = 3, col="#4CACD6"))

abline(h = max.steps, lwd=2, lty=2, col="#666666")
abline(v = max.steps.itv, lwd=2, lty=2, col="#666666")

sum.na = sum(is.na(data$steps))

# Use mean for the interval if value for steps is missing (na)
data.imputed <- inner_join(data, itv.steps, by="interval")

data.imputed[is.na(data.imputed$steps), 1] <- data.imputed[is.na(data.imputed$steps), 4]

sum.steps.imputed <- subset(data.imputed, select = c(steps, date)) %>%  
    group_by(date) %>% 
    summarise(total.steps = sum(steps))

cat("Mean: ", mean(sum.steps.imputed$total.steps))

cat("Median: ", median(sum.steps.imputed$total.steps))

hist(sum.steps.imputed$total.steps, 
     breaks = 10, 
     col="#4CACD6", 
     main = "Histogram Total number of steps taken per day", 
     xlab = "Total number of steps taken per day"
)


daytype <- ifelse(weekdays(data.imputed$date) %in% c("Saturday","Sunday"), "weekend", "weekday")

data.imputed <- cbind(data.imputed, daytype)

itv.steps.imputed <-   subset(data.imputed, select = c(steps, interval, daytype)) %>%  
    group_by(interval, daytype) %>% 
    summarise(average.steps = mean(steps, na.rm=TRUE))

xyplot(average.steps ~ interval | daytype, itv.steps.imputed, type = "l", lwd = 1, xlab = "Interval", ylab = "Number of Steps", layout = c(1,2))
