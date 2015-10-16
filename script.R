## Include Library
library(ggplot2)
library(knitr)

## Unzip File
unzip ("activity.zip", exdir = "./")

## Read File
data_activity <- read.csv("activity.csv")
data_activity <- data_activity[c("date", "steps", "interval")]
data_activity <- data_activity[complete.cases(data_activity),]

data_agg <- aggregate(data_activity$steps, by = list(data_activity$date), sum)
names(data_agg) <- c("date", "steps")
head(data_agg)

## Draw Histogram
hist(data_agg$steps, xlab = "Number of Steps", main = "Total Number of Steps Per Day")

## Creating the Summary
summary(data_agg)


##  What is the average daily activity pattern?
interval_agg <- aggregate(steps ~ interval, data_activity, mean)
plot(interval_agg$interval, interval_agg$steps, type='l', main="All-day Average Steps", xlab="Interval", ylab="Average number of steps")
interval_agg[which.max(interval_agg$steps),]


## Imputing Missing Values
imputed <- read.csv("activity.csv")
sum(!complete.cases(imputed))

for (i in 1:nrow(imputed)){
    if (is.na(imputed$steps[i])) {
        take_interval <- imputed$interval[i]
        take_steps <- interval_agg[interval_agg$interval == take_interval,]
        imputed$steps[i] <- take_steps$steps
    }
}

imputed_agg <- aggregate(imputed$steps, by = list(imputed$date), sum)
names(imputed_agg) <- c("date", "steps")

## Draw Histogram
hist(imputed_agg$steps, xlab = "Number of Steps", main = "Total Number of Steps Per Day")

## Creating the Summary
summary(imputed_agg)


## Are there differences in activity patterns between weekdays and weekends?
imputed['Type'] <- weekdays(as.Date(imputed$date))
imputed$Type[imputed$Type  %in% c('Saturday','Sunday') ] <- "weekend"
imputed$Type[imputed$Type != "weekend"] <- "weekday"

imputed$Type <- as.factor(imputed$Type)
imputed_type_agg <- aggregate(steps ~ interval + Type, imputed, mean)

qplot(data = imputed_type_agg, interval, steps, type = 'l', geom=c("line"), xlab = "Interval", ylab = "Number of steps", main = "") + facet_wrap(~ Type, ncol = 1)
