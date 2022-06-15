library(lubridate)
library(dplyr)
library(ggplot2)

##### READING THE DATA

# unzip the file if it wasn't already 
if (!file.exists("activity.csv")){
  unzip("activity.zip")
}


###### load data
ActMon <- read.csv("activity.csv")
ActMon$date <- as.Date(ActMon$date) # transform date

##### mean steps per day
## total number of steps per day
StepsDay <- tapply(ActMon$steps,ActMon$date,sum, na.rm = TRUE)

## histogram
ggplot() + aes(StepsDay) + geom_histogram(bins = 14, colour = "black", fill = "blue") +
  xlab("Number of steps per day") + ylab("Frequency") + theme_bw()

## mean and median of the total number of steps taken per day
round(mean(StepsDay), digits = 2)
round(median(StepsDay), digits = 2)


##### Average daily activity pattern
## Time series plot
# Average steps per interval
StepsInt <- tapply(ActMon$steps, ActMon$interval, mean, na.rm = TRUE)

# Plot
plot(names(StepsInt), StepsInt, type = "l", col = "blue", lwd = 2, xlab = "Interval of the day", ylab = "Number of steps")
# Which interval max steps
maxint <- which(StepsInt == max(StepsInt, na.rm = TRUE))
# temph <- make_datetime(0, 0, 0, as.numeric(names(maxint)) %/% 100, as.numeric(names(maxint)) %% 100)
# maxtimeint <- nchar(temph)


##### Input of missing values
## total number of rows with `NA`s
dim(ActMon)[1]-sum(complete.cases(ActMon))

## filling in all of the missing values in the dataset
# checking the interested columns
sum(is.na(ActMon$date))
sum(is.na(ActMon$interval))
sum(is.na(ActMon$steps)) # only steps missing
# replacing the NAs with the mean for the corresponding interval
ActMon2 <- ActMon
for (i in 1:dim(ActMon)[1]){
  if (is.na(ActMon[i,1])){
    ActMon2[i,1] <- StepsInt[as.character(ActMon[i,3])]
  }
}

## Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** 
# Total number of steps per day
StepsDay2 <- tapply(ActMon2$steps,ActMon2$date,sum)
# Histogram
ggplot() + aes(StepsDay2) + geom_histogram(bins = 14, colour = "black", fill = "blue") +
  xlab("Number of steps per day") + ylab("Frequency") + theme_bw()
# mean and median of the total number of steps taken per day
round(mean(StepsDay2), digits = 2)
round(median(StepsDay2), digits = 2)
# compared to the previous
round(mean(StepsDay), digits = 2)
round(median(StepsDay), digits = 2)


##### Are there differences in activity patterns between weekdays and weekends?
## Create a new factor variable
ActMon2[,4] <- as.factor(weekdays(ActMon2$date))
levels(ActMon2$V4)[c(1:2,5:7)] <- "weekday"
levels(ActMon2$V4)[c(2,3)] <- "weekend"

## Make a panel plot containing a time series plot
# calculate average for interval across weekday or weekend days
#StepsInt2 <- tapply(ActMon2$steps, ActMon2$interval, mean, na.rm = TRUE)
AvgTab <- ActMon2 %>%
  group_by(V4, interval) %>%
  summarise(mean = mean(steps))
# panel plot
g <- ggplot(AvgTab,aes(interval,mean))
g + geom_line(color = "blue", size = .8) + facet_grid(V4~.) + labs(x = "Interval", y = "Mean number of steps") +
  theme_bw()
