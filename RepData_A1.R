library(lubridate)

# READING THE DATA

# unzip the file if it wasn't already 
if (!file.exists("activity.csv")){
  unzip("activity.zip")
}

###### load data
ActMon <- read.csv("activity.csv")
ActMon$date <- as.Date(ActMon$date) # transform date

ActMon[,4] <- as.factor(ActMon$interval) 
levels(ActMon[,4]) <- seq(0, length(levels(ActMon[,4]))*5-1,5)
temp <- as.character(make_datetime(0, 0, 0, seq(0, length(levels(ActMon[, 4]))*5-1, 5) %/% 60,
                                  seq(0, length(levels(ActMon[, 4]))*5-1, 5) %% 60))
levels(ActMon[,4]) <- substr(temp, nchar(temp[1])-7, nchar(temp[1])-3)

##### mean steps per day
## total number of steps per day
StepsDay <- tapply(ActMon$steps,ActMon$date,sum, na.rm = TRUE)
## histogram
hist(StepsDay)#, breaks = 5)
## mean and median of the total number of steps taken per day
mean(StepsDay)
median(StepsDay)

##### Average daily activity pattern
## Time series plot
# Average steps per interval
StepsInt <- tapply(ActMon$steps, ActMon$interval, mean, na.rm = TRUE)

# Plot
plot(as.Date(names(StepsInt2),"%H%M"), StepsInt2, type = "l")
plot(seq(0,length(StepsInt2)-1), StepsInt2, type = "l")
axis(1, seq(1,length(StepsInt2)-1,50))#, labels = names(StepsInt2)[c(seq(1,length(StepsInt2)-1,50))])
# Which interval max steps
which(StepsInt == max(StepsInt, na.rm = TRUE))







names(StepsInt)
max(StepsDay)
AA <- ActMon[ActMon$interval == 2330,]
mean(AA$steps, na.rm = TRUE)

NEI$year <- as.factor(NEI$year) # factorising the year 

# Calculating the sum of emissions per year
totEmYear <- tapply(NEI$Emissions, NEI$year, sum, na.rm=TRUE)

# Plotting using base plotting system and saving to file
png("plot1.png")
plot(names(totEmYear), totEmYear/10^6, type="b", col="red", lwd=2, pch=1, cex = 1.5, 
     xlab="Year", ylab = "Emissions / Millions of tons", main = "Total PM2.5 emissions per year - U.S.")
dev.off()
