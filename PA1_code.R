library(ggplot2)

#loading data - considering the activity.zip file is on the same directory as the script.
rawactivity <- read.csv(unzip("repdata-data-activity.zip"))

rawactivity$date <- as.Date(rawactivity$date, "%Y-%m-%d")

## 1.1. Calculate the total number of steps taken per day
cleandata <- rawactivity[complete.cases(rawactivity),]

datevector <- unique(as.Date(cleandata$date, "%Y-%m-%d"))

totalsteps <- vector()
datetotalsteps <- vector()
index <- 1

for (i in 1:length(datevector)){
        if (is.na(sum(cleandata[cleandata[, "date"] ==  eval(datevector[i]),][,1])) == FALSE){
                totalsteps[index] <- sum(cleandata[cleandata[, "date"] ==  eval(datevector[i]),][,1])
                datetotalsteps[index] <- as.character(datevector[i])
                index <- index + 1
        }
}

sumdata <- data.frame(date=as.Date(datetotalsteps, "%Y-%m-%d"), total.steps=totalsteps, stringsAsFactors = FALSE)

##1.2. Make a histogram of the total number of steps taken each day.


par(mfrow= c(1,1))

plot(sumdata, type = "h", main = "Total number of steps taken each day")

##1.3. Calculate and report the mean and median of the total number of steps taken per day

## mean
meantotalsteps <- vector()
datetotalsteps <- vector()
index <- 1

for (i in 1:length(datevector)){
        if (is.na(mean(cleandata[cleandata[, "date"] ==  eval(datevector[i]),][,1])) == FALSE){
                meantotalsteps[index] <- mean(cleandata[cleandata[, "date"] ==  eval(datevector[i]),][,1])
                datetotalsteps[index] <- as.character(datevector[i])
                index <- index + 1
        }
}

initialmeandata <- data.frame(date=as.Date(datetotalsteps, "%Y-%m-%d"), mean=meantotalsteps, stringsAsFactors = FALSE)

## median

stepsperday <- vector()
sumstepsperday <- vector()
mediantotalsteps <- vector()
mediandatetotalsteps <- vector()
countmeasurestepsperday <- vector()
index <- 1
indexsteps <- 1

for (i in 1:length(datevector)){
        stepsperday <-  cleandata[cleandata[, "date"] ==  eval(datevector[i]),][,1]
        countmeasure <- 0
        sumsteps <- 0
        for (j in 1:length(stepsperday)) {
                if (stepsperday[j] > 0){
                        sumsteps <- sumsteps + stepsperday[j]
                        countmeasure <- countmeasure + 1
                }
        }
        mediantotalsteps[index] <- sumsteps / countmeasure
        mediandatetotalsteps[index] <- as.character(datevector[i])
        countmeasurestepsperday[index] <- countmeasure
        sumstepsperday[index] <- sumsteps
        index <- index + 1        
}

initialmedianstepsperday <- data.frame(date=as.Date(datetotalsteps, "%Y-%m-%d"), median=mediantotalsteps, stringsAsFactors = FALSE)

reportmeanmedian <- merge(initialmeandata, initialmedianstepsperday)

reportmeanmedian

##2. ###What is the average daily activity pattern?

##2.1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

stepsperinterval <- vector()
sumstepsperinterval <- vector()
mediantotalsteps <- vector()
mediandatetotalsteps <- vector()
countmeasurestepsperinterval <- vector()
index <- 1
indexsteps <- 1
intervalvector <- unique(cleandata$interval)

for (i in 1:length(intervalvector)){
        stepsperinterval <-  cleandata[cleandata[, "interval"] ==  eval(intervalvector[i]),][,1]
        countmeasure <- 0
        sumsteps <- 0
        for (j in 1:length(stepsperinterval)) {
                if (stepsperinterval[j] > 0){
                        sumsteps <- sumsteps + stepsperinterval[j]
                        countmeasure <- countmeasure + 1
                }
        }
        if (countmeasure == 0) {
                mediantotalsteps[index] <- countmeasure        
        }else{
                mediantotalsteps[index] <- sumsteps / countmeasure        
        }
        countmeasurestepsperinterval[index] <- countmeasure
        sumstepsperinterval[index] <- sumsteps
        index <- index + 1        
}



par(mfrow = c(1,1))
plot(intervalvector,mediantotalsteps, type ="l")

##2.2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
##order high

datasteps <- data.frame(median.steps=mediantotalsteps, interval=intervalvector, stringsAsFactors = FALSE)

maxsteps <- datasteps[order(datasteps$median.steps, na.last = NA, decreasing = TRUE),]

head(maxsteps,1)[,2]

##3 Imputing missing values

##Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

tail(summary(rawactivity),1)[,1]

##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

datevector <- unique(as.Date(rawactivity$date, "%Y-%m-%d"))
meantotalsteps <- vector()
datetotalsteps <- vector()
index <- 1

for (i in 1:length(datevector)){
        meantotalsteps[index] <- mean(rawactivity[rawactivity[, "date"] ==  eval(datevector[i]),][,1])
        datetotalsteps[index] <- as.character(datevector[i])
        index <- index + 1
}

##Create a new dataset that is equal to the original dataset but with the missing data filled in.

meandata <- data.frame(date=as.Date(datetotalsteps, "%Y-%m-%d"), mean=meantotalsteps, stringsAsFactors = FALSE)

##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

## mean
meantotalsteps <- vector()
datetotalsteps <- vector()
index <- 1

for (i in 1:length(datevector)){
        meantotalsteps[index] <- mean(rawactivity[rawactivity[, "date"] ==  eval(datevector[i]),][,1])
        datetotalsteps[index] <- as.character(datevector[i])
        index <- index + 1
}

## median

stepsperday <- vector()
sumstepsperday <- vector()
mediantotalsteps <- vector()
mediandatetotalsteps <- vector()
countmeasurestepsperday <- vector()
index <- 1
indexsteps <- 1

for (i in 1:length(datevector)){
        stepsperday <-  rawactivity[rawactivity[, "date"] ==  eval(datevector[i]),][,1]
        countmeasure <- 0
        sumsteps <- 0
        for (j in 1:length(stepsperday)) {
                if (is.na(stepsperday[j]) == FALSE){
                        if (stepsperday[j] > 0){
                                sumsteps <- sumsteps + stepsperday[j]
                                countmeasure <- countmeasure + 1
                        }
                }else{
                        countmeasure <- countmeasure + 1
                }
        }
        
        if (countmeasure == 0) {
                mediantotalsteps[index] <- countmeasure        
        }else{
                mediantotalsteps[index] <- sumsteps / countmeasure        
        }
        mediandatetotalsteps[index] <- as.character(datevector[i])
        countmeasurestepsperday[index] <- countmeasure
        sumstepsperday[index] <- sumsteps
        index <- index + 1        
}

medianstepsperday <- data.frame(date=as.Date(datetotalsteps, "%Y-%m-%d"), median=mediantotalsteps, stringsAsFactors = FALSE)

reportmeanmedian <- merge(meandata, mediantotalsteps)

par(mfrow=c(1,2))

plot(meandata, type ="h", col = "blue" , main = "Mean steps per day")
plot(medianstepsperday, type ="h", col = "blue" , main = "Median steps per day")

##Do these values differ from the estimates from the first part of the assignment? 

par(mfrow = c(2, 2), pty = "s") ## multiple plots

plot(initialmeandata, type ="h", col = "red", main = "Initial mean steps per day")
plot(initialmedianstepsperday, type ="h", col = "red", main = "Initial median steps per day")
plot(meandata, type ="h", col = "blue" , main = "Final mean steps per day")
plot(medianstepsperday, type ="h", col = "blue" , main = "Final median steps per day")


##What is the impact of imputing missing data on the estimates of the total daily number of steps?

finaltotalstepsperday <- data.frame(meandata$date,sumstepsperday,stringsAsFactors = FALSE)

par(mfrow= c(1,2))

plot(sumdata, type = "h", col = "blue", main = "Total number of steps \n Cleaned")
plot(finaltotalstepsperday, type = "h", col = "red", main = "Total number of steps \n Missing data imputed")

##Are there differences in activity patterns between weekdays and weekends?

##For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

weeklyactivity <- rawactivity

dayoftheweek <- vector()

##Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

for (i in 1:length(weeklyactivity$date)){
        if (as.POSIXlt(weeklyactivity$date[i])$wday == 0){
                dayoftheweek[i] <- "weekend"
        }else{
                if (as.POSIXlt(weeklyactivity$date[i])$wday == 6){
                        dayoftheweek[i] <- "weekend"
                }else{
                        dayoftheweek[i] <- "weekday"        
                }
        }
}

weeklyactivity <- cbind(weeklyactivity, "weekday"=as.factor(dayoftheweek))

weekdaysubset <- weeklyactivity[weeklyactivity [,"weekday"] == "weekday",]
weekendsubset <- weeklyactivity[weeklyactivity [,"weekday"] == "weekend",]

##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

par(mfrow = c(1,2))

plot(weekdaysubset$interval, weekdaysubset$steps, main = "WeekDays", type = "l", col = "blue")
plot(weekendsubset$interval, weekendsubset$steps, main = "WeekEnds" , type = "l", col = "blue")

