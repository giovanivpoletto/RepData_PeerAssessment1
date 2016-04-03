# Reproducible Research: Peer Assessment 1



###Loading and preprocessing the data

###Show any code that is needed to

1. Load the data (i.e. read.csv())


```r
rawactivity <- read.csv(unzip("repdata-data-activity.zip"))
```

2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
rawactivity$date <- as.Date(rawactivity$date, "%Y-%m-%d")
```

###What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
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
```

2. Make a histogram of the total number of steps taken each day.


```r
par(mfrow= c(1,1))

plot(sumdata, type = "h", main = "Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)


3. Calculate and report the mean and median of the total number of steps taken per day


```r
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
```

```
##          date       mean    median
## 1  2012-10-02  0.4375000  63.00000
## 2  2012-10-03 39.4166667 140.14815
## 3  2012-10-04 42.0694444 121.16000
## 4  2012-10-05 46.1597222 154.58140
## 5  2012-10-06 53.5416667 145.47170
## 6  2012-10-07 38.2465278 101.99074
## 7  2012-10-09 44.4826389 134.85263
## 8  2012-10-10 34.3750000  95.19231
## 9  2012-10-11 35.7777778 137.38667
## 10 2012-10-12 60.3541667 156.59459
## 11 2012-10-13 43.1458333 119.48077
## 12 2012-10-14 52.4236111 160.61702
## 13 2012-10-15 35.2048611 131.67532
## 14 2012-10-16 52.3750000 157.12500
## 15 2012-10-17 46.7083333 152.86364
## 16 2012-10-18 34.9166667 152.36364
## 17 2012-10-19 41.0729167 127.19355
## 18 2012-10-20 36.0937500 125.24096
## 19 2012-10-21 30.6284722  96.93407
## 20 2012-10-22 46.7361111 154.71264
## 21 2012-10-23 30.9652778 101.34091
## 22 2012-10-24 29.0104167 104.43750
## 23 2012-10-25  8.6527778  56.63636
## 24 2012-10-26 23.5347222  77.02273
## 25 2012-10-27 35.1354167 134.92000
## 26 2012-10-28 39.7847222 110.17308
## 27 2012-10-29 17.4236111  80.93548
## 28 2012-10-30 34.0937500 110.32584
## 29 2012-10-31 53.5208333 179.23256
## 30 2012-11-02 36.8055556 143.24324
## 31 2012-11-03 36.7048611 117.45556
## 32 2012-11-05 36.2465278 141.06757
## 33 2012-11-06 28.9375000 100.40964
## 34 2012-11-07 44.7326389 135.61053
## 35 2012-11-08 11.1770833  61.90385
## 36 2012-11-11 43.7777778 132.71579
## 37 2012-11-12 37.3784722 156.01449
## 38 2012-11-13 25.4722222  90.56790
## 39 2012-11-15  0.1423611  20.50000
## 40 2012-11-16 18.8923611  89.19672
## 41 2012-11-17 49.7881944 183.83333
## 42 2012-11-18 52.4652778 162.47312
## 43 2012-11-19 30.6979167 117.88000
## 44 2012-11-20 15.5277778  95.14894
## 45 2012-11-21 44.3993056 188.04412
## 46 2012-11-22 70.9270833 177.62609
## 47 2012-11-23 73.5902778 252.30952
## 48 2012-11-24 50.2708333 176.56098
## 49 2012-11-25 41.0902778 140.88095
## 50 2012-11-26 38.7569444 128.29885
## 51 2012-11-27 47.3819444 158.67442
## 52 2012-11-28 35.3576389 212.14583
## 53 2012-11-29 24.4687500 110.10938
```

###What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
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
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
datasteps <- data.frame(median.steps=mediantotalsteps, interval=intervalvector, stringsAsFactors = FALSE)

maxsteps <- datasteps[order(datasteps$median.steps, na.last = NA, decreasing = TRUE),]

result <- head(maxsteps,1)[,2]
```

The 5-minute interval contains the maximum number of steps is 835


###Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
tail(summary(rawactivity),1)[,1]
```

```
## [1] "NA's   :2304  "
```

2. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
meandata <- data.frame(date=as.Date(datetotalsteps, "%Y-%m-%d"), mean=meantotalsteps, stringsAsFactors = FALSE)
```
3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
## mean
meantotalsteps <- vector()
datetotalsteps <- vector()
index <- 1

for (i in 1:length(datevector)){
        meantotalsteps[index] <- mean(rawactivity[rawactivity[, "date"] ==  eval(datevector[i]),][,1])
        datetotalsteps[index] <- as.character(datevector[i])
        index <- index + 1
}

meandata <- data.frame(date=as.Date(datetotalsteps, "%Y-%m-%d"), mean=meantotalsteps, stringsAsFactors = FALSE)

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
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)

   #Do these values differ from the estimates from the first part of the assignment? 


```r
par(mfrow = c(2, 2), pty = "s") ## multiple plots

plot(initialmeandata, type ="h", col = "red", main = "Initial mean steps per day")
plot(initialmedianstepsperday, type ="h", col = "red", main = "Initial median steps per day")
plot(meandata, type ="h", col = "blue" , main = "Final mean steps per day")
plot(medianstepsperday, type ="h", col = "blue" , main = "Final median steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)

   
   #What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
finaltotalstepsperday <- data.frame(meandata$date,sumstepsperday,stringsAsFactors = FALSE)

par(mfrow= c(1,2))

plot(sumdata, type = "h", col = "blue", main = "Total number of steps \n Cleaned")
plot(finaltotalstepsperday, type = "h", col = "red", main = "Total number of steps \n Missing data imputed")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)

###Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

```r
weeklyactivity <- rawactivity

dayoftheweek <- vector()
```
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
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
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
par(mfrow = c(1,2))

plot(weekdaysubset$interval, weekdaysubset$steps, main = "WeekDays", type = "l", col = "blue")
plot(weekendsubset$interval, weekendsubset$steps, main = "WeekEnds" , type = "l", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)


