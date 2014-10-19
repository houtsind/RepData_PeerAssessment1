#download the raw data file
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp)
unlink(temp)

# reading in the data
unzip("repdata-data-activity.zip", overwrite=TRUE)
pa1.raw.data <- read.csv("activity.csv")



## initial exploration of the data
# load the packages

#install.packages("kernlab")
library("kernlab")
#install.packages("knitr")
library("knitr")
#install.packages("ggplot2")
library("ggplot2")
#install.packages("plyr")
library("plyr")

#confirm library has been loaded
sessionInfo()

str(pa1.raw.data)
head(pa1.raw.data)

# expected # of rows per day 288 (24 * 60 / 5)
unique(tapply(pa1.raw.data$interval, pa1.raw.data$date, length))[[1]]

pa1.date <- as.Date(pa1.raw.data$date, format = "%Y-%m-%d")
pa1.time <- sprintf("%04d", pa1.raw.data$interval)
date.time <- strptime(paste(pa1.date, pa1.time), format = "%Y-%m-%d %H%M")

pa1.time.series <- data.frame(date.time, pa1.raw.data$steps)
colnames(pa1.time.series) <- c("date.time", "steps")

pa1.data.time <- data.frame(pa1.time, pa1.raw.data$steps)
colnames(pa1.data.time) <- c("time", "steps")

1) single r markdown document
2) processable by knitr
3) transformed into an HTML file

use echo = TRUE
for plotting, use ggplot2 (self-selected)

## loading and proprocessing the data
# 1. load the data
# 2. process/transform the data into format suitable for analysis

## what is the mean total number of steps taken per day?
# 1. histogram of the total number of steps taken per day
pa1.steps.date <- aggregate(steps ~ date, data = pa1.raw.data, sum)
barplot(pa1.steps.date$steps, names.arg = pa1.steps.date$date, xlab = "Date", ylab = "Steps", main = "Total Steps by Day")



# 2. calculate and report the mean and median total number of steps taken per day

#mean # of steps
mean(pa1.steps.date$steps)

#median # of steps
median(pa1.steps.date$steps)



# example: df_agg_date <- aggregate(steps ~ date, data = df, FUN = sum) > summary(df_agg_date$steps)

## What is the average daily activity pattern?
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#    and the average number of steps taken, averaged across all days (y-axis)

aggregate (steps ~ time, pa1.data.time, summarize, mean)

pa1.time.mean <- ddply(pa1.data.time, .(interval=time), summarize, mean_value = mean(steps, na.rm=TRUE))

ggplot(pa1.time.mean, aes(x=as.numeric(as.character(time)),y=mean_value)) +
        ylab("Average (Mean) # of Steps Take") +
        xlab("Time Interval (5 min)") +
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = seq (0,2880,by=480))



# 2. Which 5-minute interval, on average across all the days in the dataset, 
#    contains the maximum number of steps?

head(arrange(pa1.time.mean,desc(mean_value)),1)



## Inputing missing values
# 1. Calculate and report the total number of missing values in the dataset 
#    (i.e. the total number of rows with NAs)

sum(is.na(pa1.raw.data$steps))



# 2. Devise a strategy for filling in all of the missing values in the dataset. 

# The mean for each 5-minute interval will be used to substitute into the missing values.

#    The strategy does not need to be sophisticated. For example, you could use 
#    the mean/median for that day, or the mean for that 5-minute interval, etc.

pa1.steps.interval <- aggregate(steps ~ interval, pa1.raw.data, mean)

pa1.data2 <- merge(pa1.raw.data, pa1.steps.interval, by="interval")
pa1.data2.isNAindex <- is.na(pa1.data2$steps.x)
pa1.data2$steps.x[pa1.data2.isNAindex] <- pa1.data2$steps.y[pa1.data2.isNAindex]



# 3. Create a new dataset that is equal to the original dataset but with the 
#    missing data filled in.

pa1.data2 <- pa1.data2[,1:3]
colnames(pa1.data2)[2] <- "steps"



# 4. Make a histogram of the total number of steps taken each day and Calculate 
#    and report the mean and median total number of steps taken per day. 
#    Do these values differ from the estimates from the first part of the assignment? 
#    What is the impact of imputing missing data on the estimates of the total 
#    daily number of steps?
        
pa1.data2.steps.date <- aggregate(steps ~ date, pa1.data2, sum)

ggplot(pa1.data2.steps.date, aes(x=date, y=steps)) + 
        geom_histogram(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("Date") +
        ylab("Steps") +
        labs(title="Total Steps by Date (No Missing Values)")
        
pa1.diff <- merge(pa1.steps.date, pa1.data2.steps.date, by="date", all = TRUE)

# mean
round(mean(pa1.data2.steps.date$steps))

# median
round(median(pa1.data2.steps.date$steps))



## Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
# 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

pa1.data3.steps.date<- cbind(pa1.data2.steps.date,day="")

day <- function(date) {
        if(weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
                "weekend"
        } else {
                "weekday"
        }
}

pa1.data3.steps.date$day <- as.factor(sapply(pa1.data2.steps.date$date, day))




pa1.data3 <- pa1.data2
pa1.data3$day <- as.factor(sapply(pa1.data3$date, day))



# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

pa1.data3 <- pa1.data3[,c(1:2,4)]

par(mfrow = c(2,1))

pa1.data3p2 <- aggregate(steps ~ interval, data=pa1.data3, subset=pa1.data3$day=="weekday", FUN=mean)
plot(pa1.data3p2, type="l", main=type)
title("Weekday", xlab = "Interval", ylab = "Steps")

pa1.data3p2 <- aggregate(steps ~ interval, data=pa1.data3, subset=pa1.data3$day=="weekend", FUN=mean)
plot(pa1.data3p2, type="l", main=type, xlab = "Minutes")
title("Weekend", xlab = "Interval", ylab = "Steps")



## Submitting the Assignment
# 1. Commit the your completed PA1_template.Rmd file to the master branch of your git repository (you should already be on the master branch unless you created new ones)
# 2. Commit your PA1_template.md and PA1_template.html files produced by processing your R markdown file with knit2html() function in R (from the knitr package) by running the function from the console.
# 3. If your document has figures included (it should) then they should have been placed in the figure/ directory by default (unless you overrided the default). Add and commit the figure/ directory to yoru git repository so that the figures appear in the markdown file when it displays on github.
# 4. Push your master branch to GitHub.
# 5. Submit the URL to your GitHub repository for this assignment on the course web site.

## Get and submit the SHA-1 hash