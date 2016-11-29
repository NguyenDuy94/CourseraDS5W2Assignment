library(dplyr)
library(lubridate)
temporaryFiles <- tempfile()
download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temporaryFiles)
activityRaw <- read.csv(unz(temporaryFiles, "activity.csv"))
activity <- activityRaw[!is.na(activity[,1]), ]
unlink(temporaryFiles)

tbl_df(activity) %>% group_by(date) -> activity2
activitySum <- summarise(activity2, sum(steps))
hist(activitySum[,2][[1]], main = "Histogram of step each day", xlab = "Number of step")
mean(activitySum[,2][[1]], na.rm = TRUE)
median(activitySum[,2][[1]], na.rm = TRUE)

activity3 <- group_by(activity, interval)
intervalMean <- summarise(activity3, mean(steps, na.rm = TRUE))
plot(intervalMean[,2][[1]], type = "l", main = "Average daily pattern activity", 
     xlab = "5-minute interval", ylab = "Average number of steps")
intervalMean[intervalMean[,2] == max(intervalMean[,2]),1]

#length(unique(activityRaw[,3]))
activityRaw[which(is.na(activityRaw[,1])), ] %>% 
        mutate(location = which(is.na(activityRaw[,1]))) -> NAData
NAData[,1] <- sapply(NAData[,3], function(x)
        {intervalMean[which(intervalMean[,1][[1]] == x),2][[1]]})
activityProxied <- activityRaw
activityProxied[NAData$location, 1] <- NAData$steps

activity4 <- mutate(activityProxied, daytype = weekdays(ymd(activityProxied$date)))
activity4[(activity4$daytype == "Sunday"| activity4$daytype == "Saturday"), "daytype"] <- "weekend"
activity4[activity4$daytype != "weekend", "daytype"] <- "weekday"
activity4 <- group_by(activity4, daytype, interval)
daytypeIntMean <- summarise(activity4, mean(steps))
colnames(daytypeIntMean) <- c("daytype", "interval", "averageStep")
qplot(x = interval, y = averageStep, data = daytypeIntMean, 
      facets = daytype~., geom = "line", main = "Average Step during Weekday or Weekend")