
# Analysis on FitBit tracker Data

## Code for reading in the dataset and/or processing the data

First unzip the data and read the data into R using read.csv function.

``` r
unzip("repdata_data_activity.zip")
```

    ## Warning in unzip("repdata_data_activity.zip"): error 1 in extracting from zip
    ## file

``` r
activity<- read.csv("activity.csv")
```

Now find the class of the date column.

``` r
class(activity$date)
```

    ## [1] "character"

## Histogram of the total number of steps taken each day

Now, use tapply to find the number of steps taken each day:

``` r
activity_total_steps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(activity_total_steps) <- c("date", "steps")
```

Now, plot the histogram of the total number of steps taken each day:

``` r
png("p1.png")
hist(activity_total_steps[,2],main="Histogram of the total number of steps taken each day",xlab="Steps taken each day")
dev.off()
```

    ## png 
    ##   2

``` r
hist(activity_total_steps[,2],main="Histogram of the total number of steps taken each day",xlab="Steps taken each day")
```

![](PA1_template_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> \#\#
Mean and median number of steps taken each day Claculate the mean value
of the total number of steps taken per day

``` r
a<-mean(activity_total_steps[,2],na.rm = TRUE)
a
```

    ## [1] 9354.23

Calculate the median number of steps taken per day:

``` r
b<-median(activity_total_steps[,2],na.rm = TRUE)
b
```

    ## [1] 10395

## Time series plot of the average number of steps taken

Now groupby the interval and find the mean of the number of steps taken
in that interval for the entire data. This can be done by the aggregate
function.

``` r
average_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)

names(average_daily_activity) <- c("interval", "mean")
```

Make a time series plot with mean steps along y axis and the interval
number along y axis

``` r
png("p2.png")
with(average_daily_activity,plot(interval, mean,type="l", lwd=3, lty=1, col="black", xlab="inetrval number", ylab="Average number of steps", main="Time series plot of the average number of steps taken"))
dev.off()
```

    ## png 
    ##   2

``` r
with(average_daily_activity,plot(interval, mean,type="l", lwd=3, lty=1, col="black", xlab="inetrval number", ylab="Average number of steps", main="Time series plot of the average number of steps taken"))
```

![](PA1_template_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## The 5-minute interval that, on average, contains the maximum number of steps

Now, find the interval which contains the maximum number of steps. Find
this using which.max function

``` r
average_daily_activity[which.max(average_daily_activity$mean),1]
```

    ## [1] 835

## Number of NA’s in a dataset.

We can find the number of NA’s in the data set using sum function over
is.na function.

``` r
sum(is.na(activity$steps))
```

    ## [1] 2304

## To impute the missing values using the mean of the number of steps taken in that day.

We use mean value imputation for simple analysis.

Initially let us find the number of missing values intervals based on
daily basis and based on the interval number basis. Let Q6.1 be the
missing value analysis based on the date. And Q6.2 be the missing value
analysis based on the interval. First make a group by analysis on both
date and interval first.

``` r
Q6<-activity
Q6$Missing<-is.na(Q6$steps)
Q6<-aggregate(data=Q6,Missing~date+interval,FUN="sum")
```

Now find Q6.1 using date as the only group by parameter. And finally
making a data frame of two columns.

``` r
Q6.1<-data.frame(tapply(Q6$Missing,Q6$date,sum))
Q6.1$date<-rownames(Q6.1)
names(Q6.1)<-c("Missing","date")
Q6.1$date<-as.Date(Q6.1$date,format="%Y-%m-%d")
```

Now, find Q6.2 using interval number as the only group by parameter. And
finally making a data frame of two columns.

``` r
Q6.2<-data.frame(tapply(Q6$Missing,Q6$interval,sum))
Q6.2$date<-rownames(Q6.2)
rownames(Q6.2)<-NULL
names(Q6.2)<-c("Missing","Interval")
```

Now, make a graph between the missing values against date initially and
then again against interval number for finding missing value pattern.

``` r
png("p3.png")
par(mfrow=c(1,2))
plot(y=Q6.1$Missing,x=Q6.1$date,main="Missing Value Distribution by Date")
plot(y=Q6.2$Missing,x=Q6.2$Interval,main="Missing Value Distribution by Interval")
dev.off()
```

    ## png 
    ##   2

``` r
par(mfrow=c(1,2))
plot(y=Q6.1$Missing,x=Q6.1$date,main="Missing Value Distribution by Date")
plot(y=Q6.2$Missing,x=Q6.2$Interval,main="Missing Value Distribution by Interval")
```

![](PA1_template_files/figure-gfm/unnamed-chunk-14-1.png)<!-- --> For
every interval, there are consistently 8 missing values. For the date,
there are consistently 288 missing values. And in total, there are 8
dates that have missing value.

Find the dates where missing value have occured do the analysis based on
weekday.

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 4.0.5

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(magrittr)
```

    ## Warning: package 'magrittr' was built under R version 4.0.5

``` r
library(ggplot2)
Q6.3<-as.data.frame(Q6.1) %>% select(date,Missing) %>% arrange(desc(Missing))
Q6.3<-Q6.3[which(Q6.3$Missing!=0),]
Q6.3$Weekday<-wday(Q6.3$date,label=TRUE)
Q6.4<-activity
Q6.4$weekday<-wday(Q6.4$date,label=TRUE)
#Finding the mean of steps every monday, and every interval
Q6.5<-aggregate(data=Q6.4,steps~interval+weekday,FUN="mean",na.rm=TRUE)
```

Merge the pre-imputation table Q6.4 table with the average table Q6.5.

``` r
Q6.6<-merge(x=Q6.4,y=Q6.5,by.x=c("interval","weekday"),by.y=c("interval","weekday"),all.x=TRUE)
```

Conditionally replacing the steps.x column NA value with the values from
steps.y column value.

``` r
Q6.6$Steps.Updated<-0
for (i in 1:dim(Q6.6)[[1]]){
if(is.na(Q6.6[i,3])){Q6.6[i,6]=Q6.6[i,5]}
else {Q6.6[i,6]=Q6.6[i,3]}
}
```

Now simplify the imputed data frame

``` r
Q6.6 <-Q6.6  %>% select(date,weekday,interval,Steps.Updated)
names(Q6.6)[[4]]<-"Steps"
```

## Find the new mean and median number of steps taken in a day.

## Histogram of the total number of steps taken each day after missing values are imputed

``` r
Q6.7 <- data.frame(with(Q6.6, tapply(Steps,date,sum)))
Q6.7$date<-rownames(Q6.7)
names(Q6.7)<-c("steps","date")
Q6.7$date<-as.Date(Q6.7$date,format="%Y-%m-%d")
rownames(Q6.7)<-NULL
# The new mean is
mean(Q6.7[,1])
```

    ## [1] 10821.21

``` r
# The new median is 
median(Q6.7[,1])
```

    ## [1] 11015

``` r
png("p4.png")
qplot(Q6.7[,1],geom="histogram",main="Total steps taken histogram post imputation",xlab="Steps",ylab="Count")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

``` r
dev.off()
```

    ## png 
    ##   2

``` r
qplot(Q6.7[,1],geom="histogram",main="Total steps taken histogram post imputation",xlab="Steps",ylab="Count")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](PA1_template_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

``` r
Q8<-Q6.6
levels(Q8$weekday)<-c(1,2,3,4,5,6,7)
Q8$WDWE<-Q8$weekday %in% c(1,2,3,4,5)
Q8.1<-aggregate(data=Q8,Steps~interval+WDWE,mean,na.rm=TRUE)
Q8.1$WDWE<-as.factor(Q8.1$WDWE)
levels(Q8.1$WDWE)<-c("Weekend","Weekday")
png("p4.png")
ggplot(data=Q8.1,aes(y=Steps,x=interval,group=1,color=WDWE))+geom_line() +scale_x_discrete(breaks = seq(0, 2500, by = 300))+ylab("Mean Steps")+xlab("Intervals")+ggtitle("Mean steps across intervals by Weekend and Weekday")
dev.off()
```

    ## png 
    ##   2

``` r
ggplot(data=Q8.1,aes(y=Steps,x=interval,group=1,color=WDWE))+geom_line() +scale_x_discrete(breaks = seq(0, 2500, by = 300))+ylab("Mean Steps")+xlab("Intervals")+ggtitle("Mean steps across intervals by Weekend and Weekday")
```

![](PA1_template_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
#Producing the panel plot
Q8.1$interval<-as.numeric(as.character(Q8.1$interval))
library(lattice)
xyplot(data=Q8.1,Steps~interval|WDWE, grid = TRUE, type = c("p", "smooth"), lwd = 4,panel = panel.smoothScatter)
```

    ## (loaded the KernSmooth namespace)

![](PA1_template_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->

``` r
library(hexbin)
```

    ## Warning: package 'hexbin' was built under R version 4.0.5

``` r
hexbinplot(data=Q8.1,Steps~interval|WDWE, aspect = 1, bins=50)
```

![](PA1_template_files/figure-gfm/unnamed-chunk-20-3.png)<!-- -->

``` r
png("p5.png")
xyplot(data=Q8.1,Steps~interval|WDWE, grid = TRUE, type = c("p", "smooth"), lwd = 4,panel = panel.smoothScatter)
dev.off()
```

    ## png 
    ##   2

``` r
png("p6.png")
hexbinplot(data=Q8.1,Steps~interval|WDWE, aspect = 1, bins=50)
dev.off()
```

    ## png 
    ##   2
