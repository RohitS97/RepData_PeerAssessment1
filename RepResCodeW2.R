#1. Code for reading in the dataset and/or processing the data
df<-read.csv("activity.csv")
head(df)

#2. Histogram of the total number of steps taken each day
hist(df$steps)
hist(log10(df$steps+1))

#3. Mean and median number of steps taken each day
mean(df$steps,na.rm = T)
median(df$steps,na.rm = T)

#4. Time series plot of the average number of steps taken
install.packages("dplyr")
library(dplyr)

df1<-df
df1$date<- strptime(df1$date,format = "%Y-%m-%d")

df2<-df1 %>% group_by(interval) %>% summarise(mean=mean(steps,na.rm = T))
plot(df2$interval,df2$mean,xlab = "5 min interval",ylab = "Avg Steps",type="l")

#5. The 5-minute interval that, on average, contains the maximum number of steps
df2[df2$mean==max(df2$mean),"interval"]

#6. Code to describe and show a strategy for imputing missing data
sum(is.na(df1$steps))
df1$steps<-ifelse(is.na(df1$steps),median(df1$steps,na.rm = T),df1$steps)
sum(is.na(df1$steps))

#7. Histogram of the total number of steps taken each day after missing values are imputed
hist(df1$steps)
hist(log10(df1$steps+1))


#8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
df3<-df1
df3$day <- weekdays(df3$date)
df3$weekend <-ifelse(df3$day == "Saturday"|df3$day == "Sunday","Weekend","weekday")
library(ggplot2)
df4<-df3 %>% group_by(weekend,interval) %>% summarise(Avg_steps=mean(steps,na.rm = T))
qplot(interval,Avg_steps,data=df4,facets=.~weekend,geom = "line")