library(xts)
library(forecast)
library(ggplot2)
library(timeSeries)
library(dynlm)


# create a data object frame 
data = read.csv("C:/Users/dhruv/Downloads/dlag.csv")

## create ts object: annual data from 1957-2021

y=ts(data$y, start=1957, end=2021, frequency = 1)
x1=ts(data$x1, start=1957, end=2021, frequency = 1)
x2=ts(data$x2, start=1957, end=2021, frequency = 1)


y=log(y)
x1=log(x1)
x2=log(x2)


fit=tslm(y~x1+x2)


summary(fit)


## get last three years of data

x1past=window(x1,2018)
x2past=window(x2,2018)

newdata_s1=data.frame(x1=rep(mean(x1past),3), x2=rep(mean(x2past),3))


s1=forecast(fit, newdata=newdata_s1, level=95)

newdata_s2=data.frame(x1=1.01*rep(mean(x1past),3), x2=1.01*rep(mean(x2past),3))

s2=forecast(fit, newdata=newdata_s2, level=95)

newdata_s3=data.frame(x1=0.99*rep(mean(x1past),3), x2=0.99*rep(mean(x2past),3))
s3=forecast(fit, newdata=newdata_s3, level=95)

## put forecasts of all three scenarios in 3 rows of the same window
par(mfrow=c(3,1))
par(mar=c(2,2,2,2))
plot(s1,include=25,  main="Scenario 1")
plot(s2,include=25,  main="Scenario 2")
plot(s3,include=25,  main="Scenario 3")

