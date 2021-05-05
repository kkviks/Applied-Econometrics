library(lubridate)
iratestseries <- ts(irates, frequency = 52, start = c(1990,15))

plot.ts(iratestseries)

iratestseriescomponents <- decompose(iratestseries)
plot(iratestseriescomponents)

iratestseriesadj <- iratestseries - iratestseriescomponents$seasonal
# iratestseriesadj <- iratestseriesSMA3 - iratestseriescomponents$seasonal
plot.ts(iratestseriesadj)


iratestseriesdiff1 <- diff(iratestseries, differences=1)
plot.ts(iratestseriesdiff1)

library(TTR)
iratestseriesSMA <- SMA(iratestseries,n=10)
plot.ts(iratestseriesSMA)
View(iratestseriesSMA)

auto.arima(iratestseriesSMA,ic="bic")
library(aTSA)
library(urca)
#ARIMA(2,1,0)
iratestseriesarima1<-arima(iratestseries, order=c(2,1,0))
iratestseriesarima1


#Forcast
library(forecast)
iratesforecasts1 <- forecast(iratestseriesarima1, h=10, level=c(99.5))     # 99.5%  CI
iratesforecasts1
