YieldTS <- ts(Yield,frequency=52,start=c(1990,15))
YieldTS
plot.ts(YieldTS)

#Additive model is to be used ( from graph)
#Financial series => Trend component will be there!

#Find seasonal time series or non-seasonal time series?
#Non seasonal time series consists of a trend component and irregular compnonent

#Series = Trend + Irregular
# Trend( of non-seasonal of additive model) -> smoothning

#Smoothing
library("TTR")

YieldTSSMA <- SMA(YieldTS, n=52)
plot.ts(YieldTSSMA)

#Financial Series => Seasonal will be there
#Our series = Trend + Seasonal + Irregular
#Decomposition of seasonal series

YieldTSComponents <- decompose(YieldTS)
YieldTSComponents$seasonal

plot(YieldTSComponents)
#Order = Observed, Trend, Seasonal, Random

#Seasonal component is there ( from graph)
#Therefore seasonal + Additive => needs to be seasonally adjusted

YieldSeasonAdj <- YieldTS - YieldTSComponents$seasonal
plot(YieldSeasonAdj)
 # YieldSeasonAdj = Trend + Irregular!

#ARIMA wala part
plot.ts(YieldSeasonAdj)

#Making Stationary
YieldTSDiff1 <- diff(YieldSeasonAdj,differences = 1)
plot.ts(YieldTSDiff1)

#First diff works!
YieldTSDiff <- diff(YieldSeasonAdj,differences = 4)
plot.ts(YieldTSDiff)

#test for stationary
library(forecast)
library(aTSA)
stationary.test(YieldSeasonAdj)
stationary.test(YieldSeasonAdj,method="pp")
stationary.test(YieldTSDiff1)
#=> Stationary Already!

#Checking for correlation b/w successive terms of irregular components
acf(YieldTSDiff1, lag.max = 50)
acf(YieldTSDiff1, lag.max = 20, plot=FALSE)

pacf(YieldTSDiff1, lag.max = 50)
pacf(YieldTSDiff1, lag.max = 20, plot=FALSE)

#auto arima
auto.arima(Yield)
auto.arima(YieldTS)
auto.arima(YieldTSDiff1)
auto.arima(YieldTSDiff1, ic="bic")

#(0,1,2) with drift and (0,0,2) with non-zero mean
YieldTSArima <- arima(YieldTS,order=c(0,1,2))
YieldTSArima
#above better

YieldTSArima2 <- arima(YieldTSDiff1,order=c(0,0,2))
YieldTSArima2

#Forecast
#YieldTSForecast <- forecast(YieldTSArima, h = 52)
#YieldTSForecast
#plot(YieldTSForecast)

YieldTSForecast2 <- forecast(YieldTSArima2, h = 52)
YieldTSForecast2
plot(YieldTSForecast2)
#test
Box.test(YieldTSForecast2$residuals, lag=52, type="Ljung-Box")
hist(YieldTSForecast2$residuals)

YieldTSArima3 <- arima(YieldTSDiff1,order=c(0,0,1))
YieldTSArima3
library(aTSA)
library(urca)
install.packages("forecast")
library(forecast)
YieldTSForecast3 <- forecast(YieldTSArima3, h = 52)
YieldTSForecast3
plot(YieldTSForecast3)
acf(YieldTSForecast3$residuals, lag=20)
Box.test(YieldTSForecast3$residuals, lag=52, type="Ljung-Box")
hist(YieldTSForecast3$residuals)


#Works = (0,0,1) 

#--------------------------------------------------------------------------------------------------

