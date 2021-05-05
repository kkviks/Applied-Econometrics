# Installing Packages
install.packages ("dynlm") #for the `dynlm()` function
library (dynlm)
install.packages ("knitr") #for kable()
library (knitr)
install.packages ("broom") #for `glance(`) and `tidy()`
library (broom)
library(lmtest) #for `coeftest()` and `bptest()`.
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(forecast)

#Importing Dataset
library(readxl)
Moodies<- read_excel("Moodies - DLags.xlsx")
View(Moodies)

# Converting into time series object & filtering its columns
MoodiesTS <- ts(Moodies, start=c(1990,15), frequency = 52)
yieldTS <- MoodiesTS[,2]
irTS <- MoodiesTS[,3]

# Checking for Stationarity
plot(yieldTS)
stationary.test(yieldTS)

plot(irTS)
stationary.test(irTS)

#Yield is non-stationary so taking first difference
yieldTSDiff1 <- diff(yieldTS, differences=1)
plot.ts(yieldTSDiff1)
stationary.test(yieldTSDiff1)

#Interest Rate is non-stationary so taking first difference
irTSDiff1 <- diff(irTS, differences=1)
plot.ts(irTSDiff1)
stationary.test(irTSDiff1)


#Checking if TS
is.ts(MoodiesTS)

#Viewing data
Y <- MoodiesTS[,2]
i <- MoodiesTS[,3]

MoodiesTab <- cbind( Y,
                     i,
                    yieldTSDiff1,
                    irTSDiff1,
                    lag(yieldTSDiff1,-1),
                    lag(irTSDiff1,-1),
                    lag(irTSDiff1,-2),
                    lag(irTSDiff1,-3)
                   )
                    
                    
kable(head(MoodiesTab), caption="The dataset with lags and diff",
      col.names = c("Y","i","DY","Di","DYt1","Dit1","Dit2","Dit2"))

#Running FDL
#Lag 0
MoodiesL0.dyn <- dynlm(d(Y) ~ L(d(i)), data=MoodiesTS)
summary(MoodiesL0.dyn)
kable(tidy(summary(MoodiesL0.dyn)),digits=4,caption="0 Lag FDL")

#Lag 1
MoodiesL1.dyn <- dynlm(d(Y) ~ L(d(i),0:1), data=MoodiesTS)
summary(MoodiesL1.dyn)
kable(tidy(summary(MoodiesL1.dyn)),digits=4,caption="1 Lag FDL")

#Lag 2
MoodiesL2.dyn <- dynlm(d(Y) ~ L(d(i),0:2), data=MoodiesTS)
summary(MoodiesL2.dyn)
kable(tidy(summary(MoodiesL2.dyn)),digits=4,caption="2 Lag FDL")

#Lag 3
MoodiesL3.dyn <- dynlm(d(Y) ~ L(d(i),0:3), data=MoodiesTS)
summary(MoodiesL3.dyn)
kable(tidy(summary(MoodiesL3.dyn)),digits=4,caption="3 Lag FDL")

#Lag 4
MoodiesL4.dyn <- dynlm(d(Y) ~ L(d(i),0:4), data=MoodiesTS)
summary(MoodiesL4.dyn)
kable(tidy(summary(MoodiesL4.dyn)),digits=4,caption="4 Lag FDL")

#Choosing better model using AIC/BIC
glL0 <- glance(MoodiesL0.dyn)[c("r.squared","statistic","AIC","BIC")]
glL1 <- glance(MoodiesL1.dyn)[c("r.squared","statistic","AIC","BIC")]
glL2 <- glance(MoodiesL2.dyn)[c("r.squared","statistic","AIC","BIC")]
glL3 <- glance(MoodiesL3.dyn)[c("r.squared","statistic","AIC","BIC")]
glL4 <- glance(MoodiesL4.dyn)[c("r.squared","statistic","AIC","BIC")]

tabl <- rbind(glL0,as.numeric(glL1),as.numeric(glL2)
              ,as.numeric(glL3),as.numeric(glL4))
kable(tabl,caption="Goodness of fit for models")

# choosen lag 1 

#----------------------Serial Correlation----------------------------
ggL1 <- data.frame(cbind(irTSDiff1),lag(irTSDiff1,-1))
names(ggL1) <- c("Di","DiL1")
plot(ggL1)

meanI <- mean(ggL1$Di,na.rm=TRUE)
abline(v=meanI,Ity=2)
abline(h=mean(ggL1$DiL1, na.rm = TRUE),Ity=2)

#-----------------------------------------------------
acf(irTSDiff1)

#-----------------------------------------------------
#Checking for auto-correlation of error term

summary(MoodiesL1.dyn) #Model of interest
#Dynamic model already fitted

ehat <- resid(MoodiesL1.dyn)
kable(tidy(MoodiesL1.dyn),caption="Summary of Lag 1 Model")

#Is the p-value reliable?
plot(ehat)
abline(h=0,Ity=2)
corrgm <- acf(ehat)
plot(corrgm)

#Relationship is not reliable!

#Lagrange Multiplier Test!

#Breush-Godfrey Test for autocorrelation function or bgtest()
# To test for using LM Test

a<-bgtest(MoodiesL1.dyn,order=1,type="F",fill=0)
b<-bgtest(MoodiesL1.dyn,order=1,type="F",fill=NA)
c<-bgtest(MoodiesL1.dyn,order=4,type="Chisq",fill=0)
d<-bgtest(MoodiesL1.dyn,order=4,type="Chisq",fill=NA)

dfr<-data.frame(rbind(
  a[c(1,2,4)],
  b[c(1,2,4)],
  c[c(1,2,4)],
  d[c(1,2,4)]
))

dfr<-cbind(c(
  "1,F,0",
  "1,F,NA",
  "4,Chisq,0",
  "4,Chisq,NA"
),dfr)

names(dfr) <- c("Method","Statistic","Parameters","p-Value")
kable(dfr,caption="Breusch-Godfrey test")


#Null hypothesis rejected => Auto-correlation exists!

#Durbin-Watson test
install.packages("car")
library(car)
dwtest(MoodiesL1.dyn)

#Null hypothesis rejected => Auto-correlation exists!
#MoodiesL1.dyn <- dynlm(d(Y) ~ L(d(i),0:1), data=MoodiesTS)
Moodies1L1.gen <- dynlm(
  d(Y) ~ L(d(Y),1:1)+ L(d(i),0:1), data=MoodiesTS)
s.gen <- summary(Moodies1L1.gen)
kable(tidy(Moodies1L1.gen), caption="1 L 1 Model")

Moodies2L1.gen <- dynlm(
  d(Y) ~ L(d(Y),1:2)+ L(d(i),0:1), data=MoodiesTS)
s.gen <- summary(Moodies2L1.gen)
kable(tidy(Moodies2L1.gen), caption="2 L 1 Model")

Moodies3L1.gen <- dynlm(
  d(Y) ~ L(d(Y),1:3)+ L(d(i),0:1), data=MoodiesTS)
s.gen <- summary(Moodies3L1.gen)
kable(tidy(Moodies3L1.gen), caption="3 L 1 Model")

Moodies4L1.gen <- dynlm(
  d(Y) ~ L(d(Y),1:4)+ L(d(i),0:1), data=MoodiesTS)
s.gen <- summary(Moodies4L1.gen)
kable(tidy(Moodies4L1.gen), caption="4 L 1 Model")


#How many lags to use in AR model?
res.Moodies2L1.gen <- resid(Moodies2L1.gen)
acf(res.Moodies2L1.gen,lag.max=520)

#Bruteforcing using loops
library(dynlm)
aics <- rep(0,8)
bics <- rep(0,8)

for( j in 1:8){
  ari <- dynlm(d(Y) ~ L(d(Y),1:j)+ L(d(i),0:1), start = j, data=MoodiesTS)
  aics[j] <- AIC(ari)
  bics[j] <- BIC(ari)
}

tbl <- data.frame(rbind(aics,bics))
names(tbl) <- c("1","2","3","4","5","6","7","8")
row.names(tbl) <- c("AIC","BIC")
kable(tbl,digits=1,align='c',caption="Lag order selection for ARDL model")

#ARDL(1,1) Final - from aic, bic as well!


#--------Forecasting----------

y <- yieldTSDiff1
y.1ARDL1 <- dynlm(d(Y) ~ L(d(Y),1:1)+ L(d(i),0:1), start = 1, data=MoodiesTS)
kable(tidy(y.1ARDL1),caption="ARDL Lag 1 for DY and Lag 1 for Di")

iInput <- ts()

fcst <- data.frame(forecast(y.1ARDL1, iInput ))

##--------------------------------
#Forecasting 

# Saving NASDAQ forcasted values in a vector
ir.vect.arima <- c(0.05700147 , 0.05581765 , 0.05537731 ,  0.05521157 , 
                   0.05514934 ,0.05512596 ,0.05511717 ,0.05511387 ,
                   0.05511263 ,0.05511217 )

ir.vect.arima.ts <- ts(ir.vect.arima, frequency = 52)
View(ir.vect.arima.ts)

ir.vect.arima.ts.diff <- diff(ir.vect.arima.ts, difference=1)
View(ir.vect.arima.ts.diff)


#X and Y Variables will have to be converted into type vector
irTSDiff1.vect <- c(irTSDiff1)
yieldTSDiff1.vect <- c(yieldTSDiff1)

#Building the ARDL model
rem.p = c(1)
remove = list(p = rem.p)

library(dLagM)
y.ardlDlm1 = ardlDlm(x=irTSDiff1.vect, y=yieldTSDiff1.vect, p=1, q=1)


forecast(y.ardlDlm1 , ir.vect.arima.ts.diff , h = 5 , interval = TRUE, 
         level = 0.95 , nSim = 500)

