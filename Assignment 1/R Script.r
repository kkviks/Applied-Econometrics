#Question 1
#Extract city name from address
library(tidyverse)
city<-matrix(nrow=29451, ncol=10)
city<-str_split_fixed(train$ADDRESS, ",", n=10)
city[7195,10]<-city[7195,6]
for(i in 1:29451){
  if(city[i,5]!="" && city[i,10]==""){
    city[i,10]=city[i,5]
  }
}
for(i in 1:29451){
  if(city[i,4]!="" && city[i,10]==""){
    city[i,10]=city[i,4]
  }
}
for(i in 1:29451){
  if(city[i,3]!="" && city[i,10]==""){
    city[i,10]=city[i,3]
  }
}
for(i in 1:29451){
  if(city[i,2]!="" && city[i,10]==""){
    city[i,10]=city[i,2]
  }
}
for(i in 1:29451){
  train$place[i]=city[i,10]
}

####################################
#Question 2 :  Calculate summary statistics of the variables & comment on the same. 

View(econf342_finall)
fdata = econf342_finall
df = data.frame(fdata)

data1$bhk_no = as.factor(data1$bhk_no)
data1$bhk_or_rk = as.factor(data1$bhk_or_rk)
data1$ready_to_move = as.factor(data1$ready_to_move)
data1$resale = as.factor(data1$resale)
data1$tier = as.factor(data1$tier)
data1$state = as.factor(data1$state)
data1$under_construction = as.factor(data1$under_construction)
data1$posted_by = as.factor(data1$posted_by)
data1$posted = as.factor(data1$posted)

smry = summary(data1) 

b = select(data1,place,square_ft)
head(b)
avgSqFt = b %>%  group_by(place) %>% summarise(m = mean(square_ft))
avgSqFt[which.max(avgSqFt$m),]
avgSqFt[which.min(avgSqFt$m),]

write_xlsx(smry_df,"df.xlsx")

dev0 = apply(data1,2,sd)
dev = data.frame(dev0)

write.csv(dev,"dev1.csv")

data = econf342_finall
data$dealer <- ifelse(data$posted == 2, 1, 0)
data$builder <- ifelse(data$posted == 3, 1, 0)

data$tier2 <- ifelse(data$tier == 2, 1, 0)
data$tier3 <- ifelse(data$tier == 3, 1, 0)

data$interestPop = data$interest_payment/data$TotalPopulation
data$loanPop = data$personal_loans_by_scheduled_comm/data$TotalPopulation

df = data.frame(data)

cols1 = c("v1", "price", "avprice", "coord", "cci", "pm2", "interest_payment", "personal_loans_by_scheduled_comm","power_availability", "nh_length", "sh_length", "fixed_capital", "inflation", "construction_worker_wage", "num_factories", "valueaddedbyconstruction", "NetStateDomesticProduct" , "TotalPopulation" , "d_mark", "lprice2" ,"nsdp2", "interestPop", "loanPop" )
inds = c(1,13,23,24,25,27,30,31:39,41,42,46,50,55,56)

skews = c(skewness(df[1]),skewness(df[13]),skewness(df[23]),skewness(df[24]),skewness(df[25]),skewness(df[27]),skewness(df[30]),skewness(df[31]),skewness(df[32]),skewness(df[33]),skewness(df[34]),skewness(df[35]),skewness(df[36]),skewness(df[37]),skewness(df[38]),skewness(df[39]),skewness(df[41]),skewness(df[42]),skewness(df[46]),skewness(df[50]),skewness(df[55]),skewness(df[56]))
sk = data.frame(skews)
write.csv(sk,"skewness.csv")

kurts = c(kurtosis(df[1]),kurtosis(df[13]),kurtosis(df[23]),kurtosis(df[24]),kurtosis(df[25]),kurtosis(df[27]),kurtosis(df[30]),kurtosis(df[31]),kurtosis(df[32]),kurtosis(df[33]),kurtosis(df[34]),kurtosis(df[35]),kurtosis(df[36]),kurtosis(df[37]),kurtosis(df[38]),kurtosis(df[39]),kurtosis(df[41]),kurtosis(df[42]),kurtosis(df[46]),kurtosis(df[50]),kurtosis(df[55]),kurtosis(df[56]))
kurts
kt = data.frame(kurts)
write.csv(kt,"kurtosis.csv")
summary(df["square_ft"])

library(moments)
summary(df$interestPop)
skewness(df$interestPop)
sapply(df$interestPop, std)

library(dplyr)
var = select(df,interestPop, loanPop)
summary(var)
apply(var,2,sd)

library(psych)
skew(df$construction_worker_wage)
kurtosi(df$construction_worker_wage)
skewness(df$square_ft)
kurtosis(df$square_ft)

install.packages("Hmisc")
library(Hmisc)
hist.data.frame(df)




####################################
#Question 3: 

data_final<-read.csv("C:/Users/Ritika Garg/Downloads/econf342_finall.csv")
data3<-data_final
data3$dealer <- ifelse(data3$posted == 2, 1, 0)
data3$builder <- ifelse(data3$posted == 3, 1, 0)
data3$tier2 <- ifelse(data3$tier == 2, 1, 0)
data3$tier3 <- ifelse(data3$tier == 3, 1, 0)
data3$interestPop = data3$interest_payment/data3$TotalPopulation
data3$loanPop = data3$personal_loans_by_scheduled_comm/data3$TotalPopulation
data3$v2<-data3$square_ft*data3$tier2
data3$v3<-data3$square_ft*data3$tier3
data3$sqft_2<-data3$v2
data3$sqft_3<-data3$v3


data3$under_wage<-data3$under_construction*data3$construction_worker_wage
reg1 = lm(lprice2 ~ builder + dealer + under_construction + rera + bhk_no + square_ft + ready_to_move + resale + tier2 + tier3 + avprice + coord + cci + pm2 + interestPop + loanPop + power_availability + nh_length + fixed_capital + inflation + construction_worker_wage + num_factories + d_mark+nsdp2+sqft_2+sqft_3+data3$under_wage, data=data3)
summary(reg1)

df<-data.frame(data3)
df<-df[,c("bhk_no","square_ft","avprice","pm2","d_mark","sqft_2","sqft_3","under_wage")]
str(df)
reg<-cor(df[,sapply(df,is.numeric)],use="complete.obs",method="pearson")
round(reg,2)
reg
pairs(reg,main="Graph Matrix")




####################################
# Question 4: 


####################################
#Question 5:

data = econf342_finall
data$v2 = data$square_ft * data$tier2
data$v3 = data$square_ft * data$tier3
data$sqft_2 = data$v2
data$sqft_3 = data$v3
data$under_wage = data$under_construction * data$construction_worker_wage

a = select(data, builder, dealer, under_construction, rera, bhk_no, square_ft, ready_to_move, resale, tier2, tier3, avprice, coord, cci, pm2, interestPop, loanPop, power_availability, nh_length, fixed_capital, inflation, construction_worker_wage, num_factories, d_mark, nsdp2,sqft_2, sqft_3, under_wage)
head(a)
predictors_df = data.frame(ex_vars)
response_df = data.frame(data$lprice2)

#Real
reg1 <- lm(lprice2 ~ dealer + under_construction + rera + bhk_no + square_ft + ready_to_move + resale + tier2 + tier3 + avprice + coord + cci + pm2 + interestPop + loanPop + power_availability + nh_length + fixed_capital + inflation + construction_worker_wage + num_factories + d_mark+nsdp2+sqft_2+sqft_3+ under_wage, data=data)
summary(reg1)
aic = AIC(reg1)
bic = BIC(reg1)
pred = predict(reg1)

reg_without_state <- lm(lprice2 ~ dealer + under_construction + rera + bhk_no + square_ft + ready_to_move + resale + tier2 + tier3 + avprice + coord + cci + pm2 + d_mark +sqft_2+sqft_3, data=data)
summary(reg_without_state)

reg_without_city <- lm(lprice2 ~ dealer + under_construction + rera + bhk_no + square_ft + ready_to_move + resale + coord + cci  + interestPop + loanPop + power_availability + nh_length + fixed_capital + inflation + construction_worker_wage + num_factories + d_mark+nsdp2 + under_wage, data=data)
summary(reg_without_city)

reg_without_sac = lm(lprice2 ~ dealer + under_construction + rera + bhk_no + square_ft + ready_to_move + resale + coord + d_mark, data=data)
summary(reg_without_sac)

aic = AIC(reg1, reg_without_state,reg_without_city, reg_without_sac)
bic = BIC(reg1, reg_without_state,reg_without_city, reg_without_sac)
class(aic)
merge.data.frame(aic,bic,by = "df",sort = 0)

aic_bic = merge.data.frame(aic,bic,by = "df",sort = 0)
write.csv(aic_bic, "aicBic.csv")





###############################################################

#Question 6 + Question 7:

#Packages which require installation 
install.packages("lmtest")
install.packages("olsrr")
install.packages("caret")
install.packages("kdensity")
install.packages("faraway")
install.packages("ggplot2")
install.packages("mctest")
install.packages("MASS")
install.packages("lattice")
install.packages("e1071")
install.packages("sandwich")
install.packages("robustbase")
install.packages("quantmod")
install.packages("xts")
install.packages("zoo")
install.packages("TTR")

#The dataset has already been imported and saved in the dataframe 'data3'

#CREATING THE REGRESSION MODEL
#The below lines of code are for cleaning of the dataset and manipulation of some variables where it is required
data3$dealer <- ifelse(data3$posted == 2, 1, 0)
data3$builder <- ifelse(data3$posted == 3, 1, 0)
data3$tier2 <- ifelse(data3$tier == 2, 1, 0)
data3$tier3 <- ifelse(data3$tier == 3, 1, 0)
data3$interestPop = data3$interest_payment/data3$TotalPopulation
data3$loanPop = data3$personal_loans_by_scheduled_comm/data3$TotalPopulation
data3$v2<-data3$square_ft*data3$tier2
data3$v3<-data3$square_ft*data3$tier3
data3$sqft_2<-data3$v2
data3$sqft_3<-data3$v3
data3$under_wage<-data3$under_construction*data3$construction_worker_wage

#The below code creates a regression model named 'reg1'
reg1 <- lm(lprice2 ~ dealer + under_construction + rera + bhk_no + square_ft + ready_to_move + resale + tier2 + tier3 
			+ avprice + coord + cci + pm2 + interestPop + loanPop + power_availability + nh_length + fixed_capital + inflation 
			+ construction_worker_wage + num_factories + d_mark+nsdp2+sqft_2+sqft_3+data3$under_wage, data=data3)


#A.6 TEST FOR DETECTING HETEROSCEDASTICTY

#A.6.1 Graphical Method of Detecting Heteroscedasticity
par("mar")# init 4 charts in 1 panel
par(mar=c(1,1,1,1))
plot(reg1)

#A.6.2 Breush-Pagan Statistical Test of Detecting Heteroscedasticity
install.packages("olsrr")      #Dont run this line if package already installed  
update.packages("olsrr") 
library(olsrr)
ols_test_breusch_pagan(reg1) #BP Test  

#A.6.3 F-Test of Detecting Heteroscedasticity
library(olsrr)
ols_test_f(reg1)


#A.7 RECTIFICATION OF HETEROSCEDASTICTY

#A.7.1 The Box-Cox Transformation
install.packages("caret")      #Dont run this line if package already installed
update.packages("caret")
library(caret)
lpriceBCMod <- caret::BoxCoxTrans(data3$lprice2)
print(lpriceBCMod)
BC_lprice2 <- ((data3$lprice2)^(-0.8) - 1)/(-0.8)     #Transforming the Y variable
data3 <- cbind(data3, BC_lprice2)
reg_BC <- lm(BC_lprice2 ~ dealer + under_construction + rera + bhk_no + square_ft + ready_to_move + resale + tier2 
			+ tier3 + avprice + coord + cci + pm2 + interestPop + loanPop + power_availability + nh_length + fixed_capital 
			+ inflation + construction_worker_wage + num_factories + d_mark+nsdp2+sqft_2+sqft_3+data3$under_wage, data=data3)
library(lmtest)
lmtest::bptest(reg_BC)  # Breusch-Pagan test

#A.7.2 Using Robust Standard Errors 
install.packages("robustbase")    #Dont run this line if package already installed
update.packages("robustbase")
library(robustbase)
reg_rob <- lmrob(lprice2 ~ dealer + under_construction + rera + bhk_no + square_ft + ready_to_move + resale + tier2 
			+ tier3 + avprice + coord + cci + pm2 + interestPop + loanPop + power_availability + nh_length + fixed_capital 
			+ inflation + construction_worker_wage + num_factories + d_mark+nsdp2+sqft_2+sqft_3+data3$under_wage, data=data3)
summary(reg_rob)

#A.7.3 Using Sandwich package
install.packages("sandwich")     #Dont run this line if package already installed
update.packages("sandwich")
library(sandwich)
coeftest(reg1, vcov = vcovHC(reg1, "HC1"))    # robust; HC1 (This line of code produces same results when we use the 'robust' keyword in stata)







#B.6 TEST FOR DETECTING MULTICOLLINEARITY
#B.6.1 The VIF method
install.packages("mctest")            #Dont run this line if package already installed
update.packages("mctest")
library(mctest)
imcdiag(reg1, vif=10)


#B.7 RECTIFICATION OF MULTICOLLINEARITY
#B.7.1 Dropping the variables having high VIF values
install.packages("mctest")			#Dont run this line if package already installed
update.packages("mctest")
library(mctest)
reg_mult <- lm(lprice2 ~ dealer + rera + bhk_no + square_ft + resale + tier2 + tier3 + avprice + coord + cci + pm2 
			+ interestPop + nh_length + fixed_capital + inflation + construction_worker_wage + num_factories + d_mark+nsdp2+sqft_2+sqft_3, data=data3)
#In the above line of code, we have dropped the following variables 'under_construction', 'loanPop', 'power_availability', 'under_wage', 'ready_to_move'
imcdiag(reg_mult, vif=10)








#C.6 TEST FOR DETECTING NORMALITY OF RESIDUALS
#C.6.1 By Plotting K-Density graphs of residuals
install.packages("ggplot2")			#Dont run this line if package already installed
update.packages("ggplot2")
install.packages("kdensity")		#Dont run this line if package already installed
update.packages("kdensity")
library(kdensity)
kden = density(reg1$residuals, from=-4, to=4)
plot(kden, main="K-Density Plot for Residuals")

#C.6.2 Shapiro-Wilk Test for Normality
shapiro.test(reg1$residuals[0:5000])     #R accepts only less than 5000 observations



#C.7 ALTERING THE MODEL IF THE RESIDUALS ARE NON-NORMAL
#We adopt three transformations to address the above issue

#C.7.1 Logarithmic Tranformation
log_price2 <- log10(data3$lprice2)
data3 <- cbind(data3, log_price2)
reg_Norm_Log <- lm(log_price2 ~ dealer + under_construction + rera + bhk_no + square_ft + ready_to_move + resale + tier2 
				+ tier3 + avprice + coord + cci + pm2 + interestPop + loanPop + power_availability + nh_length + fixed_capital 
				+ inflation + construction_worker_wage + num_factories + d_mark+nsdp2+sqft_2+sqft_3+data3$under_wage, data=data3)
shapiro.test(reg_Norm_Log$residual[0:5000])

#C.7.2 Square-root Tranformation
sqroot_price2 <- sqrt(data3$lprice2)
data3 <- cbind(data3, sqroot_price2)
reg_Norm_Sqrt<- lm(sqroot_price2 ~ dealer + under_construction + rera + bhk_no + square_ft + ready_to_move + resale + tier2 
				+ tier3 + avprice + coord + cci + pm2 + interestPop + loanPop + power_availability + nh_length + fixed_capital 
				+ inflation + construction_worker_wage + num_factories + d_mark+nsdp2+sqft_2+sqft_3+data3$under_wage, data=data3)
shapiro.test(reg_Norm_Sqrt$residuals[0:5000])

#C.7.3 Inverse Transformation
inverse_price2 <- 1/(data3$lprice2)
data3 <- cbind(data3, inverse_price2)
reg_Norm_inv <- lm(inverse_price2 ~ dealer + under_construction + rera + bhk_no + square_ft + ready_to_move + resale + tier2 
				+ tier3 + avprice + coord + cci + pm2 + interestPop + loanPop + power_availability + nh_length + fixed_capital 
				+ inflation + construction_worker_wage + num_factories + d_mark+nsdp2+sqft_2+sqft_3+data3$under_wage, data=data3)
shapiro.test(reg_Norm_inv$residuals[0:5000])








#D.6 TESTS FOR DETECTING OMITTED VARIABLE BIAS
#D.6.1 Ramsey Reset Test
install.packages("quantmod")
install.packages("xts")
install.packages("zoo")
install.packages("TTR")
library(lmtest)
resettest(reg1, power=2,  type='fitted')


#D.7 ALTERING THE MODEL IF OMITTED VARIABLE BIAS EXISTS
#D.7.1 Introducing squares of three variables namely 'bhk_no', 'square_ft', 'd_mark'
install.packages("quantmod")
install.packages("xts")
install.packages("zoo")
install.packages("TTR")
library(lmtest)

bhk_sqr <- (data3$bhk_no)^(2)
sqft_sqr <- (data3$square_ft)^(2)
dmark_sqr <- (data3$d_mark)^(2)

data3 <- cbind(data3, bhk_sqr, sqft_sqr, dmark_sqr)
reg_OVB <- lm(lprice2 ~ dealer + under_construction + rera + bhk_no + square_ft + ready_to_move + resale + tier2 + tier3 + avprice + coord + cci + pm2 + interestPop + loanPop + power_availability + nh_length + fixed_capital + inflation + construction_worker_wage + num_factories + d_mark+nsdp2+sqft_2+sqft_3+data3$under_wage + bhk_sqr+dmark_sqr+sqft_sqr, data=data3)
resettest(reg_OVB, power=2,  type='fitted')

 
 
 
####################################
#Question 8: 
library(car)
linearHypothesis(reg1, c("tier2=0", "tier3=0"), test="F")
linearHypothesis(reg1, "sqft_2-sqft_3=0", test="F")


####################################
#Question 9: 
ypredicted<-predict(reg1)
data4<-subset(data3, state!="Uttarakhand")
data4<-subset(data4, state!="Chattisgarh")
data4<-subset(data4, state!="Goa")
data4<-subset(data4, state!="Jharkhand")
data4<-subset(data4, state!="Telangana")
data4$lprice_predict<-ypredicted
ggplot(data4, aes(x=lprice2, y=lprice_predict))+geom_point()+geom_smooth()






####################################
#End 




