library(data.table)
library(ggplot2)
library(dplyr)
require(mgcv)
library(fpp)
require(gratia)
library(readxl)
library(lubridate)
library(forecast)
library(zoo)
library(stringr)
library(stats)
library(urca)

g13<-read_excel("g13.xlsx")
dat<-as.data.table(g13)
yuzt<-dat[1:372,]
mendil<-dat[373:744,]
xiaomi<-dat[745:1116,]
fakir<-dat[1117:1488,]
tayt<-dat[1489:1860,]
bik1<-dat[1861:2232,]
firca<-dat[2233:2604,]
mont<-dat[2605:2976,]
bik2<-dat[2977:3348,]

#---------------------------------------------------------------------------------------------------------
# Codes of Product 1


ts.plot(yuzt$sold_count)
acf(yuzt$sold_count, lag=100 ,main="Yüz Temizleyici")


ts11=ts(yuzt$sold_count, frequency = 7)
decomposed11= decompose(ts11,type="multiplicative")
deseasonalised11=ts11/decomposed11$seasonal
detrend11=deseasonalised11/decomposed11$trend
acf(detrend11,na.action = na.pass, main="Decomposed ACF freq=7")
plot(decomposed11)

ts12=ts(yuzt$sold_count, frequency = 15)
decomposed12= decompose(ts12,type="multiplicative")
deseasonalised12=ts12/decomposed12$seasonal
detrend12=deseasonalised12/decomposed12$trend
acf(detrend12,na.action = na.pass, main="Decomposed ACF freq=15")
plot(decomposed12)

ts13=ts(yuzt$sold_count, frequency = 30)
decomposed13= decompose(ts13,type="multiplicative")
deseasonalised13=ts13/decomposed13$seasonal
detrend13=deseasonalised13/decomposed13$trend
acf(detrend13,na.action = na.pass, main="Decomposed ACF freq=30")
plot(decomposed13)

tsdisplay(decomposed11$random, lag=30, main="Random")


model11=arima(decomposed11$random,order=c(3,0,0))
print(model11)
fitted11=fitted(model11)
fitted_transformed11=fitted11*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed11,type = "l",col="blue")
plot(residuals(model11), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted11,main="AR(3)")

model12=arima(decomposed11$random,order=c(0,0,3))
print(model12)
fitted12=fitted(model12)
fitted_transformed12=fitted12*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed12,type = "l",col="blue")
plot(residuals(model12), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted12, main="MA(3)")


model13=auto.arima(decomposed11$random,seasonal = FALSE)
print(model13)
model13=arima(decomposed11$random,order=c(3,0,2)) # Manuel version of auto.arima (same model)
fitted13=fitted(model13)
fitted_transformed13=fitted13*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed13,type = "l",col="blue")
plot(residuals(model13), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted13,main="ARIMA(3,0,2)")


AIC(model11)
BIC(model11)
AIC(model12)
BIC(model12)
AIC(model13)
BIC(model13)


accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  MAPE=sum(abs(error/actual))/n
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,MAPE,MAD,MADP,WMAPE)
  return(l)
}

accu(ts11,tail(head(fitted_transformed11,369),366))
accu(ts11,tail(head(fitted_transformed12,369),366))
accu(ts11,tail(head(fitted_transformed13,369),366))


correlation=function(dt){  # Function to compute correlation between sold counts and other columns in the data
  dt=as.data.frame(dt)
  a=data.frame(data= NA, nrow=8)
  colnames(a)=c("Variable","Correlation")
  a[1,1]="visit count"
  a[2,1]="basket count"
  a[3,1]="favored count"
  a[4,1]="category sold"
  a[5,1]="category visit"
  a[6,1]="category basket"
  a[7,1]="category favored"
  a[8,1]="category brand sold"
  for(i in 1:8) {
    a[i,2]=cor(dt[,4],dt[,i+4])
  }
  return (a)
}
cor(yuzt$sold_count,yuzt$price)
correlation(yuzt)

#mat=matrix(data=NA, nrow=372,ncol=3)   These 4 lines are unnecessary, they are created just in case
#mat[,1]=yuzt$basket_count
#mat[,2]=yuzt$category_sold
#mat[,3]=yuzt$category_favored
#modelx1=arima(decomposed11$random,order=c(3,0,2),xreg=mat)

modelx1=arima(decomposed11$random,order=c(3,0,2),xreg=yuzt$basket_count)
print(modelx1)
fittedx1=fitted(modelx1)
fitted_transformedx1=fittedx1*decomposed11$seasonal*decomposed11$trend

modelx2=arima(decomposed11$random,order=c(3,0,2),xreg=yuzt$category_sold)
print(modelx2)
fittedx2=fitted(modelx2)
fitted_transformedx2=fittedx2*decomposed11$seasonal*decomposed11$trend


modelx3=arima(decomposed11$random,order=c(3,0,2),xreg=yuzt$category_favored)
print(modelx3)
fittedx3=fitted(modelx3)
fitted_transformedx3=fittedx3*decomposed11$seasonal*decomposed11$trend

accu(ts11,tail(head(fitted_transformed13,369),7)) # arima model
accu(ts11,tail(head(fitted_transformedx1,369),7)) # arimax1
accu(ts11,tail(head(fitted_transformedx2,369),7)) # arimax2
accu(ts11,tail(head(fitted_transformedx3,369),7)) # arimax3

#--------------------------------------------------------------------------------------------------------
# Codes of Product 2

ts.plot(mendil$sold_count)
acf(mendil$sold_count, lag=100 ,main="Islak Mendil")




ts11=ts(mendil$sold_count, frequency = 7)
decomposed11= decompose(ts11,type="multiplicative")
deseasonalised11=ts11/decomposed11$seasonal
detrend11=deseasonalised11/decomposed11$trend
acf(detrend11,na.action = na.pass, main="Decomposed ACF freq=7")
plot(decomposed11)

ts12=ts(mendil$sold_count, frequency = 15)
decomposed12= decompose(ts12,type="multiplicative")
deseasonalised12=ts12/decomposed12$seasonal
detrend12=deseasonalised12/decomposed12$trend
acf(detrend12,na.action = na.pass, main="Decomposed ACF freq=15")
plot(decomposed12)

ts13=ts(mendil$sold_count, frequency = 30)
decomposed13= decompose(ts13,type="multiplicative")
deseasonalised13=ts13/decomposed13$seasonal
detrend13=deseasonalised13/decomposed13$trend
acf(detrend13,na.action = na.pass, main="Decomposed ACF freq=30")
plot(decomposed13)

tsdisplay(decomposed11$random, lag=30, main="Random")


model11=arima(decomposed11$random,order=c(3,0,0))
print(model11)
fitted11=fitted(model11)
fitted_transformed11=fitted11*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed11,type = "l",col="blue")
plot(residuals(model11), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted11,main="AR(3)")

model12=arima(decomposed11$random,order=c(0,0,3))
print(model12)
fitted12=fitted(model12)
fitted_transformed12=fitted12*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed12,type = "l",col="blue")
plot(residuals(model12), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted12, main="MA(3)")


model13=auto.arima(decomposed11$random,seasonal = FALSE)
print(model13)
model13=arima(decomposed11$random,order=c(2,0,1)) # Manuel version of auto.arima (same model)
fitted13=fitted(model13)
fitted_transformed13=fitted13*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed13,type = "l",col="blue")
plot(residuals(model13), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted13,main="ARIMA(2,0,1)")


AIC(model11)
BIC(model11)
AIC(model12)
BIC(model12)
AIC(model13)
BIC(model13)


accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  MAPE=sum(abs(error/actual))/n
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,MAPE,MAD,MADP,WMAPE)
  return(l)
}

accu(ts11,tail(head(fitted_transformed11,369),366))
accu(ts11,tail(head(fitted_transformed12,369),366))
accu(ts11,tail(head(fitted_transformed13,369),366))


correlation=function(dt){  # Function to compute correlation between sold counts and other columns in the data
  dt=as.data.frame(dt)
  a=data.frame(data= NA, nrow=8)
  colnames(a)=c("Variable","Correlation")
  a[1,1]="visit count"
  a[2,1]="basket count"
  a[3,1]="favored count"
  a[4,1]="category sold"
  a[5,1]="category visit"
  a[6,1]="category basket"
  a[7,1]="category favored"
  a[8,1]="category brand sold"
  for(i in 1:8) {
    a[i,2]=cor(dt[,4],dt[,i+4])
  }
  return (a)
}
cor(mendil$sold_count,mendil$price)
correlation(mendil)


modelx1=arima(decomposed11$random,order=c(3,0,2),xreg=mendil$basket_count)
print(modelx1)
fittedx1=fitted(modelx1)
fitted_transformedx1=fittedx1*decomposed11$seasonal*decomposed11$trend

modelx2=arima(decomposed11$random,order=c(3,0,2),xreg=mendil$category_sold)
print(modelx2)
fittedx2=fitted(modelx2)
fitted_transformedx2=fittedx2*decomposed11$seasonal*decomposed11$trend


modelx3=arima(decomposed11$random,order=c(3,0,2),xreg=mendil$category_favored)
print(modelx3)
fittedx3=fitted(modelx3)
fitted_transformedx3=fittedx3*decomposed11$seasonal*decomposed11$trend

accu(ts11,tail(head(fitted_transformed13,369),7)) # arima model
accu(ts11,tail(head(fitted_transformedx1,369),7)) # arimax1
accu(ts11,tail(head(fitted_transformedx2,369),7)) # arimax2
accu(ts11,tail(head(fitted_transformedx3,369),7)) # arimax3

AIC(model13)
AIC(modelx1)
AIC(modelx2)
AIC(modelx3)

#---------------------------------------------------------------------------------------------------------
# Codes of Product 3

ts.plot(xiaomi$sold_count)
acf(xiaomi$sold_count, lag=100 ,main="Xiaomi Bluetooth Kulaklýk")


ts11=ts(xiaomi$sold_count, frequency = 7)
decomposed11= decompose(ts11,type="multiplicative")
deseasonalised11=ts11/decomposed11$seasonal
detrend11=deseasonalised11/decomposed11$trend
acf(detrend11,na.action = na.pass, main="Decomposed ACF freq=7")
plot(decomposed11)


ts13=ts(xiaomi$sold_count, frequency = 30)
decomposed13= decompose(ts13,type="multiplicative")
deseasonalised13=ts13/decomposed13$seasonal
detrend13=deseasonalised13/decomposed13$trend
acf(detrend13,na.action = na.pass, main="Decomposed ACF freq=30")
plot(decomposed13)

tsdisplay(decomposed11$random, lag=30, main="Random")


model11=arima(decomposed11$random,order=c(3,0,0))
print(model11)
fitted11=fitted(model11)
fitted_transformed11=fitted11*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed11,type = "l",col="blue")
plot(residuals(model11), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted11,main="AR(3)")

model12=arima(decomposed11$random,order=c(0,0,3))
print(model12)
fitted12=fitted(model12)
fitted_transformed12=fitted12*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed12,type = "l",col="blue")
plot(residuals(model12), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted12, main="MA(3)")


model13=auto.arima(decomposed11$random,seasonal = FALSE)
print(model13)
model13=arima(decomposed11$random,order=c(5,0,3)) # Manuel version of auto.arima (same model)
fitted13=fitted(model13)
fitted_transformed13=fitted13*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed13,type = "l",col="blue")
plot(residuals(model13), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted13,main="ARIMA(5,0,3)")


AIC(model11)
BIC(model11)
AIC(model12)
BIC(model12)
AIC(model13)
BIC(model13)


accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  MAPE=sum(abs(error/actual))/n
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,MAPE,MAD,MADP,WMAPE)
  return(l)
}

accu(ts11,tail(head(fitted_transformed11,369),366))
accu(ts11,tail(head(fitted_transformed12,369),366))
accu(ts11,tail(head(fitted_transformed13,369),366))


correlation=function(dt){  # Function to compute correlation between sold counts and other columns in the data
  dt=as.data.frame(dt)
  a=data.frame(data= NA, nrow=8)
  colnames(a)=c("Variable","Correlation")
  a[1,1]="visit count"
  a[2,1]="basket count"
  a[3,1]="favored count"
  a[4,1]="category sold"
  a[5,1]="category visit"
  a[6,1]="category basket"
  a[7,1]="category favored"
  a[8,1]="category brand sold"
  for(i in 1:8) {
    a[i,2]=cor(dt[,4],dt[,i+4])
  }
  return (a)
}
cor(xiaomi$sold_count,xiaomi$price)
correlation(xiaomi)


modelx1=arima(decomposed11$random,order=c(3,0,2),xreg=xiaomi$basket_count)
print(modelx1)
fittedx1=fitted(modelx1)
fitted_transformedx1=fittedx1*decomposed11$seasonal*decomposed11$trend

modelx2=arima(decomposed11$random,order=c(3,0,2),xreg=xiaomi$category_sold)
print(modelx2)
fittedx2=fitted(modelx2)
fitted_transformedx2=fittedx2*decomposed11$seasonal*decomposed11$trend


modelx3=arima(decomposed11$random,order=c(3,0,2),xreg=xiaomi$price)
print(modelx3)
fittedx3=fitted(modelx3)
fitted_transformedx3=fittedx3*decomposed11$seasonal*decomposed11$trend

accu(ts11,tail(head(fitted_transformed13,369),7)) # arima model
accu(ts11,tail(head(fitted_transformedx1,369),7)) # arimax1
accu(ts11,tail(head(fitted_transformedx2,369),7)) # arimax2
accu(ts11,tail(head(fitted_transformedx3,369),7)) # arimax3

AIC(model13)
AIC(modelx1)
AIC(modelx2)
AIC(modelx3)

#---------------------------------------------------------------------------------------------------------
# Codes of Product 4

ts.plot(fakir$sold_count)
acf(fakir$sold_count, lag=100 ,main="Fakir Süpürge")


ts11=ts(fakir$sold_count, frequency = 7)
decomposed11= decompose(ts11,type="multiplicative")
deseasonalised11=ts11/decomposed11$seasonal
detrend11=deseasonalised11/decomposed11$trend
acf(detrend11,na.action = na.pass, main="Decomposed ACF freq=7")
plot(decomposed11)

ts12=ts(fakir$sold_count, frequency = 14)
decomposed12= decompose(ts12,type="multiplicative")
deseasonalised12=ts12/decomposed12$seasonal
detrend12=deseasonalised12/decomposed12$trend
acf(detrend12,na.action = na.pass, main="Decomposed ACF freq=14")
plot(decomposed12)

ts13=ts(fakir$sold_count, frequency = 30)
decomposed13= decompose(ts13,type="multiplicative")
deseasonalised13=ts13/decomposed13$seasonal
detrend13=deseasonalised13/decomposed13$trend
acf(detrend13,na.action = na.pass, main="Decomposed ACF freq=30")
plot(decomposed13)

tsdisplay(decomposed11$random, lag=30, main="Random")


model11=arima(decomposed11$random,order=c(3,0,0))
print(model11)
fitted11=fitted(model11)
fitted_transformed11=fitted11*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed11,type = "l",col="blue")
plot(residuals(model11), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted11,main="AR(3)")

model12=arima(decomposed11$random,order=c(0,0,3))
print(model12)
fitted12=fitted(model12)
fitted_transformed12=fitted12*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed12,type = "l",col="blue")
plot(residuals(model12), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted12, main="MA(3)")


model13=auto.arima(decomposed11$random,seasonal = FALSE)
print(model13)
model13=arima(decomposed11$random,order=c(4,0,1)) # Manuel version of auto.arima (same model)
fitted13=fitted(model13)
fitted_transformed13=fitted13*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed13,type = "l",col="blue")
plot(residuals(model13), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted13,main="ARIMA(4,0,1)")


AIC(model11)
BIC(model11)
AIC(model12)
BIC(model12)
AIC(model13)
BIC(model13)


accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  MAPE=sum(abs(error/actual))/n
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,MAPE,MAD,MADP,WMAPE)
  return(l)
}

accu(ts11,tail(head(fitted_transformed11,369),366))
accu(ts11,tail(head(fitted_transformed12,369),366))
accu(ts11,tail(head(fitted_transformed13,369),366))


correlation=function(dt){  # Function to compute correlation between sold counts and other columns in the data
  dt=as.data.frame(dt)
  a=data.frame(data= NA, nrow=8)
  colnames(a)=c("Variable","Correlation")
  a[1,1]="visit count"
  a[2,1]="basket count"
  a[3,1]="favored count"
  a[4,1]="category sold"
  a[5,1]="category visit"
  a[6,1]="category basket"
  a[7,1]="category favored"
  a[8,1]="category brand sold"
  for(i in 1:8) {
    a[i,2]=cor(dt[,4],dt[,i+4])
  }
  return (a)
}
cor(fakir$sold_count,fakir$price)
correlation(fakir)

modelx1=arima(decomposed11$random,order=c(4,0,1),xreg=fakir$basket_count)
print(modelx1)
fittedx1=fitted(modelx1)
fitted_transformedx1=fittedx1*decomposed11$seasonal*decomposed11$trend

modelx2=arima(decomposed11$random,order=c(4,0,1),xreg=fakir$category_sold)
print(modelx2)
fittedx2=fitted(modelx2)
fitted_transformedx2=fittedx2*decomposed11$seasonal*decomposed11$trend


modelx3=arima(decomposed11$random,order=c(4,0,1),xreg=fakir$category_favored)
print(modelx3)
fittedx3=fitted(modelx3)
fitted_transformedx3=fittedx3*decomposed11$seasonal*decomposed11$trend

accu(ts11,tail(head(fitted_transformed13,369),7)) # arima model, chosen model will be this one
accu(ts11,tail(head(fitted_transformedx1,369),7)) # arimax1
accu(ts11,tail(head(fitted_transformedx2,369),7)) # arimax2 
accu(ts11,tail(head(fitted_transformedx3,369),7)) # arimax3

AIC(model13)
AIC(modelx1)
AIC(modelx2)
AIC(modelx3)



#---------------------------------------------------------------------------------------------------------
# Codes of Product 5


ts.plot(tayt$sold_count)
acf(tayt$sold_count, lag=100 ,main="tayt")


ts11=ts(tayt$sold_count, frequency = 7)
decomposed11= decompose(ts11,type="multiplicative")
deseasonalised11=ts11/decomposed11$seasonal
detrend11=deseasonalised11/decomposed11$trend
acf(detrend11,na.action = na.pass, main="Decomposed ACF freq=7")
plot(decomposed11)

ts12=ts(tayt$sold_count, frequency = 15)
decomposed12= decompose(ts12,type="multiplicative")
deseasonalised12=ts12/decomposed12$seasonal
detrend12=deseasonalised12/decomposed12$trend
acf(detrend12,na.action = na.pass, main="Decomposed ACF freq=15")
plot(decomposed12)

ts13=ts(tayt$sold_count, frequency = 14)
decomposed13= decompose(ts13,type="multiplicative")
deseasonalised13=ts13/decomposed13$seasonal
detrend13=deseasonalised13/decomposed13$trend
acf(detrend13,na.action = na.pass, main="Decomposed ACF freq=14")
plot(decomposed13)

tsdisplay(decomposed11$random, lag=30, main="Random")


model11=arima(decomposed11$random,order=c(2,0,0))
print(model11)
fitted11=fitted(model11)
fitted_transformed11=fitted11*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed11,type = "l",col="blue")
plot(residuals(model11), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted11,main="AR(2)")

model12=arima(decomposed11$random,order=c(0,0,5))
print(model12)
fitted12=fitted(model12)
fitted_transformed12=fitted12*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed12,type = "l",col="blue")
plot(residuals(model12), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted12, main="MA(5)")


model13=auto.arima(decomposed11$random,seasonal = FALSE)
print(model13)
model13=arima(decomposed11$random,order=c(2,0,1)) # Manuel version of auto.arima (same model)
fitted13=fitted(model13)
fitted_transformed13=fitted13*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed13,type = "l",col="blue")
plot(residuals(model13), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted13,main="ARIMA(2,0,1)")


AIC(model11)
BIC(model11)
AIC(model12)
BIC(model12)
AIC(model13)
BIC(model13)


accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  MAPE=sum(abs(error/actual))/n
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,MAPE,MAD,MADP,WMAPE)
  return(l)
}

accu(ts11,tail(head(fitted_transformed11,369),366))
accu(ts11,tail(head(fitted_transformed12,369),366))
accu(ts11,tail(head(fitted_transformed13,369),366))


correlation=function(dt){  # Function to compute correlation between sold counts and other columns in the data
  dt=as.data.frame(dt)
  a=data.frame(data= NA, nrow=8)
  colnames(a)=c("Variable","Correlation")
  a[1,1]="visit count"
  a[2,1]="basket count"
  a[3,1]="favored count"
  a[4,1]="category sold"
  a[5,1]="category visit"
  a[6,1]="category basket"
  a[7,1]="category favored"
  a[8,1]="category brand sold"
  for(i in 1:8) {
    a[i,2]=cor(dt[,4],dt[,i+4])
  }
  return (a)
}
cor(tayt$sold_count,tayt$price)
correlation(tayt)


modelx1=arima(decomposed11$random,order=c(2,0,1),xreg=tayt$basket_count)
print(modelx1)
fittedx1=fitted(modelx1)
fitted_transformedx1=fittedx1*decomposed11$seasonal*decomposed11$trend

modelx2=arima(decomposed11$random,order=c(2,0,1),xreg=tayt$category_sold)
print(modelx2)
fittedx2=fitted(modelx2)
fitted_transformedx2=fittedx2*decomposed11$seasonal*decomposed11$trend


modelx3=arima(decomposed11$random,order=c(2,0,1),xreg=tayt$category_favored)
print(modelx3)
fittedx3=fitted(modelx3)
fitted_transformedx3=fittedx3*decomposed11$seasonal*decomposed11$trend

accu(ts11,tail(head(fitted_transformed13,369),7)) # arima model
accu(ts11,tail(head(fitted_transformedx1,369),7)) # arimax1
accu(ts11,tail(head(fitted_transformedx2,369),7)) # arimax2  chosen as final model
accu(ts11,tail(head(fitted_transformedx3,369),7)) # arimax3

AIC(model13)
AIC(modelx1)
AIC(modelx2)
AIC(modelx3)


#---------------------------------------------------------------------------------------------------------
# Codes of Product 6


ts.plot(firca$sold_count)
acf(firca$sold_count, lag=100 ,main="Diþ Fýrçasý")


ts11=ts(firca$sold_count, frequency = 7)
decomposed11= decompose(ts11,type="multiplicative")
deseasonalised11=ts11/decomposed11$seasonal
detrend11=deseasonalised11/decomposed11$trend
acf(detrend11,na.action = na.pass, main="Decomposed ACF freq=7")
plot(decomposed11)

ts12=ts(firca$sold_count, frequency = 14)
decomposed12= decompose(ts12,type="multiplicative")
deseasonalised12=ts12/decomposed12$seasonal
detrend12=deseasonalised12/decomposed12$trend
acf(detrend12,na.action = na.pass, main="Decomposed ACF freq=14")
plot(decomposed12)

ts13=ts(firca$sold_count, frequency = 30)
decomposed13= decompose(ts13,type="multiplicative")
deseasonalised13=ts13/decomposed13$seasonal
detrend13=deseasonalised13/decomposed13$trend
acf(detrend13,na.action = na.pass, main="Decomposed ACF freq=30")
plot(decomposed13)

tsdisplay(decomposed11$random, lag=30, main="Random")


model11=arima(decomposed11$random,order=c(3,0,0))
print(model11)
fitted11=fitted(model11)
fitted_transformed11=fitted11*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed11,type = "l",col="blue")
plot(residuals(model11), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted11,main="AR(3)")

model12=arima(decomposed11$random,order=c(0,0,5))
print(model12)
fitted12=fitted(model12)
fitted_transformed12=fitted12*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed12,type = "l",col="blue")
plot(residuals(model12), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted12, main="MA(5)")


model13=auto.arima(decomposed11$random,seasonal = FALSE)
print(model13)
model13=arima(decomposed11$random,order=c(2,0,1)) # Manuel version of auto.arima (same model)
fitted13=fitted(model13)
fitted_transformed13=fitted13*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed13,type = "l",col="blue")
plot(residuals(model13), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted13,main="ARIMA(2,0,1)")


AIC(model11)
BIC(model11)
AIC(model12)
BIC(model12)
AIC(model13)
BIC(model13)


accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  MAPE=sum(abs(error/actual))/n
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,MAPE,MAD,MADP,WMAPE)
  return(l)
}

accu(ts11,tail(head(fitted_transformed11,369),366))
accu(ts11,tail(head(fitted_transformed12,369),366))
accu(ts11,tail(head(fitted_transformed13,369),366))


correlation=function(dt){  # Function to compute correlation between sold counts and other columns in the data
  dt=as.data.frame(dt)
  a=data.frame(data= NA, nrow=8)
  colnames(a)=c("Variable","Correlation")
  a[1,1]="visit count"
  a[2,1]="basket count"
  a[3,1]="favored count"
  a[4,1]="category sold"
  a[5,1]="category visit"
  a[6,1]="category basket"
  a[7,1]="category favored"
  a[8,1]="category brand sold"
  for(i in 1:8) {
    a[i,2]=cor(dt[,4],dt[,i+4])
  }
  return (a)
}
cor(firca$sold_count,firca$price)
correlation(firca)


modelx1=arima(decomposed11$random,order=c(2,0,1),xreg=firca$visit_count)
print(modelx1)
fittedx1=fitted(modelx1)
fitted_transformedx1=fittedx1*decomposed11$seasonal*decomposed11$trend

modelx2=arima(decomposed11$random,order=c(2,0,1),xreg=firca$basket_count)
print(modelx2)
fittedx2=fitted(modelx2)
fitted_transformedx2=fittedx2*decomposed11$seasonal*decomposed11$trend


modelx3=arima(decomposed11$random,order=c(2,0,1),xreg=firca$favored_count)
print(modelx3)
fittedx3=fitted(modelx3)
fitted_transformedx3=fittedx3*decomposed11$seasonal*decomposed11$trend

accu(ts11,tail(head(fitted_transformed13,369),7)) # arima model
accu(ts11,tail(head(fitted_transformedx1,369),7)) # arimax1
accu(ts11,tail(head(fitted_transformedx2,369),7)) # arimax2  best model
accu(ts11,tail(head(fitted_transformedx3,369),7)) # arimax3

AIC(model13)
AIC(modelx1)
AIC(modelx2)
AIC(modelx3)



#---------------------------------------------------------------------------------------------------------
# Codes of Product 7

ts.plot(mont$sold_count)
ts.plot(mont$basket_count)
acf(mont$sold_count, lag=100 ,main="Mont")
acf(mont$sold_count, lag=30 ,main="Mont")



#---------------------------------------------------------------------------------------------------------
# Codes of Product 8

bik_1<-dat[2132:2232,] # data table without null and zero values of sold count for bikini model-1

ts.plot(bik_1$sold_count)
acf(bik_1$sold_count, lag=100 ,main="Bikini 1")


ts11=ts(bik_1$sold_count, frequency = 7)
decomposed11= decompose(ts11,type="multiplicative")
deseasonalised11=ts11/decomposed11$seasonal
detrend11=deseasonalised11/decomposed11$trend
acf(detrend11,na.action = na.pass, main="Decomposed ACF freq=7")
plot(decomposed11)

ts12=ts(bik_1$sold_count, frequency = 15)
decomposed12= decompose(ts12,type="multiplicative")
deseasonalised12=ts12/decomposed12$seasonal
detrend12=deseasonalised12/decomposed12$trend
acf(detrend12,na.action = na.pass, main="Decomposed ACF freq=15")
plot(decomposed12)

ts13=ts(bik_1$sold_count, frequency = 30)
decomposed13= decompose(ts13,type="multiplicative")
deseasonalised13=ts13/decomposed13$seasonal
detrend13=deseasonalised13/decomposed13$trend
acf(detrend13,na.action = na.pass, main="Decomposed ACF freq=30")
plot(decomposed13)

tsdisplay(decomposed11$random, lag=30, main="Random")


model11=arima(decomposed11$random,order=c(3,0,0))
print(model11)
fitted11=fitted(model11)
fitted_transformed11=fitted11*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed11,type = "l",col="blue")
plot(residuals(model11), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted11,main="AR(3)")

model12=arima(decomposed11$random,order=c(0,0,2))
print(model12)
fitted12=fitted(model12)
fitted_transformed12=fitted12*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed12,type = "l",col="blue")
plot(residuals(model12), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted12, main="MA(2)")


model13=auto.arima(decomposed11$random,seasonal = FALSE)
print(model13)
model13=arima(decomposed11$random,order=c(2,0,1)) # Manuel version of auto.arima (same model)
fitted13=fitted(model13)
fitted_transformed13=fitted13*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed13,type = "l",col="blue")
plot(residuals(model13), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted13,main="ARIMA(2,0,1)")


AIC(model11)
BIC(model11)
AIC(model12)
BIC(model12)
AIC(model13)
BIC(model13)


accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  MAPE=sum(abs(error/actual))/n
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,MAPE,MAD,MADP,WMAPE)
  return(l)
}

accu(ts11,tail(head(fitted_transformed11,98),95))
accu(ts11,tail(head(fitted_transformed12,98),95))
accu(ts11,tail(head(fitted_transformed13,98),95))


correlation=function(dt){  # Function to compute correlation between sold counts and other columns in the data
  dt=as.data.frame(dt)
  a=data.frame(data= NA, nrow=8)
  colnames(a)=c("Variable","Correlation")
  a[1,1]="visit count"
  a[2,1]="basket count"
  a[3,1]="favored count"
  a[4,1]="category sold"
  a[5,1]="category visit"
  a[6,1]="category basket"
  a[7,1]="category favored"
  a[8,1]="category brand sold"
  for(i in 1:8) {
    a[i,2]=cor(dt[,4],dt[,i+4])
  }
  return (a)
}

correlation(bik_1)
cor(bik_1$sold_count,bik_1$price)



modelx1=arima(decomposed11$random,order=c(2,0,1),xreg=bik_1$basket_count)
print(modelx1)
fittedx1=fitted(modelx1)
fitted_transformedx1=fittedx1*decomposed11$seasonal*decomposed11$trend

modelx2=arima(decomposed11$random,order=c(2,0,1),xreg=bik_1$visit_count)
print(modelx2)
fittedx2=fitted(modelx2)
fitted_transformedx2=fittedx2*decomposed11$seasonal*decomposed11$trend


modelx3=arima(decomposed11$random,order=c(2,0,1),xreg=bik_1$category_sold)
print(modelx3)
fittedx3=fitted(modelx3)
fitted_transformedx3=fittedx3*decomposed11$seasonal*decomposed11$trend

accu(ts11,tail(head(fitted_transformed13,98),7)) # arima model
accu(ts11,tail(head(fitted_transformedx1,98),7)) # arimax1
accu(ts11,tail(head(fitted_transformedx2,98),7)) # arimax2
accu(ts11,tail(head(fitted_transformedx3,98),7)) # arimax3


AIC(model13)
AIC(modelx1)
AIC(modelx2)
AIC(modelx3)



#---------------------------------------------------------------------------------------------------------
# Codes of Product 9

bik_2<-dat[3315:3348,]  # data table without null and zero values of sold count for bikini model-1

ts.plot(bik_2$sold_count)
acf(bik_2$sold_count, lag=100 ,main="Bikini 2")


ts11=ts(bik_2$sold_count, frequency = 7)
decomposed11= decompose(ts11,type="multiplicative")
deseasonalised11=ts11/decomposed11$seasonal
detrend11=deseasonalised11/decomposed11$trend
acf(detrend11,na.action = na.pass, main="Decomposed ACF freq=7")
plot(decomposed11)

ts12=ts(bik_2$sold_count, frequency = 15)
decomposed12= decompose(ts12,type="multiplicative")
deseasonalised12=ts12/decomposed12$seasonal
detrend12=deseasonalised12/decomposed12$trend
acf(detrend12,na.action = na.pass, main="Decomposed ACF freq=15")
plot(decomposed12)

ts13=ts(bik_2$sold_count, frequency = 30)
decomposed13= decompose(ts13,type="multiplicative")
deseasonalised13=ts13/decomposed13$seasonal
detrend13=deseasonalised13/decomposed13$trend
acf(detrend13,na.action = na.pass, main="Decomposed ACF freq=30")
plot(decomposed13)

tsdisplay(decomposed11$random, lag=30, main="Random")


model11=arima(decomposed11$random,order=c(2,0,0))
print(model11)
fitted11=fitted(model11)
fitted_transformed11=fitted11*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed11,type = "l",col="blue")
plot(residuals(model11), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted11,main="AR(2)")

model12=arima(decomposed11$random,order=c(0,0,1))
print(model12)
fitted12=fitted(model12)
fitted_transformed12=fitted12*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed12,type = "l",col="blue")
plot(residuals(model12), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted12, main="MA(1)")


model13=arima(decomposed11$random,order =c(2,0,1))
print(model13)
fitted13=fitted(model13)
fitted_transformed13=fitted13*decomposed11$seasonal*decomposed11$trend
plot(ts11,main="fitted vs real",ylab="Sold Count", xlab="Days")
points(fitted_transformed13,type = "l",col="blue")
plot(residuals(model13), xlab="Days", ylab="Values", main="Residuals")
tsdisplay(fitted13,main="ARIMA(2,0,1)")


AIC(model11)
BIC(model11)
AIC(model12)
BIC(model12)
AIC(model13)
BIC(model13)


accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  mean=mean(actual)
  sd=sd(actual)
  CV=sd/mean
  MAPE=sum(abs(error/actual))/n
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,sd,CV,MAPE,MAD,MADP,WMAPE)
  return(l)
}

accu(ts11,tail(head(fitted_transformed11,31),28))
accu(ts11,tail(head(fitted_transformed12,31),28))
accu(ts11,tail(head(fitted_transformed13,31),28))


correlation=function(dt){  # Function to compute correlation between sold counts and other columns in the data
  dt=as.data.frame(dt)
  a=data.frame(data= NA, nrow=8)
  colnames(a)=c("Variable","Correlation")
  a[1,1]="visit count"
  a[2,1]="basket count"
  a[3,1]="favored count"
  a[4,1]="category sold"
  a[5,1]="category visit"
  a[6,1]="category basket"
  a[7,1]="category favored"
  a[8,1]="category brand sold"
  for(i in 1:8) {
    a[i,2]=cor(dt[,4],dt[,i+4])
  }
  return (a)
}

correlation(bik_2)
cor(bik_2$sold_count,bik_2$price)



modelx1=arima(decomposed11$random,order=c(2,0,1),xreg=bik_2$basket_count)
print(modelx1)
fittedx1=fitted(modelx1)
fitted_transformedx1=fittedx1*decomposed11$seasonal*decomposed11$trend

modelx2=arima(decomposed11$random,order=c(2,0,1),xreg=bik_2$visit_count)
print(modelx2)
fittedx2=fitted(modelx2)
fitted_transformedx2=fittedx2*decomposed11$seasonal*decomposed11$trend


modelx3=arima(decomposed11$random,order=c(2,0,1),xreg=bik_2$favored_count)
print(modelx3)
fittedx3=fitted(modelx3)
fitted_transformedx3=fittedx3*decomposed11$seasonal*decomposed11$trend

accu(ts11,tail(head(fitted_transformed13,31),7)) # arima model
accu(ts11,tail(head(fitted_transformedx1,31),7)) # arimax1
accu(ts11,tail(head(fitted_transformedx2,31),7)) # arimax2
accu(ts11,tail(head(fitted_transformedx3,31),7)) # arimax3

AIC(model13)
AIC(modelx1)
AIC(modelx2)
AIC(modelx3)



#---------------------------------------------------------------------
#END




