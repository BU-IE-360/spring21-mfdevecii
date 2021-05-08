library(readxl)
library(fpp)
library(ggplot2)
library(data.table)
frame=read_excel("C:/Users/mfdevecii/Desktop/Sales.xlsx")
mytable=as.data.table(frame)
sales=ts(mytable$`House_Sales`, frequency = 12, start=c(2016,1) )
interest=ts(mytable$`Interest`, frequency = 12, start=c(2016,1) )
econ=ts(mytable$`Economy`, frequency = 12, start=c(2016,1) )
plot(sales, xlab="Time", ylab="Sales", main="Monthly House Sales")
plot(log(sales))   #variance change is even more 
mytable[,trend:=1:.N]
month=1:12
mytable=cbind(mytable,month)
mytable[trend %between% c(52,53),is_covid:=1]
mytable[is.na(is_covid)==TRUE, is_covid:=0] 
count1=0
count2=0
for(i in 1:63) {
  if(mytable[i,7]==0 & mytable[i,6]==4){
    count1=count1+mytable[i,2]
  }
}
for(i in 1:63) {
  if(mytable[i,7]==0 & mytable[i,6]==5){
    count2=count2+mytable[i,2]
  }
}
count1=count1/4
count2=count2/4
mytable[52,2]=count1
mytable[53,2]=count2
sales=ts(mytable$`House_Sales`, frequency = 12, start=c(2016,1) ) # Retrieve the time series version
plot(sales, xlab="Time", ylab="Sales", main="Monthly House Sales")

mytable[61,2]=1.34*mytable[61,2]  #Assignment
mytable[62,2]=1.34*mytable[62,2]  #Assignment
mytable[63,2]=1.34*mytable[63,2]  #Assignment
sales=ts(mytable$`House_Sales`, frequency = 12, start=c(2016,1) ) 
plot(sales, xlab="Time", ylab="Sales", main="Monthly House Sales")
plot(interest, xlab="Time", ylab="Interest Rates", main="Monthly Interest Rates")
plot(econ, xlab="Time", ylab="Wealth", main="Result of Surveys")

fit1 = lm(House_Sales~trend, data = mytable)
summary(fit1)
checkresiduals(fit1,lag=12)

fit2 = lm(House_Sales~trend+as.factor(month), data = mytable)
summary(fit2)
checkresiduals(fit2,lag=12)



fit3 = lm(House_Sales~trend+as.factor(month)+Interest+Economy, data = mytable)
summary(fit3)
checkresiduals(fit3,lag=12)

acf(residuals(fit3))
mytable[, residuals:=residuals(fit3)]
mytable[, lag1:=shift(residuals(fit3),1)]


ffit= lm(House_Sales~trend+as.factor(month)+Interest+Economy+lag1, data = mytable)
summary(ffit)
checkresiduals(ffit,lag=12)

mean_of_fit1=mean(residuals(fit1))
mean_of_fit2=mean(residuals(fit2))
mean_of_fit3=mean(residuals(fit3))
mean_of_fit4=mean(residuals(ffit))

acf(residuals(ffit))

fitted=fitted(ffit)
residual=residuals(ffit)
frame2=data.frame(fitted,residual,trend=2:63)
mytable2=as.data.table(frame2)
mytable2[,House_Sales:=tail(mytable$House_Sales,62)]

cols1 = c("predicted" = "yellow", "actual" = "blue")
cols2 = c("House_Sales" = "yellow", "fitted" = "blue")

mytable2 %>%
  ggplot(aes(x=fitted, y=residual)) + 
  geom_point() 

mytable2 %>%
  ggplot(aes(x=fitted, y=House_Sales)) + 
  geom_point() +
  geom_abline(slope=1, intercept=0) 

mytable2 %>%
  ggplot() + 
  geom_line(aes(x=trend, y=House_Sales,color="House_Sales")) + geom_line(aes(x=trend, y=fitted,color="fitted")) + 
  geom_abline(slope=1, intercept=0) +
  scale_color_manual(values=cols2)

temp=data.table(Date=as.character("2021-04"),House_Sales=0,Interest=0,Economy=0,trend=64,month=4,is_covid=0,residuals=0,lag1=4620.495803)
mytable=rbind(mytable,temp)
mytable[64,3]=predict(lm(Interest~trend, data = mytable))[64]
mytable[64,4]=predict(lm(Economy~trend, data = mytable)) [64]
forcest_result=predict(lm(House_Sales~trend+as.factor(month)+Interest+Economy+lag1, data = mytable))[63]

ffitted=fitted(lm(House_Sales~trend+as.factor(month)+Interest+Economy+lag1, data = mytable))
actual=tail(sales,62)
a=data.table(fitted=forcest_result , residual = 0 , trend = 64 , House_Sales=0)
mytable2=rbind(mytable2,a)
  
cols <- c("predicted" = "orange", "actual" = "blue")

ggplot() + 
  geom_line(data = mytable2, aes(x = trend, y = fitted,color = "predicted")) +
  geom_line(data = mytable2, aes(x = trend, y = House_Sales,color = "actual")) +
  xlab('time') +
  ylab('sales') +
  scale_color_manual(values = cols)

