library(ggplot2)
library(readxl)
library(readr)
getwd()
setwd("C:/Users/mfdevecii/Desktop")
mydata = read_excel("HW1.xlsx", col_types = c("skip", "numeric", "numeric", "numeric"))
# dfr is google trends data
dfr = read_csv("123.csv")
df=setNames(data.frame( 1:36, mydata$`Dolar-TL`,mydata$`Cumhuriyet Altýný-TL`,mydata$`Tüketici Fiyat Endeksi`),c("Month ","Dollar-TL","Gold-TL","TUFE"))
hist(mydata$`Dolar-TL`, main="Dollar-TL Rate Frequancy", xlab="Dollar-TL Rate", ylab="Frequancy")
plot(mydata$`Dolar-TL`, main="Dollar-TL Rate", xlab="month number from 01.2018 to 12.2020", ylab="Rate")
ggplot(df,aes(`Month `,`Dollar-TL`)) + geom_col()
ggplot(df,aes(`Month `,`Dollar-TL`)) + geom_line()
hist(mydata$`Cumhuriyet Altýný-TL`, main="Gold-TL Rate Frequancy", xlab="Gold-TL Rate", ylab="Frequancy")
plot(mydata$`Cumhuriyet Altýný-TL`, main="Gold-TL Rate", xlab="month number from 01.2018 to 12.2020", ylab="Rate")
ggplot(df,aes(`Month `,`Gold-TL`)) + geom_col()
ggplot(df,aes(`Month `,`Gold-TL`)) + geom_line()
hist(mydata$`Tüketici Fiyat Endeksi`, main="TUFE Value Frequancy", xlab="TUFE value", ylab="Frequancy")
plot(mydata$`Tüketici Fiyat Endeksi`, main="TUFE Value", xlab="month number from 01.2018 to 12.2020", ylab="Value")
ggplot(df,aes(`Month `,`TUFE`)) + geom_col()
ggplot(df,aes(`Month `,`TUFE`)) + geom_line()
mtr=matrix(dfr$Popularity, nrow=39, ncol=4)
mtr2=matrix(1:39, nrow=39, ncol=1)
for(i in 1:39) {
  count=0
  for(j in 1:4){
    count=count+mtr[i,j]
  }
  mtr2[i,1]=count/4
}
trendframe=data.frame(Popularity = head(mtr2, 36L), months  = 1:36)
hist(mtr2,main="Economic Crisis", xlab="Search Popularity", ylab="Frequancy")
plot(mtr2, main="Keyword Search Popularity", xlab="Month number from 01.2018 to 12.2020", ylab="Popularity")
ggplot(trendframe, aes(months, Popularity))+ geom_col()
ggplot(trendframe, aes(months, Popularity))+ geom_line()



