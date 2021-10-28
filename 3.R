library(tseries)
library(readr)
library(aTSA)
library(vars)
library(tseries)



setwd("C:\\Users\\hamza lagramez\\Desktop\\SCM_TP")
data<-read_csv(file='Data_TP1.csv')


RRP<-ts(data$bsp_rrp, start=c(1999,1), frequency = 4)
ts.plot(RRP)

psei<-ts(data$psei, start=c(1999,1), frequency = 4)
ts.plot(psei)
#test de sattionarity

stationary.test(RRP, method='adf')
stationary.test(psei,method='adf')

dRRP<-diff(RRP)
dpsei<-diff(psei)

#RRP et psei ne sont pas stationnaire
stationary.test(dRRP,method='adf')
stationary.test(dpsei,method='adf')

var1<-cbind(dRRP,dpsei)
colnames(var1)<-cbind("dRRP","dpsei")

lagselect<-VARselect(var1, lag.max = 16, type="const")
lagselect
lagselect$selection

model<-VAR(var1, p=2, type='const', season=NULL, exog=NULL)
summary(model)


GrangerRRP<-causality(model, cause= 'dRRP')
GrangerRRP$Granger

linearMod<-lm(RRP~psei)
summary(linearMod)

rsiduals<-residuals(linearMod)


#engle granger cointegration test
coint.test(RRP, psei)