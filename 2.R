library(tseries)
library(readr)
library(aTSA)
library(vars)
library(mFilter)
library(TSstudio)
library(tidyverse)
library(tsDyn)

setwd("C:\\Users\\hamza lagramez\\Desktop\\SCM_TP")
data<-read_csv(file='Data_TP1.csv')


rgdp<-ts(data$real_gdp_growth, start=c(1999,1), frequency = 4)
ts.plot(rgdp)

RRP<-ts(data$bsp_rrp, start=c(1999,1), frequency = 4)
ts.plot(RRP)

unem<-ts(data$unem, start=c(1999,1), frequency = 4)
ts.plot(unem)

stationary.test(rgdp,method='adf')
stationary.test(rgdp,method='pp')
stationary.test(rgdp,method='kpss')

stationary.test(RRP,method='adf')
stationary.test(RRP,method='pp')
stationary.test(RRP,method='kpss')

stationary.test(unem,method='adf')
stationary.test(unem,method='pp')
stationary.test(unem,method='kpss')

var1<-cbind(RRP,rgdp,unem)
colnames(var1)<-cbind("RRP","rgdp","unem")


lagselect<-VARselect(var1, lag.max = 4, type="const")
lagselect
lagselect$selection

model1<-VAR(var1,p=1,type="const", season=NULL, exogen = NULL)
summary(model1)


root.comp<-Im(roots(model1, modulus=FALSE))
root.real<-Re(roots(model1, modulus=FALSE))


x <- seq(-1, 1, length=1000)
y1 <- sqrt(1-x^2)
y2 <- -sqrt(1-x^2)
plot(c(x, x), c(y1, y2), xlab='Partie réelle', ylab='Partie imaginaire',
     type='l', main='cercle unitaire', ylim=c(-2, 2), xlim=c (-2, 2))
abline(h=0)
abline(v=0)

points(root.comp, root.real, pch=20)
legend(-1.5, -1, legend="Eigenvalues", pch=20)

#ho:noautocorrele
s<- serial.test(model1, lags.pt = 5, type = "PT.asymptotic" )
s
#H0:hetorosc
hter <- arch.test(model1, multivariate.only = TRUE)
hter

#H0: normalité
Norm1 <- normality.test(model1, multivariate.only = TRUE)
Norm1

forecast<-predict(model1, n.ahead=2, ci=0.95)
forecast

fanchart(forecast, names='RRP', main='Fanchart for RRP', xlab='horizon',ylab = 'RRP')
fanchart(forecast, names='rgdp', main='Fanchart for rgdp', xlab='horizon',ylab = 'rgdp')
fanchart(forecast, names='unem', main='Fanchart for unem', xlab='horizon',ylab = 'unem')



#RRP 

dRRP<-diff(RRP)/RRP

ts.plot(dRRP, differences=0.5)

var2<-cbind(dRRP,rgdp,unem)
colnames(var2)<-cbind("dRRP","rgdp","unem")

lagselect<-VARselect(var2, lag.max = 4, type="const")
lagselect
lagselect$selection

model2<-VAR(var2,p=3,type="const", season=NULL, exogen = NULL)
summary(model2)