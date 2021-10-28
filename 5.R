library(aTSA)
library(tseries)
library(readr)
library(urca)
library(tsDyn)
library(vars)
library(mFilter)
library(TSstudio)
library(tidyverse)

data <- read.csv("C:/Users/hamza lagramez/Desktop/SCM_TP/dataa.csv", sep=";")

lrgdp<-ts(data$LRGDP, start=c(1959))
plot(lrgdp)

fed<-ts(data$FEDRATE, start=c(1959))
plot(fed)

lrm<-ts(data$LRM, start=c(1959))
plot(lrm)

stationary.test(lrgdp,method='adf')
stationary.test(fed,method='adf')
stationary.test(lrm,method='adf')

dlrgdp<-diff(lrgdp)
dfed<-diff(fed)
dlrm<-diff(lrm)

stationary.test(dlrgdp,method='adf')
stationary.test(dfed,method='adf')
stationary.test(dlrm,method='adf')

var1<-cbind(dlrgdp,dfed, dlrm)
colnames(var1)<-cbind('dlrgdp','dfed', 'dlrm')

lagselect<-VARselect(var1, lag.max = 16, type="const")
lagselect
lagselect$selection

model<-VAR(var1, p=1, type='const')
summary(model)


root.comp<-Im(roots(model1, modulus=FALSE))
root.real<-Re(roots(model1, modulus=FALSE))

x <- seq(-1, 1, length=1000)
y1 <- sqrt(1-x^2)
y2 <- -sqrt(1-x^2)
plot(c(x, x), c(y1, y2), xlab='Partie réelle', ylab='Partie imaginaire',
     type='l', main='cercle unitaire', ylim=c(-2, 2), xlim=c(-2, 2))
abline(h=0)
abline(v=0)

points(root.comp, root.real, pch=20)
legend(-1.5, -1, legend="Eigenvalues", pch=20)

#ho:noautocorrele
s<- serial.test(model1, lags.pt = 8, type = "PT.asymptotic" )
s
#H0:hetorosc
hter <- arch.test(model1, multivariate.only = TRUE)
hter

#H0: normalité
Norm1 <- normality.test(model1, multivariate.only = TRUE)
Norm1

forecast<-predict(model1, n.ahead=8, ci=0.95)
forecast


vec_trace<-ca.jo(var1, ecdet = 'const', type='trace', K=2 ,  spec='transitory')
summary(vec_trace)