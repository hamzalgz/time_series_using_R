library(aTSA)
library(tseries)
library(readr)
library(urca)
library(tsDyn)
library(vars)
library(mFilter)
library(TSstudio)
library(tidyverse)

setwd("C:\\Users\\hamza lagramez\\Desktop\\SCM_TP")
data<-read_csv(file='_data.csv')


i_10<-ts(data$TEN, start=c(1999,1))
ts.plot(i_10)

i_5<-ts(data$FIVE, start=c(1999,1))
ts.plot(i_5)

i_1<-ts(data$ONE, start=c(1999,1))
ts.plot(i_1)


i_all<-cbind(i_10,i_5,i_1)
colnames(i_all)<-cbind("i_10","i_5","i_1")

lagselect<-VARselect(i_all, lag.max = 48, type="none")
lagselect
lagselect$selection

vec_trace<-ca.jo(i_all, ecdet = 'const', type='trace', K=16 ,  spec='transitory')
summary(vec_trace)

vec_einenmax<-ca.jo(i_all, ecdet = 'const', type='eigen', K=16,  spec='transitory')
summary(vec_einenmax)

i_all_vecm<-VECM(i_all, lag=15, r=1, include='const', estim = 'ML')
summary(i_all_vecm)

var<-vec2var(vec_trace, r=1)
ir<-irf(var, n.ahead = 20, impulse = 'i_1', ortho=FALSE, runs= 24)
plot(ir)


forecost<-predict(var, n.ahead=24, ci=0.95)
forecost