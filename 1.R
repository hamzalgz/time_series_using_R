library(aTSA)
library(tseries)
library(readr)
library(urca)

setwd("C:\\Users\\hamza lagramez\\Desktop\\SCM_TP")
data<-read_csv(file='Data_TP1.csv')

psei<-ts(data$psei, start=c(1999,1), frequency = 4)
ts.plot(psei)

rgdp<-ts(data$real_gdp_growth, start=c(1999,1), frequency = 4)
ts.plot(rgdp)



bsp<-ts(data$bsp_rrp, start=c(1999,1), frequency = 4)
ts.plot(bsp)

unem<-ts(data$unem, start=c(1999,1), frequency = 4)
ts.plot(unem)

stationary.test(psei,method='adf',nlag = 1)
stationary.test(psei,method='pp',nlag = 1)
stationary.test(psei,method='kpss',nlag = 1)

summary(ur.df(psei, type='none', 1))