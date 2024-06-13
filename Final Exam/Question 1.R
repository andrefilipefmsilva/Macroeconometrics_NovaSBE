setwd("C:\\Users\\Casa\\OneDrive\\Documentos\\Economia Nova SBE\\1st year\\2º Semestre\\Macroeconometrics\\Final 2020")

library(xlsx)
library(tseries)
library(quantmod)
library(urca)
library(tsDyn)
# EXERCISE A)

data=read.xlsx("data_final_2020_Q1.xls", header=TRUE, sheetIndex=1)
data1=data.frame("USA"=data[,2], "GER"=data[,3], "UK"=data[,4], "FR"=data[,5])
zt=log(data1)
zt=ts(zt, start=1960, deltat=1)

#USA GDP series
adf.test(zt[,1],k=2) # Not I(0)
adf.test(diff(zt[,1]),k=2)
# I(1)


#GER GDP series
adf.test(zt[,2], k=2) #Not I(0)
adf.test(diff(zt[,2]),k=2)
# I(1)

#UK GDP series
adf.test(zt[,3], k=2) #Not I(0)
adf.test(diff(zt[,3]),k=2) 
# I(1) at 10%

#FR GDP series
adf.test(zt[,4],k=2) #Not I(0)
adf.test(diff(zt[,4]),k=2)        
#I(1) at 10%

# At a 10% level, all the series are I(1). This is the first step in the search for a cointegrating vector, as it is a requirement
# for cointegration that the variables are integrated of the same order.

# The second step in the Johansen methodology would be to select the order of the VAR, but the question specifically tells us to use 2 lags.
plot(zt[,1])
lines(zt[,2], col="blue")
lines(zt[,3], col="red")
lines(zt[,4],col="green")
# This plotting serves to justify the inclusion of a trend in the cointegration relationship. However, for simplicity sace we will not include a trend.

vecm1=ca.jo(zt, type="trace",ecdet="none",K=2,spec="longrun")
summary(vecm1)
vecm1@teststat
vecm1@cval
vecm1@lambda
vecm1@Vorg
# We reject r=0 (no cointegrating vectors) at 5% significance. Given this, there must be at least one cointegrating relationship.
# Possibly more.

vecm2=ca.jo(zt, type="eigen", ecdet="none", K=2, spec="longrun")
summary(vecm2)
# We reject H0: r=0 and get the result that 1 and only 1 cointegrating vector exists.
# We do not reject H0: r=1 which gives robustness to our results.
# There exists only 1 cointegrating vector.
testvalues1=matrix(c(vecm1@teststat))
rownames(testvalues1)=c("r<=3","r<=2","r<=1","r=0")
colnames(testvalues1)=c("Test Statistic values")

testvalues2=matrix(c(vecm2@teststat))
rownames(testvalues2)=c("r<=3","r<=2","r<=1","r=0")
colnames(testvalues2)=c("Test Statistic values")
# EXERCISE B)

# The cointegrating vector is given by

coint=matrix(c(1.0000000,0.436967968,-1.014610727,0.005283738))
rownames(coint)=c("USA","GER","UK", "FR")
colnames(coint)=c("Cointegrating Vector")
coint

# EXERCISE c)


reg1=lm(zt[,1] ~ 0 + zt[,2] + zt[,3] + zt[,4])
summary(reg1)
resids=reg1$residuals
resids=resids[1:48]
adf.test(resids)
# This was altered in the jupyter notebook answer

# EXERCISE D)

vecm3=ca.jo(zt, type="eigen", ecdet="none", K=2, spec="longrun")
var1=vec2var(vecm3, r=1)
irf1=irf(var1, n.ahead=5, ci=0.95, runs=100)
plot(irf1)

fevd2=fevd(var1, n.ahead=5, ci=0.95, runs=100)
fevd2$USA
