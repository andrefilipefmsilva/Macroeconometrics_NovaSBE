library(quantmod)
library(tseries)
library(mFilter)
library(NTS)
library(MSwM)
#EXERCISE 4A
getSymbols("CLVMNACSCAB1GQPT",src="FRED")
ptgdp=CLVMNACSCAB1GQPT
plot(ptgdp)
adf.test(ptgdp)

#The series are not stationary.

hpgdp=hpfilter(ptgdp, freq=1600, type="lambda")
plot(hpgdp$cycle, type="l")
abline(h=0, col="red")
adf.test(hpgdp$cycle)

#At a 5% significance level, the cyclical component of the series is stationary.

#EXERCISE 4B
# https://cran.r-project.org/web/packages/NTS/NTS.pdf library documentation link
gdp_ts=ts(hpgdp$cycle)

mswitch=MSM.fit(gdp_ts,p=4,nregime=2,sw=c(T,T,T,T,T,T))
mswitch@Coef
#EXERCISE 4C
adf.test(mswitch@model$residuals) # The test indicates residuals stationarity, pointing towards model adequacy.
summary(mswitch)
# R^2 of 0.8942 on first regime and of 0.8623 on second regime. Very good fit
plot(mswitch@model)


# EXERCISE 4D
plot(mswitch@Fit@smoProb[,1], type="l",main="Regime 1")
plot(mswitch@Fit@smoProb[,2], type="l", main="Regime 2")
plotProb(mswitch, which=1)

###

# Decomposing GDP components
tsgdp=ts(as.vector(CLVMNACSCAB1GQPT), start = 1995, deltat=1/4)
stl_tsgdp=stl(tsgdp, "periodic")
cyclical=stl_tsgdp$time.series[,3]*100
plot(cyclical)
lines(mswitch@Fit@smoProb[,1], type="l", col="red")
abline(h=0, col="red")
