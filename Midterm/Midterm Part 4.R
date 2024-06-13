#EXERCISE 4

library(readxl)
data4=read_xlsx("C:\\Users\\andre\\OneDrive\\Documentos\\Economia Nova SBE\\1st year\\2º Semestre\\Macroeconometrics\\Midterm 2020\\data_midterm_2020.xlsx")
View(data4)

#EXERCISE 4A

data4a= data4[,-1]
data4a= ts(data4a, start=1995, deltat=1/4)
data4a=(log(data4a))*100
data4a=diff(data4a)
data4a=data4a[,c(3,5,2,4,1)]

#EXERCISE 4B
library(vars)
library(tseries)
var1= VAR(data4a[,1:5], lag.max=12, ic="SC")
var1$varresult
adf.test(data4a[,1])

# G is stationary at a 5% significance level 

adf.test(data4a[,2])

# Tax is NOT stationary at a 5% significance level 

adf.test(data4a[,3])

# C also NOT stationary at a 5% significance level

adf.test(data4a[,4])

# I is barely within the limit. But not stationary at a strict 5% significance level

adf.test(data4a[,5])

# GDP not stationary at a 5% significance level

# Most variables are not stationary, so unless there is some cointegration among them, the VAR model is not adequate
# However, there is more than likely cointegration among them.
# But IRFs do not require stationarity (which is what comes next)

#EXERCISE 4c

#IRFs do not require stationarity, so we can proceed
# 20 steps ahead means 5 years ahead since we have quarterly data

irf1=irf(var1, n.ahead=20, ci=0.95, runs=100)
irf1$irf$G
plot(irf1)

# From the "Orthogonal Impulse Response from G", and from the VAR, we can see that a positive shock to government spending today will lead to an increase at t+1 in consumption, GDP. Taxation is not discernibly affected by a shock from government expenditure. Investment is impacted in an oscillatory manner, first going down and then up again over some periods.
# Se tiver tempo, meter estes gráficos bonitos
par(mfrow=c(3,2))
stepSize = 1
xMax = 25
numTicks = xMax / stepSize
v1 = c(0:numTicks)*stepSize
plot(irf1$irf$G[,1], type ='l',lwd=2, main="Orthogonal Impulse Response from G", ylab="G", xlab="Steps", xaxt="n", ylim=range(irf1$Lower$G[,1],irf1$Upper$G[,1]))
lines(irf1$Upper$G[,1], lty=2, col="red")
lines(irf1$Lower$G[,1], lty=2, col="red")
axis(side=1, at=v1, labels=v1)
abline(h=0)
plot(irf1$irf$G[,2], type ='l',lwd=2, main="Orthogonal Impulse Response from G", ylab= "TAX", xlab="Steps",xaxt="n", ylim=range(irf1$Lower$G[,2],irf1$Upper$G[,2]))
lines(irf1$Upper$G[,2], lty=2, col="red")
lines(irf1$Lower$G[,2], lty=2, col="red")
axis(side=1, at=v1, labels=v1)
abline(h=0)
plot(irf1$irf$G[,3], type ='l',lwd=2, main="Orthogonal Impulse Response from G", ylab= "C", xlab="Steps",xaxt="n", ylim=range(irf1$Lower$G[,3],irf1$Upper$G[,3]))
lines(irf1$Upper$G[,3], lty=2, col="red")
lines(irf1$Lower$G[,3], lty=2, col="red")
axis(side=1, at=v1, labels=v1)
abline(h=0)

plot(irf1$irf$G[,4], type ='l',lwd=2, main="Orthogonal Impulse Response from G", ylab= "I", xlab="Steps",xaxt="n", ylim=range(irf1$Lower$G[,4],irf1$Upper$G[,4]))
lines(irf1$Upper$G[,4], lty=2, col="red")
lines(irf1$Lower$G[,4], lty=2, col="red")
axis(side=1, at=v1, labels=v1)
abline(h=0)
plot(irf1$irf$G[,5], type ='l',lwd=2, main="Orthogonal Impulse Response from G", ylab= "GDP", xlab="Steps",xaxt="n", ylim=range(irf1$Lower$G[,5],irf1$Upper$G[,5]))
lines(irf1$Upper$G[,5], lty=2, col="red")
lines(irf1$Lower$G[,5], lty=2, col="red")
axis(side=1, at=v1, labels=v1)
abline(h=0)

#EXERCISE 4D

fevdg=fevd(var1, n.ahead=20)
fevdg$G
fevdg
# G shocks explain about 1.05% of the variation in GDP right after t+1 and this value keeps increasing on the 20 steps ahead until it reaches 1.92%. We could say it has a relatively low importance in explaining variation in GDP.

fevdg$GDP

# If we look at C shocks, for instance, they're much more important than G shocks in explaining variation in GDP

#EXERCISE 4E
library(forecast)
data4e=data4[1:99,]
data4e=data4e[,-1]
data4ets= ts(data4e, start=1995, deltat=1/4)
data4ets=(log(data4ets))*100
data4ets=diff(data4ets)
data4ets=data4ets[,c(3,5,2,4,1)]
var2= VAR(data4ets[,1:5], lag.max=12, ic="SC")
varforecast=predict(var2, n.ahead=1, ci=0.95)
arimaforecast=auto.arima(data4e$GDP)


data4$GDP[100]
(varforecast$fcst$GDP)*100
forecast(arimaforecast, h=1)

# The forecasts for the VAR have such huge variation and are so far off the mark that I'm actually not sure I did everything right. However, if I did not make any mistakes, this shows that the ARIMA model is so much better because the prediction is much much closer to the observed value than the VAR one, and the standard error is much lower.