# PROBLEM 4

library(readxl)
data= read_xls("C:\\Users\\Casa\\OneDrive\\Documentos\\Economia Nova SBE\\1st year\\2º Semestre\\Macroeconometrics\\Assignments\\PSet2\\ps_2_data.xls")


data=data[,-1]
datats=as.ts(data)


#Exercise 4A)

#H0: Non - stationarity
#H1: Stationarity

library(quantmod)
library(tseries)
adf.test(datats[,1])

# For a 5% significance level, unemployment rate is not stationary. But it would be stationary if we considered a 10% significance level.

adf.test(datats[,2])

# For a 5% significance level, Average Hourly Earnings are not stationary.

# EXERCISE 4B)
library(vars)
var1=VAR(datats, lag.max= 24, ic="AIC")
var1
plot(residuals(var1)[,1])
plot(residuals(var1)[,2])
var1_resid=residuals(var1)
acf(var1_resid)
serial.test(var1) # We believe the Portmanteu Test is the multivariate version of the Ljung-box Test
Box.test(var1_resid[,1])
# Both from plotting the ACF for the residuals, and from the test for serial correlation, we conclude the residuals of the VAR to be stationary.

# EXERCISE 4C)

irf1=irf(var1, n.ahead=664, ci=0.95, runs=100)
plot(irf1)

data4c=data
data4c[,2]=(log(data4c[,2]))*100
data4cts=as.ts(data4c)
var2=VAR(data4cts, lag.max =24, ic="AIC")
irf2=irf(var2, n.ahead=664, ci=0.95,runs=100)
plot(irf2)
irf2$irf$unrate
# When there is a shock to unemployment rate of around 0.15%, wages decrease by around -0.029% contemporaneously. The effect seems to be long-lasting and increasing throughout time, it is very persistent.

irf2$irf$wage

# Contemporaneously, shocks to wages do not affect unemployment rate: this is by construction, as required for the identification of our VAR model.
# Starting from one step-ahead, however, an increase of around 0.1482% in wage leads to increases in the unemployment rate, not very large in magnitude but persistent.

# EXERCISE 4D)

data4d= data[,c(2,1)]
data4dts=as.ts(data4d)
var3=VAR(data4dts, lag.max =24, ic="AIC")

irf3=irf(var3, n.ahead=664, ci=0.95, runs=100)
plot(irf3)

acf(residuals(var3))

# The ACF does not show autocorrelation the residuals.
serial.test(var3)

# We conclude for stationarity of the residuals of the VAR. Our conclusion does not change from the one found in b).

# EXERCISE 4E)

fevd1=fevd(var3, n.ahead=664, ci=0.95)
fevd1$wage

# We see that at impact, unemployment rate has zero relevance in explain movements in wages - which matches our identification restriction.
# However, as time progresses, unemployment rate is increasingly more relevant to explain moves in wages. We can see that 8 years ahead (96 steps-ahead) it explains about 28.19% of the movement in wages. It gets increasingly more important in explaining wages as time goes by.

fevd1$unrate

# When we decompose the proportions for movements in the unemployment rate sequence, we see that right from impact, wage shocks explains some of the change in unemployment rate. It keeps increasing but as time progresses it stabilizes at about 22.5% in terms of relative importance.


# EXERCISE 4F)

# Granger causality testing requires the variables of the VARs to be stationary. We need thus to have stationary time series.
# in Question 4a) we saw that none of the two variables are stationary at a 5% level. Since we have monthly data, it is likely that first differencing alone will not solve our problem, but let's try.

firstdiff=diff(data4dts)

adf.test(firstdiff[,2])

# First differencing unemployment rate makes it stationary.

adf.test(firstdiff[,1])

# However, first differencing wages does not make it stationary yet.

# We will try 2nd differences for both variables.

seconddiff=diff(firstdiff)

adf.test(seconddiff[,1])

# The wages series is now stationary as well. We can proceed towards granger causality testing.

var4=VAR(seconddiff, lag.max = 20, ic="AIC")

# Does unemployment rate Granger-Cause wages?

causality(var4, cause=c("unrate"))

# From the F-Test p-value, we can reject the null hypothesis, and therefore conclude that unemployment rate Granger-causes wages.