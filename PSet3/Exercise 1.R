# PROBLEM 1

setwd("C:\\Users\\Casa\\OneDrive\\Documentos\\Economia Nova SBE\\1st year\\2º Semestre\\Macroeconometrics\\Assignments\\Pset3")
library(xlsx)
library(vars)
library(urca)
library(tseries)
library(knitr) 

# **EXERCISE 1A**


data=read.xlsx("ps3_data_Q1_2020.xlsx", header=TRUE, sheetIndex =1)
df=data.frame("G_Health"=data[,3], "Revenue_Total"=data[,2])
dfts=ts(df)
row.names(dfts)=c(1970:2018)

# **EXERCISE 1B**

# When you say "test for all series", I am assuming it is for all the series we included in the data frame (G_health and Revenue_Total)
plot(dfts[,1])
adf.test(dfts[,1])

# For health expenditure, we do not reject the null hypothesis of non-stationarity. So we conclude for the existence of a unit root.
# From the graph, it was already trivial to see that the series were not stationary.

plot(dfts[,2])

# From this graph it is more dubious if there is stationarity. 

adf.test(dfts[,2])

# From here, we also conclude for non-stationarity of the total revenue series. 

#Since the adf.test function detrends the series for testing, to grant the test more power, we can conclude that detrending would not make the series stationary.
#Both series are non-stationary, so our only chance at relating them with each other is if there exists some kind of cointegration between them.


# **EXERCISE 1C**
plot.ts(dfts[,1], dfts[,2], plot.type=c("multiple"))
# 1. Pretesting the variables for their order of integration

# Since the adf.test in levels shows us existence of unit root in both series, let us see what we can in first differences

plot(diff(dfts[,1]))
adf.test(diff(dfts[,1]))

# The Health expenditure series is I(1)

plot(diff(dfts[,2]))
adf.test(diff(dfts[,2]))

# The total revenue series is I(1)

# Both series are integrated of order 1 - I(1)

# 2. Selecting the order of the VAR with undifferenced data
VARselect(dfts) # select 2 lags

# 3. Estimating the model and determining the rank of $\pi$. 
# Trace test
#H0: rank of pi <= r
#H1: rank of pi > r
vecm1=ca.jo(dfts, type="trace", K=2, spec="longrun",ecdet="const")
summary(vecm1)

# We reject the null hypothesis at a 10% significance level.

# Eigen-test 
#H0: rank of pi = r
#H1: rank of pi= r+1
vecm2=ca.jo(dfts, type="eigen",K=2,spec="longrun",ecdet="const")
summary(vecm2)

# We also reject the null hypothesis at a 10% significance level. 
# We conclude the rank of pi to be 1.

# Cointegrating vector

coint = matrix(c(1.00000000 , -4.930188, 155.651228)) 
rownames(coint)= c('G_health', 'Revenue_Total', 'constant') 
colnames(coint)= c('Cointegrating Vector') 
library(knitr) 
kable(coint)


# ** EXERCISE 1D** 

# Speed of adjustment

speed_adjust = matrix(c(-0.004587278 ,0.071273797)) 
rownames(speed_adjust)= c('G_health', 'Revenue_Total') 
colnames(speed_adjust)= c('Adjustment Coefficients') 
kable(speed_adjust)


# ** EXERCISE 1E**

var1=vec2var(vecm1, r=1)
irf1=irf(var1, n.ahead=30, ci=0.95, runs=100)
plot(irf1$irf)

var2=VAR(dfts, lag.max=6, ic="AIC")
var2
irf2=irf(var2, n.ahead=30, ci=0.95, runs=100)



