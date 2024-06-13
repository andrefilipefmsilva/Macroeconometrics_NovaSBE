##Exercise 3a

library(quantmod)
getSymbols("AMZN", src="yahoo", from="2007-12-01", to="2020-03-13")
View(AMZN)
daily_close_years=ts(as.vector(AMZN$AMZN.Close), deltat=1/365, start=2007)
daily_close=ts(as.vector(AMZN$AMZN.Close))
plot(daily_close_years)
plot(daily_close)
#Upon inspection of this plot, the series does not look stationary. However, I will decompose it
#according to the Loess decomposition to get the irregular part

stl_close = stl(daily_close, "periodic")
irreg_close= stl_close$time.series[,3]
plot(irreg_close)
abline(h=0, col="blue")

# Although the variance increases substantially from 2014 onwards,
# the series does look stationary according to this plot
acf(daily_close)
acf(daily_close_years)
pacf(daily_close_years)

# From the ACF, we seem to be looking at a highly persistent AR process, but it seems
# to be converging to 0 over time, which makes me conclude for stationarity.
# I also plotted the pacf below to give me extra indications and it seems to support my theory

##Exercise 3b



daily_returns=diff(log(AMZN$AMZN.Close))
plot(daily_returns)
abline(h=0.0000, col="red") # this isn't working for some reason 

#The daily returns do look stationary

View(daily_returns) # as we took first differences, the first observation disappeared. we need to delete it

daily_returns=daily_returns[2:3090]
View(daily_returns) # it is fixed now

acf(daily_returns)

# The ACF is not very conclusive as we have significant lags on 3, 16, perhaps 34...
# I wouldn't change my conclusion based on this, however. The lags that are significant, are 
# only marginally so

library(tseries)

# We will perform the Augmented Dickey-Fuller for stationarity
# H0: Unit Root
# H1: Stationarity

adf.test(AMZN$AMZN.Close)

# The close prices are not stationary in levels

adf.test(daily_returns)


# From the test, we conclude that the daily returns as we computed them are stationary

#EXERCISE 3C

# The series had already been first differenced, so to fit we will use c(2,0,1) - or it
# would difference again
arma21=arima(daily_returns, order=c(2,0,1), include.mean=TRUE)
arma21$coef

mean_arma21= 0.0009568662/(1+0.1917667561+0.0578583013)
mean_arma21*100

# Calculating for the mean of the model we get 0.0007657. In percentage we have it is 0.077%
# To lend credibility to my answer, I calculate the following:

#Escrever a "parte teórica de calcular este resultado" - está no caderno#

#EXERCISE 3D

acf(arma21$residuals, lag=10)

# Again, from this I would conclude for stationarity because there seems to be no persistence
#but I wil check with the Ljung box test

Box.test(arma21$residuals, lag=10, type="Ljung")

#Ho: No Autocorrelation in the residuals (Stationarity)
#H1: Autocorrelation in the residuals (Non-stationarity)

#Given the p-value, we do not reject the null hypothesis,
# and we conclude the series to be stationary.

#EXERCISE 3E

tsdailyreturns=as.ts(daily_returns)

new_arma= arima(tsdailyreturns, c(2,0,1), include.mean=TRUE)
library(forecast)
forecast1=forecast(new_arma, h=5)
plot(forecast1, xlim=c(3080,3095))


mean((forecast1$mean)*100)

#The mean of the daily stock returns for the next 5 days is positive. Hence, I would recommend buying Amazon Stock


#EXERCISE 3F

#The one-step ahead forecast is directly impacted by the MA, but from two steps-ahead onwards
# only the AR component affects our forecasts. As the AR components are small in absolute value
# the forecast quickly converges to the unconditional mean (as can be seen in the plot)