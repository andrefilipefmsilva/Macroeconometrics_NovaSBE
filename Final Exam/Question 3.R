# EXERCISE a)

library(quantmod)
library(fGarch)
library(tseries)
library(forecast)
library(dynlm)
getSymbols("DJIA", src="FRED")
data=window(DJIA, start="2010-05-24", end="2020-05-23")
data=data[!is.na(data[,1])] #Removing NA values
plot(data)

# Although for the most part the the series looks non-stationary, there is a very big structural break related to the COVID-19 pandemic and as such it is hard to say just from the graph if it stationary or not.
# Structural breaks, especially as big as this cause bias. So, I will limit the analysis of the series to the Pre-Covid19 data - the threshold for this will be the maximum
# value of the series before the sudden drop.

which(data==max(data))
data1=data[1:2448]
plot(data1, type="l")

adf.test(data1)

# b


# EXERCISE b)

# Let's take first differences to check if the model becomes stationary like that.

diff_data=diff(data1)
diff_data=diff_data[!is.na(diff_data[,1])]
adf.test(diff_data) # The series is now stationary.


acf(diff_data) # Would conclude for MA(0) from this.
pacf(diff_data) # Very hard to analyse graph. 



AIC = matrix(nrow=8, ncol=8,dimnames=list(c(paste("p=",0:7)),(c(paste("q=",0:7)))))

for(i in 1:nrow(AIC)) {
  for(j in 1:ncol(AIC)) {
    AIC[i, j] = arima(diff_data, order=c(i-1,0,j-1))$aic
  }
}

AIC

# The results from running the AIC are not satisfactory. It returns the warning "possible convergence problem" many times.
# Stemming from this, I will trust the auto.arima function the come up with a properly fitted ARMA model, as the usual alternatives are
# not proving reliable.

arima1=auto.arima(data1)
arima1

acf(arima1$residuals)
Box.test(arima1$residuals)

# We check the model and find no statistically significant autocorrelation in the residuals. Hence, the model is valid and next

#EXERCISE 3C
# we will check for the existance of heteroskedasticity.

acf(arima1$residuals^2)

#The regression estimates show us that we find significant autocorrelation for the squared residuals.
#Consequently, we can conclude that the process has conditional heteroscedastic variance. 

Box.test(arima1$residuals^2, lag=2)
Box.test(arima1$residuals^2, lag=5)
Box.test(arima1$residuals^2, lag=10)

# Choosing the best GARCH model based on AIC

AIC = matrix(nrow=7, ncol=8, dimnames=list(c("q=1", "q=2", "q=3", "q=4", "q=5", "q=6", "q=7"), c("p=0", "p=1", "p=2", "p=3", "p=4", "p=5", "p=6", "p=7"))) 

AIC[1,1]= garchFit(~arma(2,2)+garch(1,0), data = diff_data)@fit$ics[1]
AIC[2,1]= garchFit(~arma(2,2)+garch(2,0), data = diff_data)@fit$ics[1] 
AIC[3,1]= garchFit(~arma(2,2)+garch(3,0), data = diff_data)@fit$ics[1] 
AIC[4,1]= garchFit(~arma(2,2)+garch(4,0), data = diff_data)@fit$ics[1] 
AIC[5,1]= garchFit(~arma(2,2)+garch(5,0), data = diff_data)@fit$ics[1] 
AIC[6,1]= garchFit(~arma(2,2)+garch(6,0), data = diff_data)@fit$ics[1] 
AIC[7,1]= garchFit(~arma(2,2)+garch(7,0), data = diff_data)@fit$ics[1] 
AIC[1,2]= garchFit(~arma(2,2)+garch(1,1), data = diff_data)@fit$ics[1] 
AIC[2,2]= garchFit(~arma(2,2)+garch(2,1), data = diff_data)@fit$ics[1] 
AIC[3,2]= garchFit(~arma(2,2)+garch(3,1), data = diff_data)@fit$ics[1] 
AIC[4,2]= garchFit(~arma(2,2)+garch(4,1), data = diff_data)@fit$ics[1] 
AIC[5,2]= garchFit(~arma(2,2)+garch(5,1), data = diff_data)@fit$ics[1] 
AIC[6,2]= garchFit(~arma(2,2)+garch(6,1), data = diff_data)@fit$ics[1] 
AIC[7,2]= garchFit(~arma(2,2)+garch(7,1), data = diff_data)@fit$ics[1] 
AIC[1,3]= garchFit(~arma(2,2)+garch(1,2), data = diff_data)@fit$ics[1] 
AIC[2,3]= garchFit(~arma(2,2)+garch(2,2), data = diff_data)@fit$ics[1] 
AIC[3,3]= garchFit(~arma(2,2)+garch(3,2), data = diff_data)@fit$ics[1] 
AIC[4,3]= garchFit(~arma(2,2)+garch(4,2), data = diff_data)@fit$ics[1] 
AIC[5,3]= garchFit(~arma(2,2)+garch(5,2), data = diff_data)@fit$ics[1] 
AIC[6,3]= garchFit(~arma(2,2)+garch(6,2), data = diff_data)@fit$ics[1] 
AIC[7,3]= garchFit(~arma(2,2)+garch(7,2), data = diff_data)@fit$ics[1] 
AIC[1,4]= garchFit(~arma(2,2)+garch(1,3), data = diff_data)@fit$ics[1] 
AIC[2,4]= garchFit(~arma(2,2)+garch(2,3), data = diff_data)@fit$ics[1] 
AIC[3,4]= garchFit(~arma(2,2)+garch(3,3), data = diff_data)@fit$ics[1] 
AIC[4,4]= garchFit(~arma(2,2)+garch(4,3), data = diff_data)@fit$ics[1] 
AIC[5,4]= garchFit(~arma(2,2)+garch(5,3), data = diff_data)@fit$ics[1] 
AIC[6,4]= garchFit(~arma(2,2)+garch(6,3), data = diff_data)@fit$ics[1] 
AIC[7,4]= garchFit(~arma(2,2)+garch(7,3), data = diff_data)@fit$ics[1] 
AIC[1,5]= garchFit(~arma(2,2)+garch(1,4), data = diff_data)@fit$ics[1] 
AIC[2,5]= garchFit(~arma(2,2)+garch(2,4), data = diff_data)@fit$ics[1] 
AIC[3,5]= garchFit(~arma(2,2)+garch(3,4), data = diff_data)@fit$ics[1] 
AIC[4,5]= garchFit(~arma(2,2)+garch(4,4), data = diff_data)@fit$ics[1] 
AIC[5,5]= garchFit(~arma(2,2)+garch(5,4), data = diff_data)@fit$ics[1] 
AIC[6,5]= garchFit(~arma(2,2)+garch(6,4), data = diff_data)@fit$ics[1]
AIC[7,5]= garchFit(~arma(2,2)+garch(7,4), data = diff_data)@fit$ics[1] 
AIC[1,6]= garchFit(~arma(2,2)+garch(1,5), data = diff_data)@fit$ics[1] 
AIC[2,6]= garchFit(~arma(2,2)+garch(2,5), data = diff_data)@fit$ics[1] 
AIC[3,6]= garchFit(~arma(2,2)+garch(3,5), data = diff_data)@fit$ics[1] 
AIC[4,6]= garchFit(~arma(2,2)+garch(4,5), data = diff_data)@fit$ics[1] 
AIC[5,6]= garchFit(~arma(2,2)+garch(5,5), data = diff_data)@fit$ics[1] 
AIC[6,6]= garchFit(~arma(2,2)+garch(6,5), data = diff_data)@fit$ics[1] 
AIC[7,6]= garchFit(~arma(2,2)+garch(7,5), data = diff_data)@fit$ics[1] 
AIC[1,7]= garchFit(~arma(2,2)+garch(1,6), data = diff_data)@fit$ics[1] 
AIC[2,7]= garchFit(~arma(2,2)+garch(2,6), data = diff_data)@fit$ics[1] 
AIC[3,7]= garchFit(~arma(2,2)+garch(3,6), data = diff_data)@fit$ics[1] 
AIC[4,7]= garchFit(~arma(2,2)+garch(4,6), data = diff_data)@fit$ics[1] 
AIC[5,7]= garchFit(~arma(2,2)+garch(5,6), data = diff_data)@fit$ics[1] 
AIC[6,7]= garchFit(~arma(2,2)+garch(6,6), data = diff_data)@fit$ics[1] 
AIC[7,7]= garchFit(~arma(2,2)+garch(7,6), data = diff_data)@fit$ics[1] 
AIC[1,8]= garchFit(~arma(2,2)+garch(1,7), data = diff_data)@fit$ics[1] 
AIC[2,8]= garchFit(~arma(2,2)+garch(2,7), data = diff_data)@fit$ics[1] 
AIC[3,8]= garchFit(~arma(2,2)+garch(3,7), data = diff_data)@fit$ics[1] 
AIC[4,8]= garchFit(~arma(2,2)+garch(4,7), data = diff_data)@fit$ics[1] 
AIC[5,8]= garchFit(~arma(2,2)+garch(5,7), data = diff_data)@fit$ics[1] 
AIC[6,8]= garchFit(~arma(2,2)+garch(6,7), data = diff_data)@fit$ics[1] 
AIC[7,8]= garchFit(~arma(2,2)+garch(7,7), data = diff_data)@fit$ics[1]




AIC==min(AIC)

# The appropriate model seems to be a GARCH(4,6)

garch=garchFit(~arma(1,1)+garch(2,2),data=diff_data)
summary(garch)

plot(residuals(garch, standardize=TRUE), type="l")
# *** EXERCISE 3D***
getSymbols("VIXCLS", src="FRED")

data2=window(VIXCLS, start="2010-05-24", end="2020-05-23")
data2=data2[!is.na(data2)]

fit=attr(garch,"fit")


# subsetting to start in 2010-05-21 and to end in 2020-02-12

plot(fit$series$h/10000, type="l", col="red")
lines(ts(data2),type="l",col="black")
