# Correct link https://s3.amazonaws.com/files.fred.stlouisfed.org/fred-md/Appendix_Tables_Update.pdf 

#EXERCISE 2A
setwd("C:\\Users\\Casa\\OneDrive\\Documentos\\Economia Nova SBE\\1st year\\2º Semestre\\Macroeconometrics\\Final 2020")
data=read.csv("data_final_2020_Q2.csv")

get_transf = function(rawdata) {
  
  n = 2:nrow(rawdata)   
  
  for (i in 2:ncol(rawdata))
    if (rawdata[1,i] == 2) {
      rawdata[n,i]= c(0,diff(rawdata[n,i]))
      
    }
  else if (rawdata[1,i] == 3) {
    rawdata[n,i]=c(0,diff(rawdata[n,i])^2)
    
  }
  else if (rawdata[1,i] == 4) {
    rawdata[n,i] = log(rawdata[n,i])
    
  }
  else if (rawdata[1,i] == 5) {
    rawdata[n,i] = c(0,diff(log(rawdata[n,i])))
    
  }
  else if (rawdata[1,i] == 6) {
    rawdata[n,i] = c(0,diff(log(rawdata[n,i]))^2)
    
  }
  else if (rawdata[1,i] == 7) {
    rawdata[n,i]= c(7,0,diff(rawdata[3:nrow(rawdata),i]/rawdata[2:(nrow(rawdata)-1),i]-1))
    
  }
  
  rawdata = rawdata[-c(1,2,736,737),]
  return(rawdata)
}

data1 = get_transf(data)
data1=data1[-c(1,2,737,736),]
data1=data1[,-c(1)]
# EXERCISE 2B

library(factoextra)


data2=scale(data1[,1:length(data1)],center=TRUE, scale=TRUE)
data2=data.frame(data2)


pca1=prcomp(na.omit(data2), center=FALSE, scale.=FALSE, rank=3)
data2=na.omit(data2)

summary(pca1)
head(pca1$x)

RPI=data2$RPI
UNRATE=data2$UNRATE

reg1=lm(RPI~pca1$x)
summary(reg1)
#R^2 of 0.1101

reg2=lm(UNRATE ~ pca1$x)
summary(reg2)
# R^2 of 0.31


## ** EXERCISE 2C ** ##

data3=scale(data2[,1:length(data2)],center=TRUE,scale=TRUE) #Need to rescale because of the deleted NAs

# Now we have standardized data


# Step1: Extract principal components of all X

pc_all=prcomp(data3, center=FALSE, scale.=FALSE, rank.=3)
C=pc_all$x
head(C)

# Step2: Principal component Extraction of slow variables

slow_vars = c("RPI", "W875RX1", "DPCERA3M086SBEA", "CMRMTSPLx", "RETAILx", "INDPRO", "IPFPNSS", "IPFINAL","IPCONGD", "IPDCONGD", "IPNCONGD", "IPBUSEQ", "IPMAT", "IPDMAT", "IPNMAT", "IPMANSICS", "IPB51222S", "IPFUELS", "CLF16OV", "CE16OV", "UEMPLT5","UEMP5TO14", "UEMP15OV", "UEMP15T26", "UEMP27OV", "CLAIMSx", "PAYEMS", "USGOOD", "CES1021000001","USCONS", "MANEMP", "DMANEMP", "NDMANEMP", "SRVPRD", "USTPU", "USWTRADE", "USTRADE", "USFIRE","USGOVT", "AMDMNOx", "AMDMUOx", "BUSINVx", "M1SL", "M2SL","M2REAL", "TOTRESNS", "BUSLOANS", "REALLN", "NONREVSL","CES0600000008","CES2000000008","CES3000000008", "MZMSL", "DTCOLNVHFNM", "DTCTHFNM", "INVEST", "HWI","HWIURATIO","UNRATE","UEMPMEAN","CES0600000007","AWOTMAN", "AWHMAN","ISRATIOx", "CONSPI")
data_slow=data3[, slow_vars]
pc_slow=prcomp(data_slow, center=FALSE, scale.=FALSE, rank. = 3)
f_slow= pc_slow$x

# Step3: Clean the PC from the effect of observed Y

reg = lm(C ~f_slow + data3[,"FEDFUNDS"])
summary(reg)

reg$coefficients

f_hat= C - data.matrix(data3[,"FEDFUNDS"])%*%reg$coefficients[5,]
head(f_hat)
dim(f_hat)


###*** STEP 4****
library(vars)
data_var = data.frame(f_hat, "FEDFUNDS" = data3[,"FEDFUNDS"])
var = VAR(data_var, p = 12)
#summary(var)

irf_point = irf(var, n.ahead = 60, impulse = "FEDFUNDS", response = "FEDFUNDS", boot = FALSE)

# Shock size of 26 basis points
impulse_sd = 0.25/sd(data2$FEDFUNDS)
scale = impulse_sd/(irf_point$irf$FEDFUNDS[1]) # position of FYFF response at step 0


# Computing Loading Factors
reg_loadings = lm(data3 ~ f_hat + data3[,"FEDFUNDS"])
loadings = reg_loadings$coefficients
# head(reg_loadings$coefficients)
#summary(reg_loadings)


#### BOOTSTRAPING ########
library(boot)
library(tsDyn)

R = 500 # Number of simulations
nvars = 128 # Number of variables
nsteps = 61 # numbers of steps

IRFs = array(c(0,0,0), dim = c(nsteps,nvars,R))

var = lineVar(data_var, lag = 12, include = "const")
for(j in 1:R){    
  data_boot = VAR.boot(var, boot.scheme ="resample")
  var_boot = VAR(data_boot, lag = 12)
  irf1 = irf(var_boot, n.ahead = 60, impulse = "FEDFUNDS", boot = FALSE)
  for(i in 1:nvars){
    IRFs[,i,j] = (irf1$irf$FEDFUNDS %*% matrix(loadings[2:5, i]))*scale
  }
} ## Boot simulations done

# Extract the quantiles of IRFs we are interested: 90% confidence interval in BBE
Upper = array(c(0,0), dim = c(nsteps, nvars))
for(k in 1:nsteps){
  for(i in 1:nvars){
    Upper[k,i] = quantile(IRFs[k,i,], probs = c(0.95))[1]
  }
}
Lower = array(c(0,0), dim = c(nsteps, nvars))
for(k in 1:nsteps){
  for(i in 1:nvars){
    Lower[k,i] = quantile(IRFs[k,i,], probs = c(0.05))[1]
  }
}
IRF = array(c(0,0), dim = c(nsteps, nvars))
for(k in 1:nsteps){
  for(i in 1:nvars){
    IRF[k,i] = quantile(IRFs[k,i,], probs = c(0.5))[1]
  }
}


# List of variables I'm interested in

variables = c(grep("^UNRATES$", colnames(data3)), grep("^INDPRO$", colnames(data3)), grep("^DPCERA3M086SBEA$", colnames(data3)),
              grep("^GS1$", colnames(data3)), grep("^AAA$", colnames(data3)))

transf_code = c(2, 5, 5, 2, 2)

variable_names = c("Civilian Unemployment Rate", "Industrial Production Index", "Real Personal Consumption Expenditures",
                   "1-year Treasury Rate", "Season Aaa Corporate Bond Yield")

library(repr)
par(mfrow=c(3,2)) 

for(i in variables){
  index = which(variables == i)
  if(transf_code[index] == 5){
    plot(cumsum(IRF[,i]), type ='l',lwd=2, main = variable_names[index],
         ylab= "", xlab="Steps", ylim=range(cumsum(Lower[,i]),cumsum(Upper[,i])),
         cex.main=1.8, cex.axis=1.3)
    lines(cumsum(Upper[,i]), lty=2, col="red")
    lines(cumsum(Lower[,i]), lty=2, col="red")
    abline(h=0)
  }
  else{
    plot(IRF[,i], type ='l',lwd=2, main = variable_names[index],
         ylab= "", xlab="Steps", ylim=range((Lower[,i]),(Upper[,i])),
         cex.main=1.8, cex.axis=1.3)
    lines((Upper[,i]), lty=2, col="red")
    lines((Lower[,i]), lty=2, col="red")
    abline(h=0)  
  }
}

data2$EXUSUKx
data3[,"EXUSUKx"]


## ** EXERCISE 2D** ##


favar=var
pred=predict(favar, n.ahead=1)


loadings2=reg_loadings$coefficients[,c("RPI","UNRATE")]
RPIpredict=loadings2[1,1] + loadings2[2,1]*pred[1]+loadings2[3,1]*pred[2]+loadings2[4,1]*pred[3]+loadings2[5,1]*pred[4]
RPIpredict=RPIpredict*sd(data1[,"RPI"])+mean(data1[,"RPI"])
forecast_RPI=exp(RPIpredict)+data[734,"RPI"]
forecast_RPI
real_RPI=data[735,"RPI"]
real_RPI

UNRATEpredict=loadings2[1,2] + loadings2[2,2]*pred[1]+loadings2[3,2]*pred[2]+loadings2[4,2]*pred[3]+loadings2[5,2]*pred[4]
UNRATEpredict=UNRATEpredict*sd(data1[,"UNRATE"])+mean(data1[,"UNRATE"])
forecast_UNRATE=UNRATEpredict+data[734,"UNRATE"]
forecast_UNRATE
real_UNRATE=data[735,"UNRATE"]
real_UNRATE
