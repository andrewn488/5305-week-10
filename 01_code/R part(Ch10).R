## Part 1 Simulation ###

#P258 Generate time trend
t=c(1:50) #generate t=1,2,...50
y=1+.5*t+rnorm(50) #y=1+.5*trend + normal white noise
ts.plot(y) #plot y
lines(.5*t) #also plot the trend


#P271 Generate randm walk
y=numeric(500) #generate a vector of 500 zeros
for (i in 2:500){ #start a for loop
  y[i]=y[i-1]+rnorm(1) #generate random walk without drift
} #end the loop
ts.plot(y) #plot y
lines(numeric(500)) #also plot horizontal line at zero

ts.plot(cumsum(rnorm(500))) #another way to generate random walk without drift


y=numeric(500) #generate a vector of 500 zeros
for (i in 2:500){ 
  y[i]=.5+y[i-1]+rnorm(1) #generate random walk with drift
}
ts.plot(y) #plot y
lines(.5*c(1:500)) #also plot trend .5*t


# P277 model for case I, showin in fig10.8a (You should run this serveral times to see the variation)
y=numeric(500) #generate a vector of 500 zeros
for (i in 2:500){ 
  y[i]=0.9988*y[i-1]+rnorm(1) #generate random walk without drift
}
ts.plot(y) #plot y
lines(0*c(1:500)) #also plot trend .5*t

# P277 model for case II 
y=numeric(500) #generate a vector of 500 zeros
for (i in 2:500){ 
  y[i]=-0.325+0.9905*y[i-1]+rnorm(1) #generate random walk with drift
}
ts.plot(y) #plot y
lines(0*c(1:500)) #also plot trend .5*t

# P277 model for case III showin in fig10.8b
y=numeric(500) #generate a vector of 500 zeros
for (i in 2:500){ 
  y[i]=0.9167+0.0082i+0.9830*y[i-1]+rnorm(1) #generate random walk with drift and a time trend
}
ts.plot(y) #plot y
lines(0*c(1:500)) #also plot trend .5*t

## Part 2 Deterministic trend  ###
#P266
library(readxl)
data <- read_excel("Figure10_5_MortgageDebt.xls")
debt<-ts(data$`MORTGAGE DEBT`, frequency =4, start = c(1992,1)) #declare debt as quarterly time series
plot(debt)

trend1 = ts(c(1:49), frequency = 4, start = c(1992,1)) #trend1=1,2,3...60, declared to be time-series
model <-lm(debt ~ poly(trend1, 4, raw=TRUE)) # y=b0+b1*t+b2*t^2+b3*t^31+b4*t^4+WN
summary(model) #estimation output

AIC(model) #AIC
## [1] 434.5619
BIC(model) #BIS
## [1] 445.9129

plot.ts(debt, ylab="", lty=2) #plot debt w/o y-axis title, dashed
fit = ts(fitted(model), frequency = 4, start=c(1992,1)) #declare fit as quarterly time series
res = ts(resid(model), frequency = 4, start=c(1992,1)) #declare res as quarterly time series
lines(fit, col="red", lty=2) #also plot fit in red color
par(new=TRUE) #following series will be graphed on the existing plot
plot.ts(res, axes=FALSE, ylab="", col="blue") #plot residuals w/o y-axis title, in blue color
axis(side=4, at = pretty(range(res))) #add the right y-axis for residuals
legend("topleft", legend=c("Actual", "Fit", "Res"), lty=c(2,2,1), col=c("black", "red", "blue")) #legend, debt and fit will be dashed 

acf(res) #ACF of residuals of the 4th order polynomial trend model
pacf(res) #PACF of residuals of the 4th order polynomial trend model

#P268
#install.packages("dynlm") #install dynamic linear modelling package
library(dynlm) #dynlm is required for using lag operators
model <- dynlm(debt ~ trend1+I(trend1^2)+I(trend1^3)+I(trend1^4)+lag(debt,-1)+lag(debt,-2)) #AR(2) + polynomial(4) trend
summary(model) #estimation output

AIC(model) #AIC
## [1] 394.7132
BIC(model) #BIS
## [1] 409.5144

#P269

plot.ts(debt, ylab="", lty=2) #plot debt w/o y-axis title, dashed
fit = ts(fitted(model), frequency = 4, start=c(1992,3)) #declare fit as quarterly time series, note that fitted values start drom 1992q3 because it is an AR(2) model
res = ts(resid(model), frequency = 4, start=c(1992,1)) #declare res as quarterly time series
lines(fit, col="red", lty=2) #also plot fit in red color
par(new=TRUE) #following series will be graphed on the existing plot
plot.ts(res, axes=FALSE, ylab="", col="blue") #plot residuals w/o y-axis title, in blue color
axis(side=4, at = pretty(range(res))) #add the right y-axis for residuals
legend("topleft", legend=c("Actual", "Fit", "Res"), lty=c(2,2,1), col=c("black", "red", "blue")) #legend, debt and fit will be dashed 


## Part 3 Dickey-Fuller Test   ###
#P281
library(readxl)
data <- read_excel("Figure10_11_Hours_USA_Spain.xls")
USA = ts(data$Hoursworked_USA, frequency = 1, start = 1970)
SPAIN = ts(data$HoursWorked_SPAIN, frequency = 1, start = 1970)

#P283
#install.packages("urca") #Unit Root and Cointegration Tests for Time Series Data
library(urca) #use urca package, allows choosing drift and/or trend for ADF test

ur.df(SPAIN,type="drift",lags=0) 
ur.df(diff(SPAIN),type="drift",lags=0) 
ur.df(diff(diff(SPAIN)),type="drift",lags=0) 

## 
## ############################################################### 
## # Augmented Dickey-Fuller Test Unit Root / Cointegration Test # 
## ############################################################### 
## 
## The value of the test statistic is: 0.1408 0.2265
#install.packages("dynlm") #Dynamic Linear Modelling
library(dynlm) #use dynlm package
dSPAIN<-diff(SPAIN)
model <-dynlm(dSPAIN ~ lag(SPAIN,-1)) #regression correspondong to the above ADF test
summary(model) #estimation output
AIC(model)
## [1] 171.2994
BIC(model)
## [1] 175.6966
res<-  residuals(model) #generate residuals
acf(res)
pacf(res)

#P286

ur.df(SPAIN,type="drift",lags=1) 

model <-dynlm(dSPAIN ~ lag(SPAIN,-1)+lag(dSPAIN,-1)) #regression corresponding to the above ADF test
summary(model)

AIC(model)
## [1] 134.9974
BIC(model)
## [1] 140.7334
res<-  residuals(model)
acf(res)
pacf(res)


# P287 Prepare to forecast table 10.8 
model <-dynlm(dSPAIN ~ lag(dSPAIN,-1)+0) #AR(1) w/o constant for differenced series
summary(model)
AIC(model)
## [1] 135.857
BIC(model)
## [1] 138.725

fcast=union(SPAIN, c(1,2,3,4)) #fcast will include actual SPAIN and 4 forecasts for SPAIN
fcastd=union(dSPAIN, c(1,2,3,4)) #fcastd will include actual dSPAIN and 4 forecasts for dSPAIN

for(i in 1:4){
  fcastd[32+i]=coef(model)[1]*fcastd[31+i] #manually retrieving forecasts from AR(1) model for dSPAIN
  fcast[33+i]=fcast[32+i]+fcastd[32+i] #the forecast for fcast_t = fcast_t-1 +fcastd_t (note that fcastd is one period "behind", thats why the last term is fcastd[32+i], and not fcastd[33+i])
}

# P288, fig 10.13
fcast = ts(fcast, frequency = 1, start = 1970)
ts.plot(fcast) #plot of the forecast
lines(SPAIN, col=6) #simualtaneous plotting of the actual series in magenta color


#P289 do the same for the u.s.
ur.df(USA,type="trend",lags=1) #ADF for USA with trend and constat
model <- dynlm(diff(USA) ~ lag(USA,-1)+diff(lag(USA,-1))+trend(USA)) #corresponding regression
summary(model)
AIC(model)
## [1] 104.6664
BIC(model)
## [1] 111.8363
model <- dynlm(USA ~ lag(USA,-1)+lag(USA,-2)+trend(USA)) #AR(2) with trend and constant
summary(model)
AIC(model)
## [1] 104.6664
BIC(model)
## [1] 111.8363




