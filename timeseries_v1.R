# Tutorial from link:
# https://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html#reading-time-series-data

#### Reading data ####

# data 1:
# number of births per month in New York city, from January 1946 to December 1959 (originally collected by Newton).
births = scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries = ts(births, frequency=12, start=c(1946,1)); birthstimeseries

# data 2:
# monthly sales for a souvenir shop at a beach resort town in Queensland, Australia, for January 1987-December 1993
souvenir = scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries = ts(souvenir, frequency=12, start=c(1987,1)); souvenirtimeseries

# plot
plot.ts(birthstimeseries) # have a strong upwards trend
plot.ts(souvenirtimeseries) # sales data seems to be having seasonality

logsouvenirtimeseries = log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

#### Decomposing seasonal time series ####

library("TTR")

# get the components
birthstimeseriescomponents = decompose(birthstimeseries)
plot(birthstimeseriescomponents)

# adjusting by seasonality
birthstimeseriesseasonallyadjusted = birthstimeseries - birthstimeseriescomponents$seasonal

# lets see the difference between the original ts and the seasonaly adjusted
par(mfrow = c(2,1))
plot(birthstimeseries)
plot(birthstimeseriesseasonallyadjusted)


#### Exponential smoothing "plain vanilla" ####

# OK Exp smoothing for short term forecasts if timeseries can:
# - be described using additive model 
# - no seasonality

# read in rain data
rain = scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries = ts(rain,start=c(1813))
par(mfrow = c(1,1))
plot.ts(rainseries)

# run forecasting with holt-winters but using simply exp smoothing
rainseriesforecasts = HoltWinters(rainseries, beta=FALSE, gamma=FALSE); rainseriesforecasts # beta, gamma = 0 == exp smoothing
rainseriesforecasts$fitted # these are the predictions
plot(rainseriesforecasts) # plot actuals vs. forecast
rainseriesforecasts$SSE # sum of squares - what does this is supposed to tell me? Is good or bad?

# now lets execute an out-of-sample forecasting for 8 years with the trained exp smoothing
library("forecast")
rainseriesforecasts2 = forecast.HoltWinters(rainseriesforecasts, h=8); rainseriesforecasts2
plot.forecast(rainseriesforecasts2)

# lets see the goodness of the model
acf(rainseriesforecasts2$residuals, lag.max=20)
# observation: autocorrelation tends to zero as you increase the lag

Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")
# Here the Ljung-Box test statistic is 17.4, and the p-value is 0.6:
# there is little evidence of non-zero autocorrelations in the in-sample forecast errors at lags 1-20.
# meaning: the model is still very autocorrelated

# new cheach if forecasting error is white noise

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

plotForecastErrors(rainseriesforecasts2$residuals)
# seems that the residuals are white noise

#### Holt-Winters exponential smoothing ####

souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries); souvenirtimeseriesforecasts
souvenirtimeseriesforecasts$SSE
plot(souvenirtimeseriesforecasts)

# out-of-sample forecast for the next 4 years
souvenirtimeseriesforecasts2 = forecast.HoltWinters(souvenirtimeseriesforecasts, h=48)
plot.forecast(souvenirtimeseriesforecasts2)

# model goodness and residuals analysis
acf(souvenirtimeseriesforecasts2$residuals, lag.max=20)
par(mfrow = c(1, 2))
plot.ts(souvenirtimeseriesforecasts2$residuals); abline(h = 0) # fluctuating around 0?
plotForecastErrors(souvenirtimeseriesforecasts2$residuals) # are residuals white noise?
shapiro.test(souvenirtimeseriesforecasts2$residuals) # normality test on residuals


#### ARIMA models ####

# the skirts data is the annual diameter of womens skirst 
# good exmaple of additive time series with no trend nor seasonality

skirts = scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat", skip=5)
skirtsseries = ts(skirts,start=c(1866))
par(mfrow = c(1, 1))
plot.ts(skirtsseries, main = "annual diameter of womens skirts")

# 1. decide the order of differenciating parameter
# lets differenciate this ts to make it stationary
skirtsseriesdiff1 = diff(skirtsseries, differences=1)
plot.ts(skirtsseriesdiff1, main = "diff = 1")

# notice that by differenciating we lose 1 data point:
length(skirtsseries); length(skirtsseriesdiff1)

# diff 1 is still non-stationary, lets be more aggressive and diff again

skirtsseriesdiff2 = diff(skirtsseries, differences=2)
par(mfrow = c(2, 1))
plot.ts(skirtsseriesdiff1, main = "diff = 1")
plot.ts(skirtsseriesdiff2, main = "diff = 2")

# lets see if the diff 2 has less variance
hist(skirtsseriesdiff1); hist(skirtsseriesdiff2)
sd(skirtsseriesdiff1); sd(skirtsseriesdiff2)

# so it will be ARIMA(p,d,q) with d = 2

# lets see what the auto fit would say
auto.arima(skirtsseries)

# lets try same auto.arima with kings series:
kings = scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries = ts(kings)
kingtimeseriesdiff1 = diff(kingstimeseries, differences=1) # lets differenciate and compare
plot.ts(kingstimeseries)
plot.ts(kingtimeseriesdiff1)

# analyse autocorrelograms and partial autocorrelograms
par(mfrow = c(2, 1))
acf(kingtimeseriesdiff1, lag.max=20) # plot 
pacf(kingtimeseriesdiff1, lag.max=20) # plot 

acf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) # get autocorrelation values
pacf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) # get partial autocorrelation values

# lets see what the auto.arima has to say
auto.arima(kings)

# new data for ARIMA test: volcanic dust veil index in the northern hemisphere, from 1500-1969 
volcanodust = scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
par(mfrow = c(1, 1))
plot.ts(volcanodustseries)

par(mfrow = c(2, 1))
acf(volcanodustseries, lag.max=20)    
pacf(volcanodustseries, lag.max=20)  
auto.arima(volcanodustseries)


# lets make predictions with arima

# fit model
kingstimeseriesarima <- arima(kingstimeseries, order=c(0,1,1)) # fit an ARIMA(0,1,1) model
kingstimeseriesarima

# out-of-sample predictions for the next 5 periods
kingstimeseriesforecasts = forecast.Arima(kingstimeseriesarima, h=5)
kingstimeseriesforecasts
par(mfrow = c(1, 1))
plot.forecast(kingstimeseriesforecasts)
