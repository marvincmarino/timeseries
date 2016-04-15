# Tutorial from link:
# https://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html#reading-time-series-data

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
