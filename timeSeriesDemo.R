# Time Series Samples
# Excerpted from the Little book of Time Series in R
# http://a-little-book-of-r-for-time-series.readthedocs.org

# This time series is only to demonstrate how to decompose / extract seasonality
# and then forecast out including seasonality

# First, get the data we want. In this case it is births from NYC in the period ~1946 - 1959
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

# Convert to a time series and then plot it
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
plot.ts(birthstimeseries)

# Now decompose it into components
# Specifically a trend / seasonality / random
birthstimeseriescomponents <- decompose(birthstimeseries)

# Then plot it
plot(birthstimeseriescomponents)

# Now show it with seasonality removed
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal

# And plot that
plot(birthstimeseriesseasonallyadjusted)

# On to forecasting. Let's see what this looks like when you forecast out seasonality.
# This requires the forecast package so the script attempts to install, then load the package.

# First we want to approximate the data we have, using the Holt Winters algorithm

# the Holt Winters forecasting algorithm requires the log of the births time series
logbirthstimeseries <- log(birthstimeseries)
birthstimeseriesforecasts <- HoltWinters(logbirthstimeseries)
# Plot the two together.
plot(birthstimeseriesforecasts)

# The historical approximation is nice, but we want to look foreward in time too
# Load the forecast time series first.
install.packages("forecast")
library(forecast)

# parameter h is in months - so h=48 means we are forecasting ahead 4 yrs.
birthstimeseriesforecasts_4y <- forecast.HoltWinters(birthstimeseriesforecasts, h=48)

# plot the forecasts
plot.forecast(birthstimeseriesforecasts_4y)

# END OF TIME SERIES SEASONALITY OVERVIEW