# Uber Economics Exercise 2.2
# Joe Silverstein
# 6-4-16

library(rjson)
library(dplyr)
library(tseries)
# library(tscount)
library(dlm)

setwd("/Users/joesilverstein/Google Drive/Uber")

### Part 1 ###

## Load and restructure data to create 15 MINUTE login counts

json_data = fromJSON(file = "logins (2).json")
df = data.frame(json_data$login_time)
names(df) = "login_time"
df$timestamp = as.numeric(as.POSIXct(df$login_time))

# Sort to make sure it converted correctly
df = df[order(df$timestamp), ]

# Aggregate by 15 minute time interval
breaksSeq = seq(from = min(df$timestamp), to = max(df$timestamp), by = 15*60)
dfAggregated = data.frame(table(cut(df$timestamp, breaks = breaksSeq)))
names(dfAggregated) = c("interval", "freq")

## Characterize time series of 15 minute login counts

# Graph time series
plot(dfAggregated$freq, pch = '.')
lines(dfAggregated$freq)
# There's too many data points and it's too discontinuous to make sense of it.

# Instead, first see if there's significant autocorrelation
autocorrelation = acf(dfAggregated$freq, lag.max = 100) 
autocorrelationFull = acf(dfAggregated$freq, lag.max = length(dfAggregated$freq)) 
# Autocorrelation is significant for almost all lags (especially early ones), since almost every autocorrelation is outside the blue dashed lines.
# Also, it looks like time series is periodic. Maybe should try spectral analysis?

# Test for stationarity
adf.test(dfAggregated$freq)
# p-value is very small, so we reject the null hypothesis of non-stationarity. That is, the test says the series is stationary.
# However, look at ACF of first-differenced series:
autocorrelationDiff = acf(diffFreq)
adf.test(diffFreq)
# It is very clearly stationary, with only one significant lag. This is much easier to fit in the next part of the question

# Raw periodogram
rawPeriodogram = spec.pgram(dfAggregated$freq)

# Smoothed periodogram (to be able to better see peaks)
# The Daniell kernel with parameter m is a centered moving average which creates a smoothed value at time t by averaging all values between 
# times t – m and t + m (inclusive)
smoothedPeriodogram4 = spec.pgram(dfAggregated$freq, kernel = kernel("daniell", m = 4))
smoothedPeriodogram20 = spec.pgram(dfAggregated$freq, kernel = kernel("daniell", m = 20))
smoothedPeriodogram40 = spec.pgram(dfAggregated$freq, kernel = kernel("daniell", m = 40))
# There are no obvious spectral peaks except at freq ~= 0. That is, it takes 1/(close to 0) periods to complete a cycle. 
# This could be happening because of a trend in the series. Try differencing to get rid of it.

diffFreq = diff(dfAggregated$freq)
smoothedPeriodogram40 = spec.pgram(diffFreq, kernel = kernel("daniell", m = 40))

# # http://stats.stackexchange.com/questions/1207/period-detection-of-a-generic-time-series
# find.freq <- function(x)
# {
#   n <- length(x)
#   spec <- spec.ar(c(x),plot=FALSE)
#   if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
#   {
#     period <- round(1/spec$freq[which.max(spec$spec)])
#     if(period==Inf) # Find next local maximum
#     {
#       j <- which(diff(spec$spec)>0)
#       if(length(j)>0)
#       {
#         nextmax <- j[1] + which.max(spec$spec[j[1]:500])
#         period <- round(1/spec$freq[nextmax])
#       }
#       else
#         period <- 1
#     }
#   }
#   else
#     period <- 1
#   return(period)
# }
# 
# spec = spec.ar(dfAggregated$freq)
# spec$freq[which.max(spec$spec)]
# (period = find.freq(dfAggregated$freq))
# 
# find.freq(diffFreq)
# This doesn't get us anywhere.

### Part 2 ###

# Based on the ACF, there should clearly be 1 AR lag. But also run with 2 and 3 just to make sure.
# Use 1 degree of differencing, as explained in part 1.
model = arima(x=dfAggregated$freq, order=c(1,1,0), method="ML")
(bic=AIC(model, k = log(length(dfAggregated$freq))))

# Check whether to add MA components by seeing if errors are serially correlated
modelResiduals = residuals(model)
Box.test(modelResiduals, type="Ljung-Box", lag=1) # reject null hypothesis of independence

# Add an MA term and do the analysis again
model1 = arima(x=dfAggregated$freq, order=c(1,1,1), method="ML")
(bic=AIC(model1, k = log(length(dfAggregated$freq))))
modelResiduals1 = residuals(model1)
Box.test(modelResiduals1, type="Ljung-Box", lag=1) # Accept null hypothesis of independence

# Conclude that the best ARIMA model is ARIMA(1,1,1)

# Do prediction 4 periods ahead
predictHour = predict(model1, n.ahead = 4) # not very accurate, based on the standard errors
predictHourRounded = round(predictHour$pred)

# Do prediction 1 week ahead
periodsPerWeek = 4*24*7
predictWeek = predict(model1, n.ahead = periodsPerWeek)
predictWeekRounded = round(predictWeek$pred)

# Since there is an MA component, the Kalman Filter will be better for prediction (see Hamilton book).


# # Note that it's not predicting count data. Should instead use Poisson ARIMA if it exists.
# poissonARMA = tsglm(ts=diffFreq, link="identity", model=list(past_obs = 1, past_mean = 1), distr="poisson")
# # Problem: Differenced series contains negative numbers, but support of Poisson distribution is the counting numbers.

# # INGARCH model with the identity link (see for example Ferland et al., 2006, Fokianos et al., 2009).
# # Conditional distribution is Poisson.
# poissonARMA = tsglm(ts=dfAggregated$freq, link="identity", model=list(past_obs = 1, past_mean = 1), distr="poisson")
# # This runs without errors. Does it need to be differenced first though?
# predictHour = predict(poissonARMA, n.ahead = 4) # Why isn't it predicting integer values?
# predictWeek = predict(poissonARMA, n.ahead = periodsPerWeek) # Takes a long time. It asymptotes and might have a trend.
# This doesn't work because the series is integrated, and values of the integrated series are not Poisson-distributed.

# Undifference the series if it didn't do it automatically.


