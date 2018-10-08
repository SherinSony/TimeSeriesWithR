
#################################################################
#               TIME SERIES FORECASTING 
#################################################################

#Open data set from : https://data.gov.in/search/site?query=paddy+price&field_search=&item=100
#see this chart first : https://otexts.org/fpp2/arima-r.html


install.packages("TTR")
library(TTR)

#see how time series data should look like
#data(AirPassengers)
#x=AirPassengers
#str(x)
#Time-Series [1:144] from 1949 to 1961: 112 118 132 129 121 135 148 148 136 119 ...

df=read.csv("paddy.csv")
df$Date_Of_Price=as.Date(df$Date_Of_Price, format="%d-%m-%Y")
plot(df)
ts=ts(df$Paddy_Price,frequency = 12,start=c(2011,4))  #year, month
class(ts) #it should be "ts" only. If we use entire df cols, we will get ts, matrix, etc.
plot(ts)
plot(decompose(ts))
dts=decompose(ts)
plot(dts$seasonal)
plot(dts$trend)
tr=dts$trend
se=dts$seasonal
plot(cbind(tr,tr*se))
plot.ts(ts)

#conclusion : this ts has seasonality and trend


##------------------------------- Smoothening techniques ---------------------------
# see table :https://otexts.org/fpp2/taxonomy.html


##--------SMA from TTR package - Simple Moving Average calculations
#in Moving Averages the past observations are weighted equally unlike ES methods.
smadf<- TTR::SMA(ts ,n=4) #bigger the n value more the points will get combined and averaged
plot(smadf)
plot(df$Paddy_Price) #compare with above


#---------------- ES(Exponential smoothing) methods ---------------------
#Exponential Smoothing assigns exponentially decreasing weights as the observation get older. In other words, recent observations are given relatively more weight in forecasting than the older observations.

#ses from forecast package
#This method is suitable for forecasting data with no clear trend or seasonal pattern. Not useful here but just mentioning

library(forecast)
sests=forecast::ses(ts, h=10) #forecasted value for the next 10 months
plot(sests)
sests #to see the forecasted value for the next 10 months
summary(sests)

# Forecast method: Simple exponential smoothing
# 
# Model Information:
#   Simple exponential smoothing 
# 
# Call:
#   ses(y = ts, h = 10) 
# 
# Smoothing parameters:
#   alpha = 0.9999 
# 
# Initial states:
#   l = 97.4998 
# 
# sigma:  1.579
# 
# AIC     AICc      BIC 
# 390.0823 390.4252 396.9945 
# 
# Error measures:
#   ME     RMSE      MAE       MPE      MAPE      MASE      ACF1
# Training set 0.6811515 1.557556 1.140615 0.5550998 0.9135698 0.1165235 0.5419242
# 
# Forecasts:
#   Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# Jun 2017          147.9 145.8763 149.9236 144.8051 150.9948
# Jul 2017          147.9 145.0383 150.7617 143.5234 152.2765
# Aug 2017          147.9 144.3952 151.4048 142.5399 153.2601
# Sep 2017          147.9 143.8530 151.9469 141.7107 154.0892
# Oct 2017          147.9 143.3754 152.4246 140.9802 154.8197
# Nov 2017          147.9 142.9435 152.8564 140.3198 155.4802
# Dec 2017          147.9 142.5464 153.2535 139.7124 156.0875
# Jan 2018          147.9 142.1768 153.6231 139.1471 156.6528
# Feb 2018          147.9 141.8296 153.9703 138.6162 157.1837
# Mar 2018          147.9 141.5013 154.2986 138.1140 157.6859

round(forecast::accuracy(sests),2)
# ME RMSE  MAE  MPE MAPE MASE ACF1
# Training set 0.68 1.56 1.14 0.56 0.91 0.12 0.54


p=forecast::autoplot(sests) + forecast::autolayer(sests$fitted, series="Fitted")
p$labels$x="Month of Year"
p$labels$y="Paddy Price in Rupees"
p #to run the plot again.

#Note the plotted "one-step-ahead" fitted values alongside the data : https://stackoverflow.com/questions/41237309/one-step-ahead-out-of-sample-forecast-from-only-one-value-received-at-a-time-i



#---------------Triple ES with HoltWinters()-----------------------
#Paddy has both Trend and Seasonality so we will use HW for triple ES (alpha, beta and gamma)

hw_triple=HoltWinters(ts)
plot(hw_triple)
hw_triple

# Holt-Winters exponential smoothing with trend and additive seasonal component.
# 
# Call:
#   HoltWinters(x = ts)
# 
# Smoothing parameters:
#   alpha: 0.8116069
# beta : 0.1312034
# gamma: 1
# 
# Coefficients:
#   [,1]
# a   148.7322634
# b     0.5899210
# s1   -0.1607628
# s2    1.3013015
# s3    2.6064806
# s4    3.3684228
# s5    2.7066913
# s6    1.0163001
# s7   -1.2737798
# s8   -2.2373703
# s9   -2.2835067
# s10  -3.1474808
# s11  -2.0298391
# s12  -0.8322634

# The value of alpha is high indicating that the estimate of the level at the current time point is based upon  recent observations and few observations in the more distant past. 
# The value of beta is nearly 0.00, indicating that the estimate of the slope b of the trend component is not updated over the time series, and instead is set equal to its initial value. This makes good intuitive sense, as the level changes quite a bit over the time series, but the slope b of the trend component remains roughly the same. 
# In contrast, the value of gamma is high, indicating that the estimate of the seasonal component at the current time point is just based upon very recent observations.


hw_triple$SSE  #sum of squared errors
#[1] 173.1214

# by default HoltWinters() just makes forecasts for the time period covered by the original data, which is 1813-1912 for the rainfall time series. We can make forecasts for further time points by using the “forecast.HoltWinters()” function in the R “forecast” package. 

#To make forecasts for future times not included in the original time series, we use the “forecast.HoltWinters()” function in the “forecast” package. 

#now after ensuring the fit, we can now predict/forecast for the future
install.packages("forecast")
library(forecast)

#use existing fitted HW model for forecasting:
plot(forecast::forecast(hw_triple,h=20))
#or
plot(forecast:::forecast.HoltWinters(hw_triple,h=20)) #note 3 colons
#or
plot(stats:::predict.HoltWinters(hw_triple,n.ahead = 20))  #note 3 colons

#The forecasts are shown as a blue line, with the 80% prediction intervals as a dark shaded area, and the 95% prediction intervals as a light shaded area

#or to change colors - Orange 95% , yellow 80%
#95% confidence interval is yt±1.96σ
#80% confidence interval is yt±1.28σ
#Where yt is a forecast value and σ "is an estimate of the standard deviation of the forecast distribution"

plot(forecast:::forecast.HoltWinters(hw_triple,h=20,level = c(95,80)), shadecols = c("orange","yellow")) #note 3 colons

#get the values
myfore=forecast:::forecast.HoltWinters(hw_triple,h=20,level = c(95,80))
myfore

# Point Forecast    Lo 95    Hi 95    Lo 80    Hi 80
# Jun 2017       149.1614 145.8636 152.4592 147.0051 151.3177
# Jul 2017       151.2134 146.7365 155.6903 148.2861 154.1407
# Aug 2017       153.1085 147.4996 158.7174 149.4411 156.7759
# Sep 2017       154.4604 147.7245 161.1963 150.0560 158.8647
# Oct 2017       154.3886 146.5127 162.2644 149.2388 159.5383
# Nov 2017       153.2881 144.2507 162.3255 147.3788 159.1973
# Dec 2017       151.5879 141.3626 161.8132 144.9020 158.2739
# Jan 2018       151.2143 139.7721 162.6564 143.7327 158.6959
# Feb 2018       151.7580 139.0688 164.4473 143.4610 160.0551
# Mar 2018       151.4840 137.5166 165.4514 142.3512 160.6168
# Apr 2018       153.1916 137.9147 168.4684 143.2025 163.1806
# May 2018       154.9791 138.3614 171.5967 144.1134 165.8448
# Jun 2018       156.2405 138.0039 174.4771 144.3162 168.1648
# Jul 2018       158.2925 138.6706 177.9143 145.4624 171.1225
# Aug 2018       160.1876 139.1479 181.2273 146.4304 173.9447
# Sep 2018       161.5394 139.0499 184.0289 146.8343 176.2445
# Oct 2018       161.4676 137.4970 185.4382 145.7941 177.1411
# Nov 2018       160.3671 134.8848 185.8495 143.7051 177.0291
# Dec 2018       158.6670 131.6428 185.6912 140.9968 176.3371
# Jan 2019       158.2933 129.6978 186.8889 139.5957 176.9909


plot(myfore$residuals)
acf(myfore$residuals, lag.max =20,na.action = na.pass) #without na.action it will fail
#plot looks good

Box.test(myfore$residuals, lag=20, type="Ljung-Box") #seems good
#If the p value is greater than 0.05 then the residuals are independent which we want for the model to be correct.

# Box-Ljung test
# 
# data:  myfore$residuals
# X-squared = 19.385, df = 20, p-value = 0.4969


#------ES using automated ETS method ---------------
#An alternative to estimating the parameters by minimising the sum of squared errors is to maximise the “likelihood”. The likelihood is the probability of the data arising from the specified model. With the ets() function, the default estimation method is maximum likelihood rather than minimum sum of squares.

myets=forecast::ets(ts)

# summary(myets)
# ETS(M,Ad,N) 
# 
# Call:
#   forecast::ets(y = ts) 
# 
# Smoothing parameters:
#   alpha = 0.9999 
# beta  = 0.8123 
# phi   = 0.8 
# 
# Initial states:
#   l = 96.1838 
# b = 1.2645 
# 
# sigma:  0.0102
# 
# AIC     AICc      BIC 
# 362.7057 363.9594 376.5301 
# 
# Training set error measures:
#   ME     RMSE       MAE      MPE      MAPE       MASE       ACF1
# Training set 0.1517182 1.221302 0.9751211 0.129346 0.7799746 0.09961692 0.03036488

round(forecast::accuracy(myets),2)
# ME RMSE  MAE  MPE MAPE MASE ACF1
# Training set 0.15 1.22 0.98 0.13 0.78  0.1 0.03

#no forecasts will be given . ets() is used only to get a good model parameters.
p=forecast::autoplot(myets) + forecast::autolayer(myets$fitted, series="Fitted")
p$labels$x="Month of Year"
p$labels$y="Paddy Price in Rupees"
p #to run the plot again.

forecast::autoplot(myets$residuals)

plot(forecast:::forecast(myets, h=20))
myforeets=forecast:::forecast(myets, h=20)
plot(myforeets$residuals) #seems fine
Box.test(myforeets$residuals, lag=20, type="Ljung-Box")  #seems fine , just escaped

# Box-Ljung test
# 
# data:  myforeets$residuals
# X-squared = 29.798, df = 20, p-value = 0.07319




#------------------------ ARIMA ---------------------------
# Exponential smoothing methods are useful for making forecasts, and make no assumptions about the correlations between successive values of the time series. However, if you want to make prediction intervals for forecasts made using exponential smoothing methods, the prediction intervals require that the forecast errors are uncorrelated and are normally distributed with mean zero and constant variance.
# 
# While exponential smoothing methods do not make any assumptions about correlations between successive values of the time series, in some cases you can make a better predictive model by taking correlations in the data into account. Autoregressive Integrated Moving Average (ARIMA) models include an explicit statistical model for the irregular component of a time series, that allows for non-zero autocorrelations in the irregular component.

#While exponential smoothing models are based on a description of the trend and seasonality in the data, ARIMA models aim to describe the autocorrelations in the data.

ts %>% diff(differences=3) %>% forecast::ggtsdisplay()


plot(diff(ts, differences=3)) #seems to be good.
diffts=diff(ts, differences=3) #use KPSS Unit Root test to check if d>0 is needed - Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test


acf(diffts, lag.max = 20,na.action = na.pass) #note the x axis , fractions should be multiplied by 12. See we will get 12 x 1.5 = 18 , + 2 more = 20 lags  !


acf(diffts, lag.max = 20,na.action = na.pass, plot=FALSE) #to get the values
# 
# Autocorrelations of series ‘diffts’, by lag
# 
# 0.0000 0.0833 0.1667 0.2500 0.3333 0.4167 0.5000 0.5833 0.6667 0.7500 0.8333 0.9167 1.0000 1.0833 1.1667 
# 1.000 -0.407 -0.315  0.370 -0.164 -0.030  0.092 -0.046 -0.077  0.136 -0.075 -0.086  0.213 -0.051 -0.173 
# 1.2500 1.3333 1.4167 1.5000 1.5833 1.6667 
# 0.192 -0.018 -0.144  0.134  0.016 -0.211 


#p = 3?
#d= 3
#q= 2 ?

#https://otexts.org/fpp2/arima-r.html
forecast:::auto.arima(ts,stepwise=FALSE , approximation=FALSE,seasonal=FALSE)  
#stepwise=FALSE and approximation=FALSE to make it work harder
#seasonal=FALSE for not looking for seasonal arima models

# Series: ts 
# ARIMA(0,1,1) with drift 
# 
# Coefficients:
#   ma1   drift
# 0.7540  0.6930
# s.e.  0.0956  0.2295
# 
# sigma^2 estimated as 1.3:  log likelihood=-112.55
# AIC=231.1   AICc=231.44   BIC=237.97


#If we use the “aic” criterion, which penalises the number of parameters , but it remains the same in our case.

fit=forecast:::auto.arima(ts,ic = "aic", seasonal = FALSE)
# Series: ts 
# ARIMA(0,1,1)(0,1,1)[12] 
# 
# Coefficients:
#   ma1     sma1
# 0.6212  -0.8816
# s.e.  0.1234   0.5684
# 
# sigma^2 estimated as 1.148:  log likelihood=-97.49
# AIC=200.99   AICc=201.41   BIC=207.32

#If we combine differencing with autoregression and a moving average model, we obtain a non-seasonal ARIMA model. 
arima(ts, order=c(0,1,1))
# Call:
#   arima(x = ts, order = c(0, 1, 1))
# 
# Coefficients:
#   ma1
# 0.7925
# s.e.  0.0838
# 
# sigma^2 estimated as 1.415:  log likelihood = -116.75,  aic = 237.5

library(dplyr)
fit %>% forecast::forecast(h=10) %>% forecast::autoplot(include=80)


#-------------------SEASONAL ARIMA ---------------------

#https://otexts.org/fpp2/seasonal-arima.html
fit=forecast:::auto.arima(ts,ic = "aic")
# Series: ts 
# ARIMA(0,1,1)(0,1,1)[12] 
# 
# Coefficients:
#   ma1     sma1
# 0.6212  -0.8816
# s.e.  0.1234   0.5684
# 
# sigma^2 estimated as 1.148:  log likelihood=-97.49
# AIC=200.99   AICc=201.41   BIC=207.32
# 

ts %>%  forecast::Arima(order=c(3,3,2), seasonal=c(3,3,2)) %>%
  residuals() %>% ggtsdisplay()

forecast::Arima(ts,order=c(0,1,1),seasonal=c(0,1,1))
