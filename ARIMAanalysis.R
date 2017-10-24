library('ggplot2')
library('forecast')
library('tseries')
trainingSet = read.csv('trainingSet.csv')
sales_ts = ts(trainingSet[, c('sales')])

trainingSet$salesTS = tsclean(sales_ts) # using the clean count with no outliers

trainingSet$sales_ma = ma(trainingSet$salesTS, order=1) #calculating daily average
trainingSet$sales_ma7 = ma(trainingSet$salesTS, order=7)#calculating weekly average


d1 <- ggplot() +
  geom_line(data = trainingSet[500:1000,], aes(x = dateTime, y = salesTS, colour = "Counts")) +
  geom_line(data = trainingSet[500:1000,], aes(x = dateTime, y = sales,   colour = "Daily Moving Average"))  +
  geom_line(data = trainingSet[500:1000,], aes(x = dateTime, y = sales_ma7, colour = "Weekly Moving Average"))  +
  ylab('sales made')

sales_ma = ts(na.omit(trainingSet$sales_ma), frequency=7) # same as sampling
decomp = stl(sales_ma, s.window="periodic")  #filter structure for smoothing
deseasonal_cnt <- seasadj(decomp) #the 'seasonal' element is removed. normally this patter occurs at quarters but we do not have enough quarters in our data, so I am interested in patterns that live short term

(adf.test(sales_ma, alternative = "stationary"))

#_______output
#Augmented Dickey-Fuller Test

#data:  sales_ma
#Dickey-Fuller = -44.807, Lag order = 56, p-value = 0.01
#alternative hypothesis: stationary

#now we will subtract the lagged series and take the test again
sales_d1 = diff(deseasonal_cnt, differences = 1) #lagged series by one step

(adf.test(sales_d1, alternative = "stationary"))

#_______output
  Augmented Dickey-Fuller Test

#data:  sales_d1
#Dickey-Fuller = -84.758, Lag order = 56, p-value = 0.01
#alternative hypothesis: stationary

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE) # i could not understand how the analysis of lags help to find P,Q and D values so i am going with the automatic ARIMA model.

#______
#ARIMA(2,0,3) with non-zero mean 

#Coefficients:
#         ar1      ar2      ma1     ma2     ma3     mean
#      1.6177  -0.6342  -1.2034  0.2490  0.0394  13.2180
#s.e.  0.0309   0.0288   0.0311  0.0158  0.0058   0.1065

#sigma^2 estimated as 78.87:  log likelihood=-659724.5
#AIC=1319463   AICc=1319463   BIC=1319534
#my model has the order (2,0,3) .... the next value in series is ar1 (1.6177) dampened version of the first
##############sa
testingSet = read.csv('testingSet.csv')
#get index to represent 24 hours
sales_test_ts = ts(testingSet[,c('sales')])
fit1 <- Arima(sales_test_ts, model=fit)

(plot(sales_test_ts , col = 'green' , xlab = 'time Series' , main = 'Predicted and true sales as time series'))
(lines(fit1$fitted , col = 'red'))
error <- sum(sales_test_ts - fit1$fitted)
print(error)