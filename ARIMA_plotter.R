library('ggplot2')
library('forecast')
library('tseries')
trainingSet = read.csv('trainingSet.csv')
sales_train_ts = ts(trainingSet[, c('sales')])

sales_train_ts = tsclean(sales_train_ts) # using the clean count with no outliers
#this might take time to fit
fit<-auto.arima(sales_train_ts, seasonal=FALSE) # i could not understand how the analysis of lags help to find P,Q and D values so i am going with the automatic ARIMA model.

testingSet = read.csv('testingSet.csv')

sales_test_ts = ts(testingSet[1:1500,c('sales')])#just taking first 1500 values to show the graph nicely
fit1 <- Arima(sales_test_ts, model=fit)

(plot(sales_test_ts , col = 'green' , xlab = 'time Series' , main = 'Predicted and true sales as time series'))
(lines(fit1$fitted , col = 'red'))
error <- sqrt(sum(abs(sales_test_ts - fit1$fitted))/length(sales_test_ts))
print("RSME on testing data: ") 
print(error)

#str(fit)
#summary of the model (P,Q,D) = (2,3,0)
