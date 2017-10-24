library(dplyr)
library(ggplot2)
require(gridExtra)
require(forecast)
require(tseries)
require(StatMeasures)
library('forecast')
library('tseries')
library(gridExtra)
#library(StatMeasures)

UKretailOriginal <- read.csv('sales_data.csv')
#str(UKretailOriginal)

extractWeekNumber = function (date) { 
 monthOfDate = as.character(strftime(date , format = "%m")) #Not used  
 yearOfDate = as.character(strftime(date , format = "%y"))
 hourOfInvoice = as.character(strftime(date , format = "%H"))
 dateOfInvoice = (strftime(date , format = "%D"))#,"%d/%m/%y")
 dayOfInvoice = strftime(date , format = "%d")
 myTempDate = paste("01",monthOfDate , yearOfDate , sep = "-" )
 myTempDate = strptime(myTempDate , format = "%d-%m-%y")
 yearWeekOfFirstDay = as.integer(strftime(myTempDate , format = "%W"))
 yearWeekOfDate = (as.integer(strftime(date , format = "%W")))
 weekNumberInMonth = 1 + (yearWeekOfDate - yearWeekOfFirstDay)
 return(list(weekNumberInMonth,yearOfDate,hourOfInvoice,dayOfInvoice,dateOfInvoice))
}

tempDates <- strptime(UKretailOriginal$InvoiceDate , format = "%Y-%m-%d %H:%M:%S")
dummy <- extractWeekNumber(tempDates)
UKretailOriginal$weekNumber <- unlist(dummy[1])
UKretailOriginal$year <- unlist(dummy[2])
UKretailOriginal$hours <- unlist(dummy[3])
UKretailOriginal$days <- unlist(dummy[4])
UKretailOriginal$InvoiceDateAsDates <- unlist(dummy[5])
UKretailOriginal$weekDays <- weekdays(tempDates)  #weekDays function gives name of weekdays
UKretailOriginal$months <- months(tempDates) #gives name of Months
UKretailOriginal$sales <- UKretailOriginal$Quantity * UKretailOriginal$UnitPrice #definition of sales. '-'ive sales are losses

tempFirstCharacterInvoiceNo <- sapply(as.character(UKretailOriginal$InvoiceNo), function(x) substr(x , 0 , 1))  #if the invoice numebr starts with letter 'C'
UKretailOriginal$cancelledInvoice <- sapply(tempFirstCharacterInvoiceNo , function(x) x == 'C')  #it represents a cancelled Invoice

UKretailOriginal$InvoiceDatePOSIXct <- as.POSIXct(tempDates)       #POSIXct dates are easily compareable

badObservations <- sapply(tempFirstCharacterInvoiceNo , function(x) (is.na(as.numeric(x))& (x != 'C')))  #some invoice numbers start with letter 'A'
badObservationsIndex <- which(badObservations == TRUE)
print(badObservationsIndex) #printing out the Invoice number and indecies of those observations
UKretailOriginal <- UKretailOriginal[-badObservationsIndex,]

tempNewInvoice <- as.numeric(diff(UKretailOriginal$InvoiceNo))
tempNewInvoice <- sapply(tempNewInvoice , function(x) if(x != 0) return(TRUE) else return(FALSE))
UKretailOriginal$isInvoiceNew <- c(TRUE , tempNewInvoice) #makig the first one a new invoice

UKretailOriginal <- mutate(UKretailOriginal , changeOfHour = c(0 , diff(as.integer(UKretailOriginal$hours))) , changeOfDate = c(0 , diff(as.integer(UKretailOriginal$days)))) #checking where the hours  and days of sales have changed
UKretailOriginal <- mutate(UKretailOriginal , changeOfHour = ifelse(changeOfHour == 0 , FALSE , TRUE) , changeOfDate = ifelse(changeOfDate == 0, FALSE, TRUE)) #Making the change in hour and days detection as.Logical

tempTimeDifference <- diff(UKretailOriginal$InvoiceDatePOSIXct) #difference of time between consecutive sales
tempTimeDifference <- c(0 , tempTimeDifference)

tempTimeDifference[which(UKretailOriginal$changeOfDate == TRUE)] <- 0 
UKretailOriginal$timeDiffSecs <- tempTimeDifference
UKretailOriginal$timeDiffMin <- UKretailOriginal$timeDiffSecs %/% 60
rm(tempDates, dummy,tempFirstCharacterInvoiceNo,badObservations, badObservationsIndex, tempNewInvoice, tempTimeDifference) #clearing R's RAM


UKretailLossesOnly <- subset(UKretailOriginal , cancelledInvoice == TRUE) #seperatiing losses from sales to plot them seperately
UKretailSalesOnly <- subset(UKretailOriginal , cancelledInvoice == FALSE) #getting profits/sales only

salesByCountries <- UKretailSalesOnly %>% group_by(Country) %>% summarise(productsSold = sum(Quantity), sumSales = sum(sales) , meanSales = mean(sales))  #total sales, mean sales and number of products sold in each country
salesByProducts <- UKretailSalesOnly %>% group_by(StockCode) %>% summarise(productsSold = sum(Quantity), sumSales = sum(sales) , meanSales = mean(sales)) #total sales, mean sales and number of products sold of each product

salesByHours <- UKretailSalesOnly %>% group_by(hours) %>% summarise(productsSold = sum(Quantity),sumSales = sum(sales) , meanSales = mean(sales)) #total sales, mean sales and number of products sold in each hour
noOfCustomersAtHour <- UKretailSalesOnly %>% group_by(hours) %>% summarise( number = length(CustomerID) , uniqueCustomers = length(unique(CustomerID))) #total customers, number of unique customers in every hour

salesByWeekDays <- UKretailSalesOnly %>% group_by(weekDays) %>% summarise(productsSold = sum(Quantity) , sumSales = sum(sales) , meanSales = mean(sales)) #total sales, mean sales and number of products sold in every weekday
noOfCustomersAtWeekDays <- UKretailOriginal %>% group_by(weekDays) %>% summarise(number = length(CustomerID) , uniqueCustomers = length(unique(CustomerID)) , ratioNewToRepeatCust = length(CustomerID)/length(unique(CustomerID))) #total No. of customers, number of unique customers and ratio of new to repeated customers that came on every weekday
productsByWeekDays <- UKretailOriginal %>% group_by(weekDays) %>% summarise(productsSold = sum(Quantity) , noOfProducts = length(StockCode) , noUniqueProducts = length(unique(StockCode))) #total products sold, number of unique products sold and ratio of new to repeated products sold on every weekday

salesByDays <- UKretailSalesOnly %>% group_by(days) %>% summarise(productsSold = sum(Quantity) , sumSales = sum(sales) , meanSales = mean(sales))#total sales, mean sales and number of products sold on each of the 30/31 days of a month

salesByMonthsYear <- UKretailSalesOnly %>% group_by(months, year) %>% summarise(productsSold = sum(Quantity) , sumSales = sum(sales) , meanSales = mean(sales))#total sales, mean sales and number of products sold in every month
salesByMonthsYear <- mutate(salesByMonthsYear , xAxesGraph = paste(months , year))
salesByMonthsYear <- salesByMonthsYear[order(salesByMonthsYear$sumSales),] #order the sales as per lowest to highest sales

noOfCustomersAtMonths <- UKretailOriginal %>% group_by(months ,year) %>% summarise(number = length(CustomerID) , uniqueCustomers = length(unique(CustomerID)) , ratioNewToRepeatCust = length(CustomerID)/length(unique(CustomerID)))

salesAtWeekDaysMonths <- UKretailSalesOnly %>% group_by(weekDays,months) %>% summarise(productsSold = length(Quantity) , sumSales = sum(sales) , meanSales = mean(sales))

salesAtHoursWeekDays <- UKretailSalesOnly %>% group_by(hours,weekDays) %>% summarise(productsSold = length(Quantity) , sumSales = sum(sales) , meanSales = mean(sales)) #for box plotting sales on every hour of every weekday
salesAtHoursMonths <- UKretailSalesOnly %>% group_by(hours,months) %>% summarise(productsSold = length(Quantity) , sumSales = sum(sales) , meanSales = mean(sales))
salesAtHoursDays <- UKretailSalesOnly %>% group_by(hours,days) %>% summarise(productsSold = length(Quantity) , sumSales = sum(sales) , meanSales = mean(sales))

salesAtDaysMonths <- UKretailSalesOnly %>% group_by(months,days) %>% summarise(productsSold = length(Quantity) , sumSales = sum(sales) , meanSales = mean(sales))

#to plot the sales and losees combined
negPosSales <- UKretailOriginal %>% mutate(positiveSales = ifelse(cancelledInvoice == FALSE , sales , 0) , negativeSales = ifelse(cancelledInvoice == FALSE , 0, sales))
negPosSales <- negPosSales %>% select(InvoiceDateAsDates , positiveSales , negativeSales)
negPosSalesGrouped <- negPosSales %>% group_by(InvoiceDateAsDates) %>% summarise(sumPositiveSales = sum(positiveSales), sumNegativeSales = sum(negativeSales)) #summing positive sales and negative sales on every day seperately
negPosSalesGrouped <- mutate(negPosSalesGrouped , totalSales = sumPositiveSales + sumNegativeSales) #making a total sales variable

timeLagsAtHoursWeekDays <- UKretailOriginal %>% group_by(hours,weekDays) %>% summarise(productsSold = length(Quantity) , sumSales = sum(sales) , meanSales = mean(sales) , totalTimeLag = sum(timeDiffSecs))
timeLagsMinAtHoursWeekDays <- UKretailOriginal %>% group_by(hours,weekDays) %>% summarise(productsSold = length(Quantity) , sumSales = sum(sales) , meanSales = mean(sales) , totalTimeLag = sum(timeDiffMin))

#(p <- ggplot(salesAtWeekDaysMonths, aes(months,weekDays)) + geom_tile(aes(fill = sumSales),colour = "red") + scale_fill_gradient(low = "white",high = "red") + ggtitle("box plot - sales per week at every month"))
#(p2 <- ggplot(salesAtHoursWeekDays, aes(weekDays,hours)) + geom_tile(aes(fill = sumSales),colour = "red") + scale_fill_gradient(low = "white",high = "red") + ggtitle("box plot - sales per hour on every weekday"))
#(p3 <- ggplot(salesAtHoursMonths, aes(months,hours)) + geom_tile(aes(fill = sumSales),colour = "red") + scale_fill_gradient(low = "white",high = "red") + ggtitle("box plot - sales per hour on every month"))
#(p4 <- ggplot(salesAtHoursDays, aes(days,hours)) + geom_tile(aes(fill = sumSales),colour = "red") + scale_fill_gradient(low = "white",high = "red") + ggtitle("box plot - sales per day of month by hours"))
#(p5 <- ggplot(salesAtDaysMonths, aes(months,days)) + geom_tile(aes(fill = sumSales),colour = "red") + scale_fill_gradient(low = "white",high = "red")+ ggtitle("box plot - sales per month on every day"))
##(p6 <- ggplot(timeLagsAtHoursWeekDays, aes(weekDays , hours)) + geom_tile(aes(fill = totalTimeLag),colour = "red") + scale_fill_gradient(low = "white",high = "red")+ ggtitle("box plot - delays between sales per weekday on every hour."))
#(p7 <- ggplot(timeLagsMinAtHoursWeekDays, aes(weekDays , hours)) + geom_tile(aes(fill = totalTimeLag),colour = "red") + scale_fill_gradient(low = "white",high = "red")+ ggtitle("box plot - delays between sales in Minutes on every weekday"))

#(p8 <- ggplot() + geom_point(aes(x = xAxesGraph , y = sumSales),data = salesByMonthsYear, color ="red")+ ggtitle("scatter Plot - total sales made in every month") + xlab("months") + ylab ("sales"))
#(p9 <- ggplot() + geom_point(aes(x = Country , y = sumSales),data = salesByCountries, color ="red") + ggtitle("scatter plot - total sales made in different countries")+ xlab("countries") + ylab ("sales"))
#(p10 <- ggplot() + geom_point(aes(x = StockCode , y = sumSales),data = salesByProducts, color ="red") + ggtitle("scatter plot - total sales made by selling each product")+ xlab("products") + ylab ("sales"))
#(p11 <- ggplot() + geom_point(aes(x = days , y = sumSales),data = salesByDays, color ="red") + ggtitle("scatter plot - total sales made in different days of the months")+ xlab("days") + ylab ("sales"))
#(p12 <- ggplot() + geom_point(aes(x = hours , y = sumSales),data = salesByHours, color ="red")+ ggtitle("scatter plot - total sales made in different hours of a day") + xlab("hours") + ylab ("sales"))
#(p13 <- ggplot() + geom_point(aes(x = weekDays , y = sumSales),data = salesByWeekDays, color ="red")+ ggtitle("scatter plot - total sales made in different weekdays of a week") + xlab("weekDays") + ylab ("sales"))
#(p13 <- ggplot(salesByWeekDays , aes(x = weekDays)) + geom_point(aes(y = sumSales),color ="red")+ geom_point(aes(y = productsSold),color ="green") + ggtitle("scatter plot - sales (red) and total products (green) sold on different weekdays"))

d1 <- ggplot() + geom_line(aes(x = as.Date(InvoiceDateAsDates, format = "%m/%d/%y") , y = totalSales),data = negPosSalesGrouped , color ="black") + xlab("dates of the sales") + ylab("total sales")
d2 <- ggplot() + geom_line(aes(x = as.Date(InvoiceDateAsDates, format = "%m/%d/%y") , y = sumPositiveSales),data = negPosSalesGrouped , color ="green") + xlab("dates of the sales") + ylab("sales made")
d3 <- ggplot() + geom_line(aes(x = as.Date(InvoiceDateAsDates, format = "%m/%d/%y") , y = sumNegativeSales),data = negPosSalesGrouped , color ="red") + xlab("dates of the sales") + ylab("sales lost")
#grid.arrange(d1,d2 ,d3, nrow = 3 )

cancelledOrdersByDay <- table(UKretailOriginal$weekDays,ifelse(UKretailOriginal$cancelledInvoice == FALSE, "Not cancelled", "Cancelled")) #some tables to explore how orders are cancelled
cancelledOrdersByMonth <- table(UKretailOriginal$months,ifelse(UKretailOriginal$cancelledInvoice == FALSE, "Not cancelled", "Cancelled"))
cancelledOrdersByHours <- table(UKretailOriginal$hours,ifelse(UKretailOriginal$cancelledInvoice == FALSE, "Not cancelled", "Cancelled"))

indexToSplit <- floor(0.75 * nrow(UKretailOriginal))
trainingSet <- UKretailOriginal[1:indexToSplit,]
testingSet <- UKretailOriginal[indexToSplit:nrow(UKretailOriginal),]

sales_train_ts = ts(trainingSet[, c('sales')])

sales_train_ts = tsclean(sales_train_ts) # using the clean count with no outliers
#this might take time to fit
fit<-auto.arima(sales_train_ts, seasonal=FALSE) # i could not understand how the analysis of lags help to find P,Q and D values so i am going with the automatic ARIMA model.


sales_test_ts = ts(testingSet[1:1500,c('sales')])#just taking first 1500 values to show the graph nicely
fit1 <- Arima(sales_test_ts, model=fit)

(plot(sales_test_ts , col = 'green' , xlab = 'time Series' , main = 'Predicted and true sales as time series'))
(lines(fit1$fitted , col = 'red'))
error <- sqrt(sum(abs(sales_test_ts - fit1$fitted))/length(sales_test_ts))
print("RSME on testing data: ") 
print(error)

