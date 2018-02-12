#created by MUHAMMAD WAHEED UD DIN SIDDIQUI
#purpose HelloFresh intern test

#start this file first
import pandas as pd  #for dataframe operations
import datetime #for time differencing
import numpy as np
import sys #to print my python vesion
import seaborn as sns #for heatmaps
import matplotlib.pyplot as plt #regular plotting
import time #to record time of my program
from sklearn import preprocessing #preprocessing is quite efficeint in sklearn
import numbers #to check between various data types

start_time = time.time()
print("Pandas version: ")
print(pd.__version__)

print("Python version")
print(sys.version)

################################################HELPER FUNCTIONS###########################################################
def extractDateTime(dateTimeSeries ,suffixName = ''):   #some off the shelf dateTime processing
    tempDateTime = pd.to_datetime(dateTimeSeries, format="%Y-%m-%d %H:%M:%S")
    tempDateTime_df = pd.DataFrame({"year" + suffixName: tempDateTime.dt.year,
                                    "month"+ suffixName: tempDateTime.dt.month,
                                    "dayOfMonth"+ suffixName: tempDateTime.dt.day,
                                    "hour"+ suffixName: tempDateTime.dt.hour,
                                    "minute"+ suffixName:tempDateTime.dt.minute,
                                    "seconds"+ suffixName:tempDateTime.dt.second,
                                    "dayofyear"+ suffixName: tempDateTime.dt.dayofyear,
                                    "week"+ suffixName: tempDateTime.dt.week,
                                    "weekofyear"+ suffixName: tempDateTime.dt.weekofyear,
                                    "weekday"+ suffixName: tempDateTime.dt.weekday_name,
                                    "quarter"+ suffixName: tempDateTime.dt.quarter,
                                    "dateTime":tempDateTime
                                    })
    tempDateTime_df['month'] = tempDateTime_df['month'].apply(lambda x: datetime.datetime.strptime(str(x), '%m').strftime('%b')) #to get month name. usedfor plots
    return tempDateTime_df

def findTimeDifferenceSecs(dataRow): #to be used in a lambda expression. converts time differences into seconds
    timeDifference = dataRow['dateTime'] - dataRow["dateTime_p"]
    seconds = timeDifference / np.timedelta64(1, 's') #standard from the documentation here https://pandas.pydata.org/pandas-docs/stable/timedeltas.html
    return seconds

def findTimeDifferenceMin(dataRow): #to be used in a lambda expression. converts time differences into min
    timeDifference = dataRow['dateTime'] - dataRow["dateTime_p"] # pandas._libs.tslib.Timestamp'
    seconds = timeDifference / np.timedelta64(1, 's')
    return seconds//60

def history(tempDataFrame):   #introduces a lag in the dataFrame and returns only the newly created df
    history_df = tempDataFrame.shift(1)
    columnNames = tempDataFrame.columns.tolist()
    newColumnNames = []
    for name in columnNames:
        newColumnNames.append(name + '_p')  #unique style of naming a lagged feature
    history_df.columns = newColumnNames
    return(history_df)

def getXticks(tempSeries):          #to set the xticks of the multiple plots that will be draw in EDA part
    myxLabels = tempSeries.index.tolist()
    myxLabels.insert(0, ' ')   #basically you need an empty space to give spaings at the ends of your graph i.e. insert ...
    myxLabels.append(' ')      #and append
    return myxLabels

def myIntToString(_x):           #hours are returned as integers from function 'extractDateTime'. I tweaked them to strings so that R would....
    if (isinstance(_x, numbers.Number)):   #understand them as factors
        convertedValue = 'd_' + str(_x)
    else:
        convertedValue = 'NAN'
        print("NANs introduced while converting to string")
    return convertedValue

def seriesPlots(tempSeries  ,_title = "", _rot = 15):   #one function to plot a series
    plt.figure()
    axis = plt.gca()
    ax1 = tempSeries.plot(legend=True, ax=axis, marker='o', rot=_rot, figsize=(20, 12), title=_title)
    if(isinstance(tempSeries.index.tolist()[0],numbers.Number)): #the value of xlim are tricky to handle....
        minPoint = int(min(tempSeries.index.tolist())) - 1  #when they are int or float, we want the limits to be the min...
        maxPoint = int(max(tempSeries.index.tolist())) + 1  #max
    else:
        minPoint = -1                             #else an index like array from -1 to lenght of unique values works fine!!!
        maxPoint = len(tempSeries.index.tolist())
    ax1.set_xticks(range(minPoint, maxPoint + 1, 1))
    ax1.set_xticklabels(getXticks(tempSeries))
    plt.show()


########################################FUNCTIONAL CODE##################################################################
UKretailOriginal = pd.read_csv('UKretail.csv',encoding='iso-8859-1')

#print(UKretailOriginal.info())

withNArows = UKretailOriginal.shape[0]
UKretailOriginal = UKretailOriginal.dropna(how='any')  #because it is a series analysis, remving values later changes the perspective
droppedRows = withNArows - UKretailOriginal.shape[0]   #I dropped them at the start
print("Number of Rows with NA dropped at the beginning" , droppedRows)

myDateTime = UKretailOriginal['InvoiceDate']
print('FEATURE ENGINEERING:')
print('\textracting DateTime')
tempDateTime_df = extractDateTime(UKretailOriginal['InvoiceDate'])
ownFeatures = pd.DataFrame()
print('FEATURE ENGINEERING:')
print('\tcreating sales feature')
ownFeatures['sales'] = UKretailOriginal['Quantity'] *  UKretailOriginal['UnitPrice']
ownFeatures['cOrder'] = ownFeatures['sales'].apply(lambda x: (True if x < 0 else False))   #when sales is -ive, we have a 'c'-cancelled order
print('\tSeprerating sales and losses')
ownFeatures['posSales'] = ownFeatures.apply((lambda x: 0 if x['cOrder']    else x['sales']) , axis = 1) #to be used for plotting the saless ....
ownFeatures['losses'] = ownFeatures.apply((lambda x: -1*x['sales'] if x['cOrder'] else 0) , axis = 1)  #and losses of whole data and see outliers
print('\tworking on country feauture. Check if country is UK')
ownFeatures['isCountryUK'] = UKretailOriginal['Country'].apply(lambda x: False if x != 'United Kingdom' else True) #there are 37 or so countires but UK ...
                                                                                                                   #is the most common. I seperated that.

UKretail_beforeHistory = pd.concat([UKretailOriginal ,tempDateTime_df, ownFeatures], axis=1)  #the features all collected and now lag will be introduced
print('\tcreating history of data')
UKretailHistory = history(UKretail_beforeHistory)

#ownFeatures2['timeDiffSec'] = UKretailHistory.apply(lambda x: findTimeDifferenceSecs(x) , axis = 1)
#ownFeatures2['timeDiffMin'] = UKretailHistory.apply(lambda x: findTimeDifferenceMin(x) , axis = 1)

UKretailFinal = pd.concat([UKretail_beforeHistory , UKretailHistory], axis=1) #once I have my lagged values, I can create difference
print('\tcreating time differnece between transactions') #in time just by comparing columns of the same row. time differences is one of the values to be predicted. IMPORTANT
UKretailFinal['timeDiffSec'] = UKretailFinal.apply(lambda x: findTimeDifferenceSecs(x) , axis = 1) #row-wise operation and COSTLY
UKretailFinal['timeDiffMin'] = UKretailFinal.apply(lambda x: findTimeDifferenceMin(x) , axis = 1)
print('\trecording time differences in history')
UKretailFinal['timeDiffSec_p'] = UKretailFinal['timeDiffSec'].shift(1) #important to introduce a lag in the new features
UKretailFinal['timeDiffMin_p'] = UKretailFinal['timeDiffMin'].shift(1)
print('\tdropping NA rows introduced by feature engineering and the NA (first row) due to lagged features')
withNArows = UKretailFinal.shape[0]
UKretailFinal = UKretailFinal.dropna(how='any')
droppedRows = withNArows - UKretailFinal.shape[0]
print('number of rows dropped: ' , droppedRows)

print('\tconverting categorical features to proper factors') #factors should be factor. only string or object are factors in python. I needed
UKretailFinal['hour_c'] = UKretailFinal['hour'].apply(lambda x: myIntToString(x))   #to convert them say 06 hour to 'd_6' - d for dummy
UKretailFinal['hour_p_c'] = UKretailFinal['hour_p'].apply(lambda x: myIntToString(x)) #do the same to the lag

UKretailFinal['isCountryUK_c'] = UKretailFinal['isCountryUK'].apply(lambda x: myIntToString(x)) #for the boolean caegorical variable now
UKretailFinal['isCountryUK_p_c'] = UKretailFinal['isCountryUK_p'].apply(lambda x: myIntToString(x))

UKretailFinal['cOrder_p_c'] = UKretailFinal['cOrder_p'].apply(lambda x: myIntToString(x))  #one last time for the boolean

print('EXPLORATORY DATA ANALYSIS:')

#the names of the variables should be self explaining below
averageSalesweekDay = UKretailFinal.groupby('weekday')["sales"].mean()
totalSalesweekDay = UKretailFinal.groupby('weekday')["sales"].sum()

averageSalesMonth = UKretailFinal.groupby(['month','year'])["sales"].mean() #december occurs twice in the data... hence groupped by the year
totalSalesMonth = UKretailFinal.groupby(['month','year'])["sales"].sum()
#####first the histograms from categorical features
salesByCountry = UKretailFinal.groupby('Country')['sales'].count()   #UK stands out and then i decided to make a boolean feature 'isCountryUK'
salesByCountry.plot(kind = 'bar' , title ='sales in every country')

totalCustomersByWeekDay = UKretailFinal.groupby('weekday')["CustomerID"].count()
totalCustomersByWeekDay.plot(kind = "bar" ,title = "no of customers per weekday",rot = 15)

totalCustomersByMonth = UKretailFinal.groupby(['month','year'])["CustomerID"].count()
totalCustomersByMonth.plot(kind = "bar",title = 'no of customers in a month' ,rot = 15)
####the series from continous univariate aggregates
averageSalesHour = UKretailFinal.groupby('hour')["sales"].mean()
totalSalesHour = UKretailFinal.groupby('hour')["sales"].sum()

seriesPlots(totalSalesHour , "total sales by hour")
seriesPlots(totalSalesweekDay, "total sales by weekday")
seriesPlots(totalSalesMonth , "total sales by Month")
#_________boxplots

averageSalesweekDayHour = UKretailFinal.groupby(['hour', 'weekday'])["sales"].mean()
#INTERESTING ANOMALY HERE.... Apparently at the 06th hour of one thursday a lot of things were returned. normally no business happens
#  at that time. I had to handle this one instance to make a pretty graph. But i did not change the actual value. Just for the graph
averageSalesweekDayHour = averageSalesweekDayHour.reset_index()
anomalyIndex = averageSalesweekDayHour.index[(averageSalesweekDayHour['hour'] == 6) & (averageSalesweekDayHour['weekday'] == 'Thursday')].tolist()
averageSalesweekDayHour.ix[anomalyIndex,'sales'] = 0.0  #change in only the aggregated value
averageSalesweekDayHour = averageSalesweekDayHour.pivot('hour','weekday' , 'sales') #pandas.DataFrame.pivot(index=None, columns=None, values=None)
plt.figure()  #pivot to make sns heatmap
plt.xlabel('week day')
plt.ylabel('hour of the day')
plt.title('average sales on every hour of a week') #totals could be plot much the same. looking at averages tells you if the data is stationary i.e the mean and variance is not changing with time
sns.heatmap(averageSalesweekDayHour, annot=False)
plt.show()

#this part is tricky.... plotting all values busts my computer. I have a screen shot to prove it works but I am not testing it.
overAllPosSales = UKretailFinal.groupby('InvoiceDate')["posSales"].sum()
overAllLosses = UKretailFinal.groupby('InvoiceDate')["losses"].sum()

###############seriesPlots(overAllPosSales,"positive sales of the whole data")
#########seriesPlots(overAllLosses,_title="negative sales of the whole data", _rot = 0)

totalSalesHourByDay = UKretailFinal.groupby(['hour', 'dayOfMonth'])["sales"].sum()
totalSalesHourByDay = totalSalesHourByDay.reset_index()
totalSalesHourByDay = totalSalesHourByDay.pivot('hour','dayOfMonth' , 'sales') #pandas.DataFrame.pivot(index=None, columns=None, values=None)
plt.figure()
plt.xlabel('week day')
plt.ylabel('hour of the day')
plt.title('total sales on every hour of a 30-31 days of a month')
sns.heatmap(totalSalesHourByDay, annot=False)
plt.show()

totalSalesweekDayHour = UKretailFinal.groupby(['hour' ,'weekday'])["sales"].sum()
totalSalesweekDayHour = totalSalesweekDayHour.reset_index()
totalSalesweekDayHour = totalSalesweekDayHour.pivot('hour','weekday' , 'sales') #pandas.DataFrame.pivot(index=None, columns=None, values=None)
plt.figure()
plt.xlabel('week day')
plt.ylabel('hour of the day')
plt.title('total sales on every hour of a week')
sns.heatmap(totalSalesweekDayHour, annot=False)
plt.show()

print('PRE PROCESSING FEATURES USING SKLEARN')
#____________preprocesing

featuresForPreprocessing = ['sales','Quantity_p','UnitPrice_p','isCountryUK_c','isCountryUK_p_c','sales_p','minute','hour_c',
            'seconds','weekday','minute_p','seconds_p','weekday_p','timeDiffMin_p','timeDiffSec_p','cOrder_p_c' , 'posSales_p' , 'losses_p'] # I seleted them on intuition. NO FEATURE RANKING has been deon by me.
#normally I perform feature ranking at this part. see my github https://github.com/waheeduddin/qualityUsuabilityProject/tree/master/
labels = ['sales', 'timeDiffSec' , 'timeDiffMin']  #would it be sales or time series for prediction

#following part is just preprocessing done in a loop
usableFeatures = []  # a matrix that will help me writing all the required column names automatically
correlationFeatures = []  # to select only numerical features for correlation
for name in featuresForPreprocessing:
    if(isinstance(UKretailFinal[name].iloc[0], numbers.Number)): #if it is a int or float or even a bool.... that is why I convverted my bools to string earlier
        tempName = name + "_n"
        dummy = preprocessing.normalize(UKretailFinal[name].values.reshape(UKretailFinal[name].values.shape[0], -1)) #0 mean and 1 variance
        UKretailFinal[tempName] = dummy.reshape(UKretailFinal[name].values.shape)
        #UKretailFinal[tempName] = UKretailFinal[name]
        usableFeatures.append(tempName)
        if name not in labels:
            correlationFeatures.append(tempName) #this feature can be used for correlation analysis
    else:
        usableFeatures.append(name)
        temprorayDf = pd.get_dummies(UKretailFinal[name],prefix=name) #using dummy with a relevant prefix
        print(temprorayDf.info()) #basically I am making vecotrs for my categorical data. I wanted to use them in NN or SVM in R later
        UKretailFinal = pd.concat([UKretailFinal, temprorayDf], axis=1) #sklean onehotEncoder did not work for me...go pandas!!!
        columnNames = list(temprorayDf.columns.values)
        for temporayName in temprorayDf:
            usableFeatures.append(temporayName)

print("SPLITTING THE DATA")
splitIndex = int(np.floor(0.75 * UKretailFinal.shape[0]))
trainingSet = UKretailFinal[:splitIndex]
testingSet = UKretailFinal[splitIndex:]

#corr_df = UKretailFinal[correlationFeatures] I left it undone... I could not get the labels right. the correlation values are not high. I am not
#using current unit price or quantity to predict sales.

#this file is written to help me write column names in R later
with open("featureNames.txt", "w") as output:
    for name in usableFeatures:
        output.write("'" + str(name) + "',")
    output.write('\n')
    for name in usableFeatures:
        output.write("or$" + str(name) + ",") #'or' is just the name of reader in R
    output.write('\n')
    for name in usableFeatures:
        output.write("+" + str(name) + " ")
    output.write('\n')

# #writing to data to files
trainingSet.to_csv('trainingSet.csv') #will be decoded to UTF-8 my python 3 case
testingSet.to_csv('testingSet.csv')

print('finish!!!')
print("--- %s seconds ---" % (time.time() - start_time))
# UKretailFinal.to_csv('result.csv')
#main
#extract date
#history
#EDA
#preProcessing
#feature ranking and correlation check
#split
#exporting to csv