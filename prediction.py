#created by MUHAMMAD WAHEED UD DIN SIDDIQUI
#purpose HelloFresh intern test

#start this file after running start.py
from sklearn.ensemble import RandomForestRegressor
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def randomForest(features_train, labels_train , features_test , labels_test): #this function is very similar to the one found on my github.
    myRFC = RandomForestRegressor() #regressor not classifier
    myRFC.fit(features_train, labels_train)   #my intuition was I was use this function to make three models to predict the targets as well
    pred = myRFC.predict(features_train)     #as predict depednats fo those predictors to be use the regressor as a forecaster... predicitng intermediate dependencies before predicting the main target
    trainingScore = np.sqrt (sum(abs(labels_train - pred))/len(labels_train)) #RSME score
    pred = myRFC.predict(features_test)
    testScore = np.sqrt(sum(abs(labels_test - pred)))/len(labels_train)   #RSME score
    return myRFC , trainingScore , testScore  #return the model

def myPlotter(_pred , _target , _startPoint = 1 , _endPoint = 500):  #A standard one graph two lines plot. how many values to be plotted from the predicted and target variables
    xAxisValues = range(_startPoint,_endPoint,1)
    plt.figure()
    plt.plot(xAxisValues, _pred.ravel()[xAxisValues], color='red')
    plt.plot(xAxisValues, _target.ravel()[xAxisValues], color='green')
    plt.legend(['predictions', 'trueValues'], loc='upper left')
    plt.ylabel('continous values')
    plt.xlabel('index of observation')
    plt.show()

UKretailTraining = pd.read_csv('trainingSet.csv')
UKretailTesting = pd.read_csv('testingSet.csv')

features_sales = ['sales_p','hour_c_d_10','hour_c_d_11','hour_c_d_12','isCountryUK_c_d_False','isCountryUK_c_d_True',
            'hour_c_d_13','hour_c_d_14','hour_c_d_15','hour_c_d_16','hour_c_d_17','hour_c_d_18','hour_c_d_19','hour_c_d_20','hour_c_d_6',
            'hour_c_d_7','hour_c_d_8','hour_c_d_9','minute_p','timeDiffMin_p','timeDiffSec_p',
            'cOrder_p_c_d_False','cOrder_p_c_d_True','posSales_p','losses_p']   #NO FEATURE RANKING.... Just used these values based one EDA e.g. hours have a pattern.
# More reasoning in README_MUHAMMAD file
labels_sales = ['sales']   #not using normalized values as label..... Most of the mare too small because the data has HIGH variance. RF can handle this because it works on GINI index calculation

sales_train_features = UKretailTraining[features_sales]
sales_train_labels = UKretailTraining[labels_sales]

sales_test_features = UKretailTesting[features_sales]
sales_test_labels = UKretailTesting[labels_sales]


salesModel , trainingScore , accuracyScore = randomForest(sales_train_features.values ,sales_train_labels.values.ravel(),sales_test_features.values,sales_test_labels.values.ravel())

print("RMSE on training set: " , trainingScore)
print("RMSE on testing set: " , accuracyScore)

predictions = salesModel.predict(sales_test_features)
myPlotter(predictions,sales_test_labels.values.ravel(),47589,52471)#random potting to see how the values match