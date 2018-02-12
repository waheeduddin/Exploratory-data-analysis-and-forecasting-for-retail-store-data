# Exploratory-data-analysis-and-forecasting-for-retail-store-data
The project is concerned with analysis and prediction of a time series data from retail stores. The data is about sales from the retail stores for about one year of timeperiod.
The goal was to engineer new features from a time series data which can be grouped together so that they can be used by simple graph plotting techniques to identify patterns and outliers within the data.
Also prediction was carried out using a number of algorithms. Here I report results of Random forest and ARIMA implementation only.  The predicted results are compared with test data and RSME error of ‘sales’(profit or loss made on each sale in US dollars) values is reported. But RSME is not a good measure because for a sales value, the actual values are not so important but it is interesting to see if the prediction is able to generate and follow the correct trend of sale values. So graph results are also reported for both the cases. 
Note: ARIMA analysis does not use features engineered for its analysis.
Visualizations, feature engineering and random forest predictions were made in python language while GLM and ARIMA based predictions as well as a rehearsal of visualizations and feature engineering  were made in R language. 
All code is provided with helpful comments.
# Files:
Project.R : deals with engineering features and visualization in R language. The file dumps out training and testing set data that have new features in them to be used for prediction. This code also performs Random forest based predictions.
Start.py : deals with engineering features and visualization in Pyhton language. The file again creates the training and testing sets.
Prediction.py: Performs prediction using Random forest regressor and then plots over a 1000 values of predicted sales and true sales to show a trend. Requires testingSet.csv and trainingSet.csv generated by either ‘Start.py’ or ‘Project.R’
ARIMAanalysis.R: Performs statistics analysis based on concepts of frequency domain analysis and finds out lag split (P, Q, D) values that can be used to generate future trends from the ARIMA model. This code requires the trainingSet.csv file generated earlier.
ARIMA_plotter.R: I forgot to plot trends with the ARIMA analysis so I added them later.
Numerous ‘.png’ files are there to show the working of the project.
# Error Reports:
RMSE on training set:  3.3668158565502213
RMSE on testing set:  0.006120800608978031