#Install Required Packages in R:
install.packages("forecast")
install.packages("Metrics")
library(ggplot2)
library(tseries)
library(forecast)
# WRite down the RMSE function:
rmse=function(error)
{ 
  sqrt(mean(error^2))
}

#Storing the Training and Testing data into 'R' variable:
y_train = Datathon_Trainset
sales = y_train$Sales_MM
y_test = Datathon_Test_2
y = y_test$Sales_MM

# Plotting the Training Dataset to undertand the Auto-Regressive patterns:
plot(sales)

# Polttng ACF and PACF, to understand seasonality and identify AR() and MA() parameters:
acf(sales)
pacf(sales)

# Fitting the ARIMA model with seasonality:
fit_s = Arima((sales),order=c(24,1,1),seasonal=c(1,1,4,12),model=fit2,include.mean=T,include.drift=T)

#Computing tarining error (RMSE):
accuracy(fit_s)

# Forecasting datapoints in time, 'N+1, using data from time 'N', 'N-1',....'t=0' $ Computing RMSE value:
fut2 = forecast(fit_s,h=24) # Forecasting over the next 24 months/time-steps OR 2 yrs.
diff1 = (y)-fut2[["mean"]]
rmse(diff1)
#Result: RMSE value --> 621.608

#COMMENT: Although this ARIMA model does not include any specific features from real-world datasets into the model,
#it is a very efficient way of forecasting time-series datasets with a relatively large historical data. The ARIMA 
#model is powerful in capturing the overall trend (exponential moving average) as well as identifying seasonality in 
#the response variable, if at all. It is specifically useful to utilize a dynamic regresive model, such as ARIMA, if 
#the respoinse variable needs to be forecasted for a near future, with relatively small time-horizon in the future. 
#For the task of forecasting for the next 24 months monsth an ARIMA model is worth exploring and implementing, to 
#capture the increase in the moving average and seasonal fluctuations. 
#PLEASE REFER TO THE GRAPH PROVIDED IN THE ZIP FILE FOR MORE DETAILS.

#plotting past and forecasted expenditure on Housing Improvements:
plot(fut2)
