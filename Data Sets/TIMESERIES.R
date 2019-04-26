##Time Series
##read the data
stock<-read.csv("D:/Rlang/stock_pred.csv")
View(stock)
##Mean absolute per" error is a measure of pred" accuracy of a forecasting method i.e trend estimation by this we can calculate the accurate value of MAPE
mape = function(act,pred) {
  res = mean(abs((act-pred)/act))
  return(res)
}
##convert data into time series object
stock_ts <-ts(stock[,1],start = 2008,freq=12)
head(stock_ts)
stock_ts
####plot time series
plot(stock_ts)
##various components of time series
stock_1 = stl(stock_ts,s.window = "periodic")
stock_1
plot(stock_1)
##predicting Alpha,Beta,Gamma values
stock_hws <-HoltWinters(stock_ts)
stock_hws
##calculation for value prediction

pred = as.numeric(stock_hws$fitted[,1])
act = as.numeric(stock_ts[(1:96)])
##calculating the in-sample error
mape(act,pred)
##now lets forecast for next 12 months
pred= predict(stock_hws,n.ahead=12,prediction.interval= T,level=0.95)
pred
##plotting forecast values
##black are actual values and red are smoothing lines which follows the black
plot(stock_hws,pred)

##check for the assumption
err = act-as.numeric(pred[,1])
err
##error should follow normal distribution if error are normally distributed then our model is fine
par(mfrow=c(1,2))
hist(err)
qqnorm(err)
##
par(mfrow(1,1))
acf(err,lag.max = 30)
##acf is a matrix by with we can check automatic correlation 
##HO: independence in a given time series
Box.test(err,lag = 30,type = "Ljung-Box")

## there seems to be some spikes going out of the thresholds.lets do a ljung Box test if these spikes are statistically significant here the P-value is greater than 0.05
