https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/

RawData = scan()
0.557
0.458
0.730
0.943
0.812
0.906
1.269
0.902
0.815
0.976
0.793
0.980
0.808
0.855
0.918
0.809
0.691
0.779
0.623
0.601
0.811
0.515
0.612
0.396
0.231
0.421
0.626
0.230
0.396
0.732
0.307
0.804
0.653
0.393
0.825
0.512
1.067
0.721
0.814
0.914
0.772
0.852
0.844
0.721
0.877
0.793
0.785
1.016
0.718
0.845
0.867
1.046
0.860
1.157
0.725
0.799
1.160
0.912
1.115
1.383
0.946
1.260
1.129
0.938
1.122
1.083
1.128
1.134
0.944
0.803
1.224
1.190
1.086
1.023
0.630
0.685
0.862
1.355
0.841
0.814
0.421
0.380
0.658
0.766
0.545
0.769
0.451
-0.212
0.303
0.789
0.262

tsData = ts(RawData, start = c(2011,2), frequency = 12)

plot(tsData)
components.ts = decompose(tsData)
plot(components.ts)

library("fUnitRoots")
urkpssTest(tsData, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(tsData, differences=1)
plot(tsstationary)


acf(tsData,lag.max=34)

timeseriesseasonallyadjusted <- tsData - timeseriescomponents$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)

acf(tsstationary, lag.max=34)
pacf(tsstationary, lag.max=34)

fitARIMA <- arima(tsData, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
library(lmtest)
coeftest(fitARIMA)


confint(fitARIMA)


acf(fitARIMA$residuals)
library(FitAR)
boxresult<-LjungBoxTest (fitARIMA$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)


library(forecast)
auto.arima(tsData, trace=TRUE)

fitARIMA <- arima(tsData, order=c(1,0,1),seasonal = list(order = c(0,0,1), period = 12),method="ML")

predict(fitARIMA,n.ahead = 12)

futurVal <- forecast(fitARIMA,h=12, level=0.95)
plot(futurVal)