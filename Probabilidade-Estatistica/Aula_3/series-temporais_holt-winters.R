time<-scan()
41297.00
41240.00
41118.00
40924.00
41220.00
41485.00
41662.00
41620.00
41862.00
41959.00
42150.00
42341.00
42892.00
42580.00
43137.00
42788.00
42013.00
42600.00
43355.00
43943.00
44554.00
44676.00
45263.00
45896.00
44495.00
45094.00
45172.00

demand<-ts(time,start=c(2016,7), frequency = 12)
plot(demand)
hw <- HoltWinters(demand)
plot(hw)
forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)
forecast