#RBA retail sales

library(dplyr)
library(lubridate)
library(ggplot2)
setwd("/Users/Jake/Projects/RBARetailSales/Data/")

data = read.csv("RBARetailSales.csv")

data = select(data, Date, Retail.Turnover.per.Capita, Log)
myts = select(data,Retail.Turnover.per.Capita)
myts = ts(myts, frequency = 4, start = c(1983,3))

plot.ts(myts)
rba_retail_sales_components <- decompose(myts)

plot(rba_retail_sales_components)

rba_forecasts <- HoltWinters(myts)

#Predicts 8 quarters into the future.
rba_future <- forecast.HoltWinters(rba_forecasts, h=8)
plot.forecast(rba_future)


plotForecastErrors(rba_future$residuals)