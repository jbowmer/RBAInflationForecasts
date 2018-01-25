#RBA retail sales

library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(zoo)
setwd("/Users/Jake/Projects/RBARetailSales/Data/")

data = read.csv("RBARetailSales.csv")
#rename headings
names(data) = c("Per_Capita_Current", "Per_Capita_Current_2", "Per_Capita_Current_3", "Per_Capita_Current_Chain_1", "Per_Capita_Current_Chain_2", "Per_Capita_Current_Chain_3")


data = select(data, Date, Per_Capita_Current)
myts = select(data,Per_Capita_Current)
myts = ts(myts, frequency = 4, start = c(1983,3))

plot.ts(myts)
rba_retail_sales_components <- decompose(myts)

plot(rba_retail_sales_components)

rba_forecasts <- HoltWinters(myts)

#Predicts 8 quarters into the future.
rba_future <- forecast.HoltWinters(rba_forecasts, h=8)
plot.forecast(rba_future)


#Plot Individual Components of Retail Sales:
industry_data = read.csv("SalesByIndustry.csv", stringsAsFactors = FALSE)

industry_data = industry_data[-c(1,2,3,4,5,6,7,8,9,10), c(1,2,3,4,5,6,7,8)]
names(industry_data) = c("Date", "Food_Retailing", "Household_Goods", "Clothing_Footwear", "Department_Stores", "Other", "Cafes_Restaurants",
                         "Total")
industry_data$Date = paste0("01-",industry_data$Date)
industry_data$Date = dmy(industry_data$Date)  


#Change all columns to numeric
industry_data[-1] <- lapply(industry_data[-1], as.numeric)



# labels and breaks for X axis text
#brks <- industry_data$Date[seq(1, length(industry_data$Date), 12)]
#lbls <- lubridate::year(brks)
#Plotting first:
abs_retail_sales = ggplot(industry_data, aes(x=Date)) + 
  geom_line(aes(y=Food_Retailing, col="Food_Retailing")) + 
  geom_line(aes(y=Household_Goods, col = "Household_Goods")) +
  geom_line(aes(y=Clothing_Footwear, col = "Clothing_Footwear")) +
  geom_line(aes(y=Department_Stores, col = "Department_Stores")) +
  geom_line(aes(y=Other, col = "Other")) + 
  geom_line(aes(y=Cafes_Restaurants, col = "Cafes_Restaurants")) +
  labs(title="ABS Retail Sales", 
       caption="Source: ABS, jakebowmer.com", y="Dollar, Millions") +
  scale_color_manual(name="", 
                     values = c("Food_Retailing"="red", "Household_Goods"="blue", "Clothing_Footwear" = "green",
                                "Department_Stores" = "yellow", "Other" = "grey", "Cafes_Restaurants" = "black")) 
  
  
#Now a chart for total as a time series.
  
myts = select(industry_data,Total)
myts = ts(myts, frequency = 12, start = c(1982,5))

autoplot(myts)
abs_retail_sales_components <- decompose(myts)

autoplot(rba_retail_sales_components)

abs_forecasts <- HoltWinters(myts)

#Predicts 8 quarters into the future.
abs_future <- forecast.HoltWinters(abs_forecasts, h=8)
autoplot(abs_future)

#Trend for department stores
department_store_ts = select(industry_data,Department_Stores)
department_store_ts = ts(department_store_ts, frequency = 12, start = c(1982,5))

autoplot(department_store_ts)
department_store_components <- decompose(department_store_ts)

autoplot(department_store_components)

autoplot(department_store_components$trend) +
  labs(title="ABS Department Store Trend", 
       caption="Source: ABS, jakebowmer.com", y = "Trend Sales")

abs_forecasts <- HoltWinters(myts)

#Predicts 8 quarters into the future.
abs_future <- forecast.HoltWinters(abs_forecasts, h=8)
autoplot(abs_future)