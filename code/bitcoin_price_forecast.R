# Main R script
## Required libraries
library(xts)
library(quantmod)
library(ggthemes)
library(dygraphs)
library(tidyverse)
library(urca)
library(tseries)
library(forecast)
library(dplyr)

## Clean any local/environment variables
rm(list = ls())

## Get this script present location
LOC_CODE = dirname(rstudioapi::getSourceEditorContext()$path)

print(LOC_CODE)
## Set it to working direcotry
setwd(LOC_CODE)

## Read bitcoin csv daily price
# quotes_bitcoin <- read_csv("../data/Bitcoin_01_01_2012-04_10_2018_historical_data_coinmarketcap.csv", 
                           # col_select = c(timeOpen,close))
quotes_bitcoin <- read_csv("../data/Bitcoindata.csv", 
                           col_select = c(Date,Close))
#colnames(quotes_bitcoin)[2] <- "Close"


#quotes_bitcoin$Close <- quotes_bitcoin$High
## We can examine structure of the resulting object:
head(quotes_bitcoin)
tail(quotes_bitcoin)
glimpse(quotes_bitcoin)

## Let’s also check the class of the Date column:
class(quotes_bitcoin$Close)

## lets check structure of the whole dataset
str(quotes_bitcoin)

##Let's transform timeOpen into date:
quotes_bitcoin$Date <- as.Date(quotes_bitcoin$Date, format = "%d/%m/%Y")
## We have to give the format in which date is originally stored: * %y means 2-digit year, * %Y means 4-digit year * %m means a month * %d means a day
class(quotes_bitcoin$Date)
head(quotes_bitcoin)
glimpse(quotes_bitcoin)
# Now R understands this column as dates


## Creating xts objects
quotes_bitcoin <- 
  xts(quotes_bitcoin[, -1], # data columns (without the first column with date)
      quotes_bitcoin$Date)  # date/time index

# Lets see the result:
head(quotes_bitcoin)
str(quotes_bitcoin)


## Finally, let’s use the ggplot2 package to produce nice visualization. 
## The ggplot2 package expects data to be in long format, rather than wide format. 
## Hence, first we have to convert the tibble to a long tibble:
## Plotting Actual Bitcoin Price
tibble(df = quotes_bitcoin) %>%
  ggplot(aes(zoo::index(quotes_bitcoin), df)) +
  geom_line() +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y")+
  labs(
    title = "Actual Bitcoin Price",
    subtitle = paste0("Number of observations: ", length(quotes_bitcoin)),
    caption = "source: RR 2024",
    x="",
    y=""
  )


## Plotting Log Transformed Bitcoin Price
tibble(df = quotes_bitcoin) %>%
  ggplot(aes(zoo::index(quotes_bitcoin), log(quotes_bitcoin))) +
  geom_line() +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y")+
  labs(
    title = "Log Transformed Bitcoin Price",
    subtitle = paste0("Number of observations: ", length(quotes_bitcoin)),
    caption = "source: RR 2024",
    x="",
    y=""
  )

## Plotting 1st Difference Log Operator
tibble(df = quotes_bitcoin) %>%
  ggplot(aes(zoo::index(quotes_bitcoin), periodReturn(quotes_bitcoin, period="daily", type="log"))) +
  geom_line() +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y")+
  labs(
    title = "1st Difference Log Operator",
    subtitle = paste0("Number of observations: ", length(quotes_bitcoin)),
    caption = "source: RR 2024",
    x="",
    y=""
  )




selectIndex500 <- (zoo::index(quotes_bitcoin) <= as.Date("2013-05-14","%Y-%m-%d"))

#original_500 <- window(ausbeer, start=1995)

original_500 <- quotes_bitcoin[selectIndex500,]
log_transformed <- log(quotes_bitcoin)
log_transformed_500 <- log_transformed[selectIndex500,]
first_diff_log_operator_500 <- periodReturn(original_500, period="daily", type="log")

adf.original_500 <- adf.test(original_500)  
pp.original_500 <- pp.test(original_500)
print(adf.original_500)
print(pp.original_500)
adf.log_transformed_500 <- adf.test(log_transformed_500)  
pp.log_transformed_500 <- pp.test(log_transformed_500)  
print(adf.log_transformed_500)
print(pp.log_transformed_500)
adf.first_diff_log_operator_500 <- adf.test(first_diff_log_operator_500)  
pp.first_diff_log_operator_500 <- pp.test(first_diff_log_operator_500)  
print(adf.first_diff_log_operator_500)
print(pp.first_diff_log_operator_500)

selectIndex2000 <- (zoo::index(quotes_bitcoin) <= as.Date("2017-06-25","%Y-%m-%d"))


original_2000 <- quotes_bitcoin[selectIndex2000,]
log_transformed_2000 <- log(original_2000)
first_diff_log_operator_2000 <- periodReturn(original_2000, period="daily", type="log")

adf.original_2000 <- adf.test(original_2000)  
pp.original_2000 <- pp.test(original_2000)
print(adf.original_2000)
print(pp.original_2000)
adf.log_transformed_2000 <- adf.test(log_transformed_2000)  
pp.log_transformed_2000 <- pp.test(log_transformed_2000)  
print(adf.log_transformed_2000)
print(pp.log_transformed_2000)
adf.first_diff_log_operator_2000 <- adf.test(first_diff_log_operator_2000)  
pp.first_diff_log_operator_2000 <- pp.test(first_diff_log_operator_2000)  
print(adf.first_diff_log_operator_2000)
print(pp.first_diff_log_operator_2000)


auto.arima(original_500) #Result is ARIMA(4,1,4) 

########
######## Table 2 details start from here
########
arima_model_500 <- Arima(log_transformed_500$Close, order=c(4,1,0))
summary(arima_model_500) # Training Sample performance, Need to put in result

arima_model_2000 <- Arima(log_transformed_2000$Close, order=c(4,1,1))
summary(arima_model_2000) # Training Sample performance, Need to put in result

# Fit NNAR model (specify orders explicitly)
nnar_model_500 <- nnetar(log_transformed_500$Close, p=2, P=1, size=2)
accuracy(nnar_model_500)

nnar_model_2000 <- nnetar(log_transformed_2000$Close, p=1, P=2, size=2)
accuracy(nnar_model_2000)

######## Table 2 details end here

# Forecast next 10 day with ARIMA for log_transformed_500
arima_forecast_500 <- forecast(arima_model_500, h = 10)
print(arima_forecast_500)
selectIndex510 <- (zoo::index(quotes_bitcoin) <= as.Date("2013-05-24","%Y-%m-%d"))
original_510 <- quotes_bitcoin[selectIndex510,]
log_transformed_510 <- log(original_510)
autoplot(arima_forecast_500) + autolayer(ts(log_transformed_510$Close), series="Actual")


# Forecast next 10 day with ARIMA for log_transformed_2000
arima_forecast_2000 <- forecast(arima_model_2000, h = 5)
print(arima_forecast_2000)
selectIndex2010 <- (zoo::index(quotes_bitcoin) <= as.Date("2017-06-30","%Y-%m-%d"))
original_2010 <- quotes_bitcoin[selectIndex2010,]
log_transformed_2010 <- log(original_2010)
autoplot(arima_forecast_2000) + autolayer(ts(log_transformed_2010$Close), series="Actual")

#train_forecasts <- forecast(nnar_model_500, h=length(log_transformed_500))

#accuracy(train_forecasts, log_transformed_500$Close)


########
######## Table 3 details start from here
########
#arima_model_500test <- Arima(log_transformed_500$Close, order=c(0,1,0))

#original_1966 <- window(quotes_bitcoin, start = "2013-05-15")
#log_transformed_1966 <- log(original_1966)
#arima_forecast_1966 <- forecast(arima_model_500test, h = 1966)
#accuracy(arima_forecast_1966, log_transformed_1966)
#autoplot(arima_forecast_1966) + autolayer(ts(log_transformed_2010$Close), series="Actual")


hh <- 1966

# Create a vector to store the forecasts
forecasts_without_reestimation_arima <- numeric(2466)
forecasts_with_reestimation_arima <- numeric(2466)
forecasts_without_reestimation_nnar <- numeric(2466)
forecasts_with_reestimation_nnar <- numeric(2466)
for(k in 1:500) {
  forecasts_without_reestimation_arima[k] <- as.double(log_transformed_500$Close[k])
  forecasts_with_reestimation_arima[k] <- as.double(log_transformed_500$Close[k])
  forecasts_without_reestimation_nnar[k] <- as.double(log_transformed_500$Close[k])
  forecasts_with_reestimation_nnar[k] <- as.double(log_transformed_500$Close[k])
}

re_estimated_arima <- log_transformed_500
re_estimated_nnar <- log_transformed_500
# Forecasting step-by-step without re-estimation
for (i in 1:hh) {
  print(i)
  # Forecast the next period using the already fitted model
  if (i==1) {
    arima_model_500_without <- Arima(log_transformed_500$Close, order=c(4,1,0))
    arima_model_500_with <- auto.arima(log_transformed_500$Close)
    nnar_model_500_without <- nnetar(log_transformed_500$Close, p=2, P=1, size=2)
    nnar_model_500_with <- nnetar(log_transformed_500$Close)
  } else {
    arima_model_500_without <- Arima(ts(rbind(log_transformed_500,log(quotes_bitcoin[500:500+(i-1)]))), order = c(4,1,0))
    arima_model_500_with <- auto.arima(rbind(log_transformed_500,log(quotes_bitcoin[500:500+(i-1)]))$Close)
    nnar_model_500_without <- nnetar(ts(rbind(log_transformed_500,log(quotes_bitcoin[500:(500+(i-1))]))), p=2, P=1, size=2)
    nnar_model_500_with <- nnetar(ts(rbind(log_transformed_500,log(quotes_bitcoin[500:(500+(i-1))]))))
    
  }
  
  temp_result <- forecast(arima_model_500_without, h=1)
  forecasts_without_reestimation_arima[500+i] <- temp_result$mean[1]
  temp_result <- forecast(arima_model_500_with, h=1)
  forecasts_with_reestimation_arima[500+i] <- temp_result$mean[1]
  re_estimated_arima <- rbind(re_estimated_arima, xts(temp_result$mean[1], last(index(re_estimated_arima))+1))
  temp_result <- forecast(nnar_model_500_without, h=1)
  forecasts_without_reestimation_nnar[500+i] <- temp_result$mean[1]
  temp_result <- forecast(nnar_model_500_with, h=1)
  forecasts_with_reestimation_nnar[500+i] <- temp_result$mean[1]
  re_estimated_nnar <- rbind(re_estimated_nnar, xts(temp_result$mean[1], last(index(re_estimated_nnar))+1))
  
}


accuracy(forecasts_without_reestimation_arima, log(quotes_bitcoin))
accuracy(forecasts_with_reestimation_arima, log(quotes_bitcoin))

accuracy(forecasts_without_reestimation_nnar, log(quotes_bitcoin))
accuracy(forecasts_with_reestimation_nnar, log(quotes_bitcoin))

autoplot(ts(forecasts_without_reestimation_arima)) + autolayer(ts(log(quotes_bitcoin)), series="Actual")
autoplot(ts(forecasts_with_reestimation_arima)) + autolayer(ts(log(quotes_bitcoin)), series="Actual")

autoplot(ts(forecasts_without_reestimation_nnar)) + autolayer(ts(log(quotes_bitcoin)), series="Actual")
autoplot(ts(forecasts_with_reestimation_nnar), series="NNAR Re-estimated") + autolayer(ts(log(quotes_bitcoin)), series="Actual")

autoplot(xts(forecasts_with_reestimation_nnar, index(quotes_bitcoin)), series="NNAR Re-estimated") + autolayer(ts(log(quotes_bitcoin)), series="Actual")
+ scale_x_date(date_labels = "%Y", date_breaks = "1 year")

autoplot(xts(forecasts_with_reestimation_nnar, index(quotes_bitcoin))) + scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 year")

autoplot(xts(forecasts_with_reestimation_nnar, index(quotes_bitcoin))) + 
  autolayer(log(quotes_bitcoin), PI=FALSE) + 
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 year")
df_1966_days_forecasted <- data.frame(date=index(quotes_bitcoin), 
                                      arima_without_reestimation = forecasts_without_reestimation_arima, 
                                      arima_with_reestimation = forecasts_with_reestimation_arima,
                                      nnar_without_reestimation = forecasts_without_reestimation_nnar,
                                      nnar_with_reestimation = forecasts_with_reestimation_nnar,
                                      close=log(quotes_bitcoin$Close))
ggplot(data = df_1966_days_forecasted, aes(x = date), type='h') +
         geom_line(aes(y = Close), color = "darkred") + 
         geom_line(aes(y = arima_without_reestimation), color = "blue", linetype="twodash") +
        geom_line(aes(y = arima_with_reestimation), color = "red", linetype="twodash") +
        geom_line(aes(y = nnar_without_reestimation), color = "green", linetype="twodash") +
        geom_line(aes(y = nnar_with_reestimation), color = "yellow", linetype="twodash") +
         scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 year")






######## Table 3 details end here

modelarima410 = arima(first_diff_log_operator_500, order = c(4,0,0))

et410 = residuals(modelarima410)
acf(et410)
plot.ts(et410)
Box.test(et410, lag = 10, type = c("Box-Pierce", "Ljung-Box"), fitdf = 4)
gghistogram(et410)

#install.packages("forecast")
options(scipen=999)


forcast410 = forecast(modelarima410, h=5)
print(forcast410)

modelarima410 = arima(first_diff_log_operator_500[1:490], order = c(4,0,0))
forcast410 = forecast(modelarima410, h=10)
accuracy(modelarima410)
