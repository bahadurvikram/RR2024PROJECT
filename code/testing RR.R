# Load necessary libraries
library(readr)
library(dplyr)
library(forecast)
library(tseries)
library(ggplot2)

# Load the data
file_path <- "path_to_your_csv_file/Bitcoin_01_01_2012-04_10_2018_historical_data_coinmarketcap (1).csv"
bitcoin_data <- read_csv("C:/Users/User/Downloads/Bitcoin_01_01_2012-04_10_2018_historical_data_coinmarketcap (2).csv")

# Display the structure of the data
str(bitcoin_data)

# Convert Date column to Date type and sort the data by Date
bitcoin_data$Date <- as.Date(bitcoin_data$timeClose, format="%b %d, %Y")
bitcoin_data <- bitcoin_data %>% arrange(Date)

# Select relevant columns (Date and Closing Price)
bitcoin_data <- bitcoin_data %>% select(Date, close)

# Log-transform the closing price
bitcoin_data$log_Close <- log(bitcoin_data$close)

# Differencing the log-transformed series
#bitcoin_data$diff_log_Close <- diff(bitcoin_data$log_Close, differences = 1)

# Differencing the log-transformed series
bitcoin_data$diff_log_Close <- c(NA, diff(bitcoin_data$log_Close, differences = 1))

# Perform Augmented Dickey-Fuller Test
adf_test <- adf.test(na.omit(bitcoin_data$diff_log_Close), alternative = "stationary")
print(adf_test)

# Perform Phillips-Perron Test
pp_test <- pp.test(na.omit(bitcoin_data$diff_log_Close), alternative = "stationary")
print(pp_test)

# Perform Augmented Dickey-Fuller Test for close prices
adf_test <- adf.test(na.omit(bitcoin_data$close), alternative = "stationary")
print(adf_test)

# Differencing the close series
bitcoin_data$diff_close <- c(NA, diff(bitcoin_data$close, differences = 1))

adf_test <- adf.test(na.omit(bitcoin_data$diff_close), alternative = "stationary")
print(adf_test)

# Focus on the first 500 days
bitcoin_data_500 <- bitcoin_data[1:500, ]

# Log-transform the closing price
bitcoin_data_500$log_Close <- log(bitcoin_data_500$close)

# Differencing the log-transformed series
bitcoin_data_500$diff_log_Close <- c(NA, diff(bitcoin_data_500$log_Close, differences = 1))

# Remove NA values
bitcoin_data_500 <- bitcoin_data_500 %>% filter(!is.na(diff_log_Close))

# Perform Augmented Dickey-Fuller Test on first difference
adf_test_500 <- adf.test(bitcoin_data_500$diff_log_Close, alternative = "stationary")
print(adf_test_500)

# Perform Phillips-Perron Test on first difference
pp_test_500 <- pp.test(bitcoin_data_500$diff_log_Close, alternative = "stationary")
print(pp_test_500)

# Split the data into training and test sets (for the first 500 days, we use the entire data as training)
train_data_500 <- bitcoin_data_500

# Focus on the first 500 days
bitcoin_data_500 <- bitcoin_data[1:500, ]

# Log-transform the closing price
bitcoin_data_500$log_Close <- log(bitcoin_data_500$Close)

# Differencing the log-transformed series
bitcoin_data_500$diff_log_Close <- c(NA, diff(bitcoin_data_500$log_Close, differences = 1))

# Remove NA values
bitcoin_data_500 <- bitcoin_data_500 %>% filter(!is.na(diff_log_Close))

# Perform Augmented Dickey-Fuller Test on first difference
adf_test_500 <- adf.test(bitcoin_data_500$diff_log_Close, alternative = "stationary")
print(adf_test_500)

# Perform Phillips-Perron Test on first difference
pp_test_500 <- pp.test(bitcoin_data_500$diff_log_Close, alternative = "stationary")
print(pp_test_500)

# Split the data into training and test sets (for the first 500 days, we use the entire data as training)
train_data_500 <- bitcoin_data_500

# # Focus on the first 500 days
bitcoin_data_500 <- bitcoin_data[1:500, ]

# Log-transform the closing price
bitcoin_data_500$log_Close <- log(bitcoin_data_500$Close)

# Differencing the log-transformed series
bitcoin_data_500$diff_log_Close <- c(NA, diff(bitcoin_data_500$log_Close, differences = 1))

# Remove NA values
bitcoin_data_500 <- bitcoin_data_500 %>% filter(!is.na(diff_log_Close))

# Perform Augmented Dickey-Fuller Test on first difference
adf_test_500 <- adf.test(bitcoin_data_500$diff_log_Close, alternative = "stationary")
print(adf_test_500)

# Perform Phillips-Perron Test on first difference
pp_test_500 <- pp.test(bitcoin_data_500$diff_log_Close, alternative = "stationary")
print(pp_test_500)

# Split the data into training and test sets (for the first 500 days, we use the entire data as training)
train_data_500 <- bitcoin_data_500

# Focus on the first 500 days
bitcoin_data_500 <- bitcoin_data[1:500, ]

# Log-transform the closing price
bitcoin_data_500$log_Close <- log(bitcoin_data_500$Close)

# Differencing the log-transformed series
bitcoin_data_500$diff_log_Close <- c(NA, diff(bitcoin_data_500$log_Close, differences = 1))

# Remove NA values
bitcoin_data_500 <- bitcoin_data_500 %>% filter(!is.na(diff_log_Close))

# Perform Augmented Dickey-Fuller Test on first difference
adf_test_500 <- adf.test(bitcoin_data_500$diff_log_Close, alternative = "stationary")
print(adf_test_500)

# Perform Phillips-Perron Test on first difference
pp_test_500 <- pp.test(bitcoin_data_500$diff_log_Close, alternative = "stationary")
print(pp_test_500)

# Split the data into training and test sets (for the first 500 days, we use the entire data as training)
train_data_500 <- bitcoin_data_500

# Fit ARIMA(4,1,0) model
arima_model_500 <- Arima(train_data_500$log_Close, order=c(4,1,0))
summary(arima_model_500)

# Fit NNAR model (specify orders explicitly)
nnar_model_500 <- nnetar(train_data_500$log_Close, p=2, P=1, size=2)
summary(nnar_model_500)

# Forecast next 1 day with ARIMA
arima_forecast_500 <- forecast(arima_model_500, h = 1)
autoplot(arima_forecast_500) + autolayer(train_data_500$log_Close, series="Actual")

# Forecast next 1 day with NNAR
nnar_forecast_500 <- forecast(nnar_model_500, h = 1)
autoplot(nnar_forecast_500) + autolayer(train_data_500$log_Close, series="Actual")

# Print forecasted values
print(arima_forecast_500)
print(nnar_forecast_500)

# Fit NNAR model
nnar_model_500 <- nnetar(train_data_500$log_Close)
summary(nnar_model_500)

# Forecast next 1 day with ARIMA
arima_forecast_500 <- forecast(arima_model_500, h = 1)
autoplot(arima_forecast_500) + autolayer(train_data_500$log_Close, series="Actual")

# Forecast next 1 day with NNAR
nnar_forecast_500 <- forecast(nnar_model_500, h = 1)
autoplot(nnar_forecast_500) + autolayer(train_data_500$log_Close, series="Actual")

# Print forecasted values
print(arima_forecast_500)
print(nnar_forecast_500)

# Fit NNAR model
nnar_model_500 <- nnetar(train_data_500$log_Close)
summary(nnar_model_500)

# Forecast next 1 day with ARIMA
arima_forecast_500 <- forecast(arima_model_500, h = 1)
autoplot(arima_forecast_500) + autolayer(train_data_500$log_Close, series="Actual")

# Forecast next 1 day with NNAR
nnar_forecast_500 <- forecast(nnar_model_500, h = 1)
autoplot(nnar_forecast_500) + autolayer(train_data_500$log_Close, series="Actual")

# Print forecasted values
print(arima_forecast_500)
print(nnar_forecast_500)

# Fit NNAR model
nnar_model_500 <- nnetar(train_data_500$log_Close)
summary(nnar_model_500)

# Forecast next 1 day with ARIMA
arima_forecast_500 <- forecast(arima_model_500, h = 1)
autoplot(arima_forecast_500) + autolayer(train_data_500$log_Close, series="Actual")

# Forecast next 1 day with NNAR
nnar_forecast_500 <- forecast(nnar_model_500, h = 1)
autoplot(nnar_forecast_500) + autolayer(train_data_500$log_Close, series="Actual")

# Print forecasted values
print(arima_forecast_500)
print(nnar_forecast_500)
