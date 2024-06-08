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
source("config.R")
source("utils.R")
source("model_executor.R")

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

selectIndex500 <- (zoo::index(quotes_bitcoin) <= as.Date(FIRST_TRAINING_END_DATE,YYY_MM_DD))

#original_500 <- window(ausbeer, start=1995)

original_500 <- quotes_bitcoin[selectIndex500,]
log_transformed <- log(quotes_bitcoin)
log_transformed_500 <- log_transformed[selectIndex500,]
first_diff_log_operator_500 <- periodReturn(original_500, period="daily", type="log")

########
#Table 1. Stationary test of data start here
########

adf.original_500 <- adf.test(original_500)
pp.original_500 <- pp.test(original_500)

results_500 <- data.frame()
results_500 <- stationary_test_data(results_500, "Original data", "01/01/2012~14/05/2013", adf.original_500, pp.original_500)

adf.log_transformed_500 <- adf.test(log_transformed_500)  
pp.log_transformed_500 <- pp.test(log_transformed_500)  

results_500 <- stationary_test_data(results_500, "Log transformed data", "01/01/2012~14/05/2013", adf.log_transformed_500, pp.log_transformed_500)

adf.first_diff_log_operator_500 <- adf.test(first_diff_log_operator_500)  
pp.first_diff_log_operator_500 <- pp.test(first_diff_log_operator_500)  

results_500 <- stationary_test_data(results_500, "1st difference log operator", "01/01/2012~14/05/2013", adf.first_diff_log_operator_500, pp.first_diff_log_operator_500)

print(results_500)

selectIndex2000 <- (zoo::index(quotes_bitcoin) <= as.Date(SECOND_TRAINING_END_DATE,YYY_MM_DD))

original_2000 <- quotes_bitcoin[selectIndex2000,]
log_transformed_2000 <- log(original_2000)
first_diff_log_operator_2000 <- periodReturn(original_2000, period="daily", type="log")

adf.original_2000 <- adf.test(original_2000)
pp.original_2000 <- pp.test(original_2000)
results_2000 <- data.frame()
results_2000 <- stationary_test_data(results_2000, "Original data", "01/01/2012~25/06/2017", adf.original_2000, pp.original_2000)

adf.log_transformed_2000 <- adf.test(log_transformed_2000)
pp.log_transformed_2000 <- pp.test(log_transformed_2000)
results_2000 <- stationary_test_data(results_2000, "Log transformed data", "01/01/2012~25/06/2017", adf.log_transformed_2000, pp.log_transformed_2000)


adf.first_diff_log_operator_2000 <- adf.test(first_diff_log_operator_2000)
pp.first_diff_log_operator_2000 <- pp.test(first_diff_log_operator_2000)
results_2000 <- stationary_test_data(results_2000, "1st difference log operator", "01/01/2012~25/06/2017", adf.first_diff_log_operator_2000, pp.first_diff_log_operator_2000)

print(results_2000)

#Table 1. Stationary test of data end here

# auto.arima(original_500) #Result is ARIMA(4,1,4) 

########
######## Table 2 details start from here
########
arima_model_500 <- Arima(log_transformed_500$Close, order=ARIMA_MODEL_410)
performance_500 <- data.frame()
performance_500 <- performance_data(performance_500,"ARIMA (4,1,0)", "01/01/2012~14/05/2013", accuracy(arima_model_500))

arima_model_2000 <- Arima(log_transformed_2000$Close, order=ARIMA_MODEL_411)
performance_2000 <- data.frame()
performance_2000 <- performance_data(performance_2000,"ARIMA (4,1,1)", "01/01/2012~25/06/2017", accuracy(arima_model_2000))

# Fit NNAR model (specify orders explicitly)
nnar_model_500 <- nnetar(log_transformed_500$Close, p=NNAR_21[1], P=NNAR_21[2], size=2)
performance_500 <- performance_data(performance_500,"NNAR (2,1)", "01/01/2012~14/05/2013", accuracy(nnar_model_500))
print(performance_500)

nnar_model_2000 <- nnetar(log_transformed_2000$Close, p=NNAR_12[1], P=NNAR_12[2], size=2)
performance_2000 <- performance_data(performance_2000,"NNAR (1,2)", "01/01/2012~25/06/2017", accuracy(nnar_model_2000))
print(performance_2000)
######## Table 2 details end here

######## Table 3 details start here
df_1966_days_forecasted <- model_executor(quotes_bitcoin, log_transformed_500, ARIMA_MODEL_410, NNAR_21, TRUE)

df_466_days_forecasted <- model_executor(quotes_bitcoin, log_transformed_2000, ARIMA_MODEL_411, NNAR_12, TRUE)

ggplot(data = df_1966_days_forecasted, aes(x = date), type='h') +
         geom_line(mapping = (aes(y = Close, color = "close"))) + 
         geom_line(mapping = (aes(y = arima_without_reestimation, color = "arima_without_reestimation"))) +
        geom_line(mapping = (aes(y = arima_with_reestimation, color = "arima_with_reestimation"))) +
        geom_line(mapping = (aes(y = nnar_without_reestimation, color = "nnar_without_reestimation"))) +
        geom_line(mapping = (aes(y = nnar_with_reestimation, color = "nnar_with_reestimation"))) +
        geom_vline(xintercept = as.Date(FIRST_TESTING_START_DATE), color = "gray", size=1, linetype="dashed") + 
        scale_x_date(date_labels = YYY_MM_DD, date_breaks = "1 year") +
        labs(title="Plot of different models, where training size is 500 days and test window is 1966 days\nAll are in log normal",
        x ="", y = "")

ggplot(data = df_1966_days_forecasted, aes(x = date), type='h') +
  geom_line(mapping = (aes(y = Close, color = "close"))) + 
  geom_line(mapping = (aes(y = arima_without_reestimation, color = "arima_without_reestimation"))) +
  geom_line(mapping = (aes(y = arima_with_reestimation, color = "arima_with_reestimation"))) +
  geom_line(mapping = (aes(y = nnar_without_reestimation, color = "nnar_without_reestimation"))) +
  geom_line(mapping = (aes(y = nnar_with_reestimation, color = "nnar_with_reestimation"))) +
  geom_vline(xintercept = as.Date(FIRST_TESTING_START_DATE), color = "gray", size=1, linetype="dashed") + 
  scale_x_date(date_labels = YYY_MM_DD, date_breaks = "1 year") +
  coord_cartesian(xlim=c(as.Date(FIRST_TESTING_START_DATE), as.Date(DATA_SET_END_DATE))) +
  labs(title="Plot of different models, where training size is 500 days and test window is 1966 days\nAll are in log normal (Zoomed Version)",
       x ="", y = "")

performance_1996_without <- data.frame()
performance_1996_without <- performance_data(performance_1996_without,"ARIMA (4,1,0)", "15/05/2013~04/10/2018", accuracy(df_1966_days_forecasted$arima_without_reestimation, log(quotes_bitcoin)))
performance_1996_without <- performance_data(performance_1996_without,"NNAR (2,1)", "15/05/2013~04/10/2018", accuracy(df_1966_days_forecasted$nnar_without_reestimation, log(quotes_bitcoin)))
print(performance_1996_without)




performance_1996_with <- data.frame()
performance_1996_with <- performance_data(performance_1996_with,"ARIMA", "15/05/2013~04/10/2018", accuracy(df_1966_days_forecasted$arima_with_reestimation, log(quotes_bitcoin)))
performance_1996_with <- performance_data(performance_1996_with,"NNAR", "15/05/2013~04/10/2018", accuracy(df_1966_days_forecasted$nnar_with_reestimation, log(quotes_bitcoin)))
print(performance_1996_with)


ggplot(data = df_466_days_forecasted, aes(x = date), type='h') +
  geom_line(mapping = (aes(y = Close, color = "close"))) + 
  geom_line(mapping = (aes(y = arima_without_reestimation, color = "arima_without_reestimation"))) +
  geom_line(mapping = (aes(y = arima_with_reestimation, color = "arima_with_reestimation"))) +
  geom_line(mapping = (aes(y = nnar_without_reestimation, color = "nnar_without_reestimation"))) +
  geom_line(mapping = (aes(y = nnar_with_reestimation, color = "nnar_with_reestimation"))) +
  geom_vline(xintercept = as.Date(SECOND_TESTING_START_DATE), color = "gray", size=1, linetype="dashed") +
  scale_x_date(date_labels = YYY_MM_DD, date_breaks = "1 year") +
  labs(title="Plot of different models, where training size is 2000 days and test window is 466 days\nAll are in log normal",
       x ="", y = "")

ggplot(data = df_466_days_forecasted, aes(x = date), type='h') +
  geom_line(mapping = (aes(y = Close, color = "close"))) + 
  geom_line(mapping = (aes(y = arima_without_reestimation, color = "arima_without_reestimation"))) +
  geom_line(mapping = (aes(y = arima_with_reestimation, color = "arima_with_reestimation"))) +
  geom_line(mapping = (aes(y = nnar_without_reestimation, color = "nnar_without_reestimation"))) +
  geom_line(mapping = (aes(y = nnar_with_reestimation, color = "nnar_with_reestimation"))) +
  geom_vline(xintercept = as.Date(SECOND_TESTING_START_DATE), color = "gray", size=1, linetype="dashed") +
  scale_x_date(date_labels = YYY_MM_DD, date_breaks = "3 months") +
  coord_cartesian(xlim=c(as.Date(SECOND_TESTING_START_DATE), as.Date(DATA_SET_END_DATE))) +
  labs(title="Plot of different models, where training size is 2000 days and test window is 466 days\nAll are in log normal (Zoomed Version)",
       x ="", y = "")


performance_466_without <- data.frame()
performance_466_without <- performance_data(performance_466_without,"ARIMA (4,1,1)", "26/06/2017~04/10/2018", accuracy(df_466_days_forecasted$arima_without_reestimation, log(quotes_bitcoin)))
performance_466_without <- performance_data(performance_466_without,"NNAR (1,2)", "26/06/2017~04/10/2018", accuracy(df_466_days_forecasted$nnar_without_reestimation, log(quotes_bitcoin)))
print(performance_466_without)

performance_466_with <- data.frame()
performance_466_with <- performance_data(performance_466_with,"ARIMA (4,1,1)", "26/06/2017~04/10/2018", accuracy(df_466_days_forecasted$arima_with_reestimation, log(quotes_bitcoin)))
performance_466_with <- performance_data(performance_466_with,"NNAR (1,2)", "26/06/2017~04/10/2018", accuracy(df_466_days_forecasted$nnar_with_reestimation, log(quotes_bitcoin)))
print(performance_466_with)


######## Table 3 details end here

######## Table 4 details start from here
#First test-sample window (1966 days)
perf_compared_1966 <- data.frame()
dm_result_arima_nnar_reestimated_1966 <- dm.test(df_1966_days_forecasted$arima_with_reestimation, df_1966_days_forecasted$nnar_with_reestimation, alternative = "two.sided", h = 1, power = 2)
perf_compared_1966 <- performance_compared_data(perf_compared_1966, "ARIMA vs. NNAR (re-estimation)", dm_result_arima_nnar_reestimated_1966)

dm_result_arima_nnar_without_reestimated_1966 <- dm.test(df_1966_days_forecasted$arima_without_reestimation, df_1966_days_forecasted$nnar_without_reestimation, alternative = "two.sided", h = 1, power = 2)
perf_compared_1966 <- performance_compared_data(perf_compared_1966, "ARIMA vs. NNAR (without re-estimation)", dm_result_arima_nnar_without_reestimated_1966)

dm_result_arima_with_without_reestimated_1966 <- dm.test(df_1966_days_forecasted$arima_with_reestimation, df_1966_days_forecasted$arima_without_reestimation, alternative = "two.sided", h = 1, power = 2)
perf_compared_1966 <- performance_compared_data(perf_compared_1966, "ARIMA (re-estimation) vs. ARIMA (without re-estimation)", dm_result_arima_with_without_reestimated_1966)

dm_result_nnar_with_without_reestimated_1966 <- dm.test(df_1966_days_forecasted$nnar_with_reestimation, df_1966_days_forecasted$nnar_without_reestimation, alternative = "two.sided", h = 1, power = 2)
perf_compared_1966 <- performance_compared_data(perf_compared_1966, "NNAR (re-estimation) vs. NNAR (without re-estimation)", dm_result_nnar_with_without_reestimated_1966)
print(perf_compared_1966)

#Second test-sample window (466 days)
perf_compared_466 <- data.frame()
dm_result_arima_nnar_reestimated_466 <- dm.test(df_466_days_forecasted$arima_with_reestimation, df_466_days_forecasted$nnar_with_reestimation, alternative = "two.sided", h = 1, power = 2)
perf_compared_466 <- performance_compared_data(perf_compared_466, "ARIMA vs. NNAR (re-estimation)", dm_result_arima_nnar_reestimated_466)

dm_result_arima_nnar_without_reestimated_466 <- dm.test(df_466_days_forecasted$arima_without_reestimation, df_466_days_forecasted$nnar_without_reestimation, alternative = "two.sided", h = 1, power = 2)
perf_compared_466 <- performance_compared_data(perf_compared_466, "ARIMA vs. NNAR (without re-estimation)", dm_result_arima_nnar_without_reestimated_466)

dm_result_arima_with_without_reestimated_466 <- dm.test(df_466_days_forecasted$arima_with_reestimation, df_466_days_forecasted$arima_without_reestimation, alternative = "two.sided", h = 1, power = 2)
perf_compared_466 <- performance_compared_data(perf_compared_466, "ARIMA (re-estimation) vs. ARIMA (without re-estimation)", dm_result_arima_with_without_reestimated_466)

dm_result_nnar_with_without_reestimated_466 <- dm.test(df_466_days_forecasted$nnar_with_reestimation, df_466_days_forecasted$nnar_without_reestimation, alternative = "two.sided", h = 1, power = 2)
perf_compared_466 <- performance_compared_data(perf_compared_466, "NNAR (re-estimation) vs. NNAR (without re-estimation)", dm_result_nnar_with_without_reestimated_466)
print(perf_compared_466)
######## Table 4 details end here

options(scipen=999)
#Ljung-Box testing for used ARIMA models
modelarima410 = arima(log_transformed_500, order = ARIMA_MODEL_410)
pacf(first_diff_log_operator_500)
et410 = residuals(modelarima410)
acf(et410)
plot.ts(et410)
Box.test(et410, lag = 8, type = c("Box-Pierce", "Ljung-Box"), fitdf = 4)
gghistogram(et410)

modelarima411 = arima(log_transformed_2000, order = ARIMA_MODEL_411)
pacf(first_diff_log_operator_2000)
et411 = residuals(modelarima411)
acf(et411)
plot.ts(et411)
Box.test(et411, lag = 8, type = c("Box-Pierce", "Ljung-Box"), fitdf = 5)
gghistogram(et411)

#Proposed improved solution for ARIMA models
modelarima611 = arima(log_transformed_500, order = c(6,1,1))
pacf(first_diff_log_operator_500)
et611 = residuals(modelarima611)
acf(et611)
plot.ts(et611)
Box.test(et611, lag = 10, type = c("Box-Pierce", "Ljung-Box"), fitdf = 7)
gghistogram(et611)
accuracy(modelarima611)

modelarima510 = arima(log_transformed_2000, order = c(5,1,0))
pacf(first_diff_log_operator_2000)
et510 = residuals(modelarima510)
acf(et510)
plot.ts(et510)
Box.test(et510, lag = 7, type = c("Box-Pierce", "Ljung-Box"), fitdf = 5)
gghistogram(et510)
accuracy(modelarima510)

