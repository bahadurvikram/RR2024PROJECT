# Main R script
## Required libraries
library(xts)
library(quantmod)
library(ggthemes)
library(dygraphs)
library(tidyverse)

## Clean any local/environment variables
rm(list = ls())

## Get this script present location
LOC_CODE = dirname(rstudioapi::getSourceEditorContext()$path)

## Set it to working direcotry
setwd(LOC_CODE)

## Read bitcoin csv daily price
quotes_bitcoin <- read_csv("../data/Bitcoin_01_01_2012-04_10_2018_historical_data_coinmarketcap.csv", 
                           col_select = c(timeOpen,close))
## We can examine structure of the resulting object:
head(quotes_bitcoin)
tail(quotes_bitcoin)
glimpse(quotes_bitcoin)

## Let’s also check the class of the Date column:
class(quotes_bitcoin$timeOpen)

## lets check structure of the whole dataset
str(quotes_bitcoin)

##Let's transform timeOpen into date:
quotes_bitcoin$timeOpen <- as.Date(quotes_bitcoin$timeOpen, format = "%Y-%m-%d")
## We have to give the format in which date is originally stored: * %y means 2-digit year, * %Y means 4-digit year * %m means a month * %d means a day
class(quotes_bitcoin$timeOpen)
head(quotes_bitcoin)
glimpse(quotes_bitcoin)
# Now R understands this column as dates


## Creating xts objects
quotes_bitcoin <- 
  xts(quotes_bitcoin[, -1], # data columns (without the first column with date)
      quotes_bitcoin$timeOpen)  # date/time index

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

