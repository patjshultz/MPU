# Script to clean Eurodollar options data from Bloomberg

# 0. Housekeeping
rm(list = ls())
library(reshape2)
library(readxl)
library(tidyverse)
library(plyr)
source("functions.R")
theme_set(theme_bw(base_size = 20))

#===============================================================================
# load/format all options data
#===============================================================================

options_list <- load_options_new()
puts_list <- options_list$puts
calls_list <- options_list$calls
underlying_df <- load_underlying()

# merge to a single dataframe in long format
puts_data <- Reduce(f = rbind, x = puts_list)
puts_data <- puts_data[order(puts_data$date),]
puts_data$strikes <- as.numeric(substring(puts_data$variable, 7))
colnames(puts_data) <- c("date", "ticker", "G","expir", "tau", "X")
puts_data$PC <- "P"

calls_data <- Reduce(f = rbind, x = calls_list)
calls_data <- calls_data[order(calls_data$date),]
calls_data$strikes <- as.numeric(substring(calls_data$variable, 7))
colnames(calls_data) <- c("date", "ticker", "G","expir", "tau", "X")
calls_data$PC <- "C"
  
options_data <- rbind(calls_data, puts_data)

# add column for the underlying ticker
year <- as.numeric(substr(options_data$ticker, start = 4, stop = 4))
ind.1 <- which(year > 3)
ind.0 <- which(year <= 3)
options_data$underlying <- NA
options_data$underlying[ind.1] <- paste(substr(options_data$ticker[ind.1], start = 1, stop = 3),
                                 "1", substr(options_data$ticker[ind.1], start = 4, stop = 4),
                                 " Comdty", sep = "")
options_data$underlying[ind.0] <- paste(substr(options_data$ticker[ind.0], start = 1, stop = 3),
                                        "2", substr(options_data$ticker[ind.0], start = 4, stop = 4),
                                        " Comdty", sep = "")
options_data$P <- NA
options_data$moneyness <- NA

# loop through each day and add the underlying prices to the options dataframe
# and determine whether or not the options are in or out of the money
dates <- unique(options_data$date) 
nloops <- length(dates)
pr <- progress_text()
pr$init(nloops)

for(day in dates){
  # extract data for each given trading day
  options_daily <- options_data[which(options_data$date == day), ]
  futures_daily <- underlying_df[which(underlying_df$date == day), ]
  futures_contracts <- as.character(unique(futures_daily$variable))
  
  # determine the underlying contract price for each option
  for (i in 1:length(futures_contracts)) {
    
    # specify the underlying futures contract and its price
    contract <- futures_contracts[i]
    contract_price <- futures_daily$value[i]
    
    # allocate the price to the necessary indices
    option_ind <- which(options_daily$underlying == contract)
    options_daily$P[option_ind] <- contract_price
    
  }
  
  options_data[which(options_data$date == day), ] <- options_daily
  pr$step()
  
}


# reformat GSW data to long
gsw <- read.csv("../../data/GSW_data.csv", stringsAsFactors = F)
gsw$date <- as.Date(gsw$date, format = "%m/%d/%Y")


# output data to csv files
write_csv(x = options_data, path = "../../data/ed_options.csv")
write_csv(x = underlying_df, path = "../../data/ed_underyling.csv")

