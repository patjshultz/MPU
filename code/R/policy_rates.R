rm(list = ls())
library(tidyverse)
library(lubridate)
source("functions.R")

fedrate <- read_csv("../../data/policy_rates/FEDFUNDS.csv")
bojrate <- read_csv("../../data/policy_rates/boj_call_rate.csv")
ecbrate <- read_csv("../../data/policy_rates/ecb_policy_rate.csv")

# aggregate ecb rate to monthlyl frequerncy
ecbrate$month <- month(ecbrate$date)
ecbrate$year <- format(ecbrate$date, format = "%Y")
ecbrate <- aggregate(ecb_rate ~ month + year, ecbrate, mean)
ecbrate$date <- as.Date(paste(ecbrate$year, "-", ecbrate$month, "-", "01", sep = ""))

# create new ecb rate dataframe
ecbrate <- data.frame(date = ecbrate$date, ecb_rate = ecbrate$ecb_rate)

# merge policy rates
policylist <- list()
policylist$fed <- fedrate; policylist$boj <- bojrate; policylist$ecb <- ecbrate
rates <- Reduce(list_merge, policylist)
rates <- na.omit(rates)

# find rolling correlation
roll.cor <- function(df, n = 12) {
  nobs <- nrow(df)
  data <- df[, -1]
  cormat <- matrix(data = NA, nrow = (nobs-12), ncol = ncol(data))
  for(i in n:nobs) {
    X <- cor(data[(i-(n-1)): i, ])
    cormat[(i-11), ] <- X[lower.tri(X)]
    print(i)
  }
}