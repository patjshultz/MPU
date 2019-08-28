#===============================================================================
# VAR analysis
#===============================================================================

rm(list = ls())
library(tidyverse)
library(vars)
library(readxl)
library(lubridate)

mpu <- read_csv("../../data/tau_mpu.csv")
mpu <- tibble(date = mpu$date, mpu = mpu$`2.5`) 
macro <- read_excel("../../data/macro_data.xls")
macro$date <- as.Date(macro$date)
yields <- read_excel("../../data/yields_1yr_10yr.xls")
yields$date <- as.Date(yields$date)
spread <- data.frame(date = yields$date, S = yields$Spread)
fomc_dates <- read_csv("../../data/fomc_announcement.csv")
fomc_dates <- as.Date(fomc_dates$date, format = "%m/%d/%Y")

#===============================================================================
# Sum MPU monthly and assign a value of zero when there is no FOMC meeting
#===============================================================================

mpu_fomc <- mpu[which(mpu$date %in% fomc_dates), ]
day <- as.numeric(format(mpu_fomc$date, "%d"))
mpu_fomc$date <- as.Date(mpu_fomc$date - (day - 1))


macro$mpu <- 0
macro$mpu[which(macro$date %in% mpu_fomc$date)] <- mpu_fomc$mpu

macro <- macro[-which(macro$date < mpu$date[1]), ]
macro <- merge(x = macro, y = spread, by= "date")

X <- data.frame(IP = log(macro$INDPRO), 
                PPI = log(macro$PPI),
                MPU = macro$mpu,
                FFR = macro$FEDFUNDS,
                S = macro$S)
X <- na.omit(X)
fit.ols <- VAR(X, p = 2, type = 'const')
plot(irf(fit.ols, n.ahead = 36))

