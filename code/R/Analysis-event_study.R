#===============================================================================
# Event study 
#===============================================================================
rm(list = ls())
library(tidyverse)
library(extrafont)
source("functions.R")
#font_import()
#loadfonts(device = "win")
theme_set(theme_bw(base_size = 20))

#===============================================================================
# Read in relevant data
#===============================================================================

mpu <- read_csv("../../data/tau_mpu.csv")
fomc_dates <- read_csv("../../data/fomc_announcement.csv")
fomc_dates <- as.Date(fomc_dates$date, format = "%m/%d/%Y")

n.meetings <- length(fomc_dates)

# create a vector of event dates for QE, FG, TT
event.dates <- as.Date(c("2012-01-25", "2012-09-13", "2012-12-12", "2013-06-19",
                         "2014-12-17", "2015-03-18", "2015-09-17", "2015-10-28", 
                         "2015-12-16", "2017-03-15", "2018-01-31", "2018-06-13",
                         "2018-09-26", "2019-01-30"))

event.dates <- fomc_dates

# yield data
yields <- read_csv("../../data/US_yields.csv")

# term premium data
tp <- read_csv("../../data/term_premium.csv")

# libor ois spread data
libor_ois <- read_csv("../../data/libor_ois_spread.csv")

# exchange rate
exch.rates <- read_csv("../../data/exchange_rates.csv", na = ".")
exch.rates <- na.omit(exch.rates)

# generic eurodollar yields 
ed <- read_csv("../../data/ED_generic.csv")

# daily changes in data 
mpu_diff <- calc_diff_ts(mpu)
mpu_diff_announcement <- mpu_diff[which(mpu_diff$date %in% event.dates), ]

ed_diff <- calc_diff_ts(ed)
ed_diff_announcement <- ed_diff[which(ed_diff$date %in% event.dates), ]

tp_diff <- calc_diff_ts(tp)
tp_diff_announcement <- tp_diff[which(tp_diff$date %in% event.dates), ]

yields_diff <- calc_diff_ts(yields)
yields_diff_announcement <- yields[which(yields$date %in% event.dates), ]

libor_ois_diff <- calc_diff_ts(libor_ois)
libor_ois_announcement <- libor_ois_diff[which(libor_ois_diff$date %in% event.dates), ]

exch.rate_diff <- calc_diff_ts(exch.rates, pch = T)
exch.rate_diff_announcement <- exch.rate_diff[which(exch.rate_diff$date %in% event.dates)-1, ]


#===============================================================================
# regression analysis: Y = a + b1 * D.mpu + b2 * eds + e
#===============================================================================
D.mpu <- mpu_diff_announcement$`2.5` - mpu_diff_announcement$`1`; eds <- ed_diff_announcement$`ED8 Comdty`

fity1 <- lm(yields_diff_announcement$ACMY01 ~ D.mpu + eds)
summary(fity1)

fity10 <- lm(yields_diff_announcement$ACMY10 ~ D.mpu + eds)
summary(fity10)

fity10 <- lm(exch.rate_diff_announcement$DEXCAUS ~  eds)
summary(fity10)


plot(eds, exch.rate_diff_announcement$DEXCAUS)
abline(lm(exch.rate_diff_announcement$DEXCAUS ~  eds))







# create data frame to store events in 
event_df <- data.frame(dates = event.dates)
event_df$mpu <- mpu_diff_announcement$`2.5`
event_df$tp5y <- tp_diff_announcement$ACMTP05 
event_df$tp10y <- tp_diff_announcement$ACMTP10 

