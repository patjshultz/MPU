#===============================================================================
# Calculate "crash probability" of eurodollar futures. THis corresponds to the
# probability of a large interest rate hike
#===============================================================================

rm(list = ls())
library(tidyverse)
library(reshape2)
library(splines)
library(ISLR)
library(plyr)
source("functions.R")
library(KernSmooth)
library(extrafont)
loadfonts()
theme_set(theme_bw(base_size = 20))

#===============================================================================
# load options data with implied volatilities calculated
# also load GSW parameters to get risk free bond prices at any maturity
#===============================================================================

options_data <- read_csv("../../data/ED_options_IV.csv")
options_data <- options_data[which(options_data$tau > 0.4), ]

# set up global variables
options_dates <- unique(options_data$date)
all.contracts <- unique(options_data$underlying)
total.n.contracts <- length(all.contracts)

# load parameters to calculate zero coupon bond prices at any maturity
gsw_params <- read_csv("../../data/gsw_parameters.csv")

# find shared dates between gsw data and options data
gsw_dates <- gsw_params$date
dates <- as.Date(intersect(x = options_dates, y = gsw_dates), origin = "1970-01-01")
nobs <- length(dates)

#===============================================================================
# for each observation map the discrete IVs into a continuous function
# using a cubic spline.
#===============================================================================


# allocate empty matrices for crash probabilities and for taus
crash.probs <-
  matrix(data = NA,
         nrow = nobs,
         ncol = (total.n.contracts + 1))

lower.bound.probs <-
  matrix(data = NA,
         nrow = nobs,
         ncol = (total.n.contracts + 1))

tau.data <-
  matrix(data = NA,
         nrow = nobs,
         ncol = (total.n.contracts + 1))

diff_mat <- matrix(data = NA, nrow= nobs, ncol = (total.n.contracts + 1))
# label columns
colnames(crash.probs) <- c("date", all.contracts)
colnames(tau.data) <- c("date", all.contracts)
colnames(lower.bound.probs) <- c("date", all.contracts)

# convert to dataframes and allocate date column
crash.probs <- as.data.frame(crash.probs)
crash.probs$date <- dates

tau.data <- as.data.frame(tau.data)
tau.data$date <- dates

lower.bound.probs <- as.data.frame(lower.bound.probs)
lower.bound.probs$date <- dates

rate.change <- 0.01

# test for specific contract
# options_data <- options_data[which(options_data$underlying == "EDU15 Comdty"), ]
# dates <- unique(options_data$date)
# date <- dates[400]

# set up progress bar
nloops <- length(dates)
pr <- progress_text()
pr$init(nloops)


for (date in dates) {
  index <- which(dates == date)
  
  # select all calls for a given day
  daily_data <- options_data[which(options_data$date == date),]
  contracts <- unique(daily_data$underlying)
  ncontracts <- length(contracts)
  
  # select yield curve parameters
  params <- gsw_params[which(gsw_params$date == date),]
  
  for (con in contracts) {
    #------------------------------
    # select specific contract data
    #------------------------------
    
    # for crash probability we need puts
    df.P <-
      daily_data[which(daily_data$underlying == con &
                         daily_data$PC == "P"), ]
    
    # for rate cuts/ZLB indicator we need calls
    df.C <-
      daily_data[which(daily_data$underlying == con &
                         daily_data$PC == "C"), ]
    
    if (nrow(df.C) == 0 | nrow(df.P) == 0){ 
    } else {
    U <- df.P$P[1]
    X <- df.P$X
    IV <- df.P$IV
    tau <-  df.P$tau[1]
    
    #----------------------------------------------------
    # calculate risk free rate and zero coupon bond price
    #----------------------------------------------------
    
    rf <- gsw_yields(params = params, maturity = tau) / 100
    zc.P <- exp(-rf * tau)
    
    #-----------------------------------------
    # fit a cubic spline in strike - IV space
    #-----------------------------------------
    
    # choose the strike grid
    strike.grid <- seq(92, 100, by = 0.01)
    
    # fit spline to put prices
    # spline.P <-
    #   summary(lm(df.P$IV ~ df.P$X  + I(df.P$X ^ 2) + I(df.P$X ^ 3) + I(df.P$X ^ 4)))
    #  coef.P <- spline.P$coefficients[, "Estimate"]
    #  fitted.P.IVs <-
    #    fit_spline(coef.P, strike.grid = strike.grid, order = 4)
    
    # use local polynomial regression instead of spline
    x <- df.P$X
    y <- df.P$IV
    locpolyfit <-  locpoly(x, y, bandwidth = 1)  
    fitted.P.IVs <-  locpolyfit$y
    strike.grid <- locpolyfit$x
    
    fitted.P.prices <- BS_put_price(
      S = U,
      X = strike.grid,
      rf = rf,
      IV = fitted.P.IVs,
      tau = tau
    )
    
    fitted.P.prices[which(fitted.P.prices < 0)] <- 0
    
    # fit spline to call prices
    # spline.C <-
    #   summary(lm(df.C$IV ~ df.C$X  + I(df.C$X ^ 2) + I(df.C$X ^ 3) + I(df.C$X ^ 4)))
    # coef.C <- spline.C$coefficients[, "Estimate"]
    # fitted.C.IVs <-
    #   fit_spline(coef.C, strike.grid = strike.grid, order = 4)
    
    
    # use local polynomial regression instead of spline
    x <- df.C$X
    y <- df.C$IV
    locpolyfit <-  locpoly(x, y, bandwidth = 1)  
    fitted.C.IVs <-  locpolyfit$y
    strike.grid <- locpolyfit$x
    
    fitted.C.prices <- BS_call_price(
      S = U,
      X = strike.grid,
      rf = rf,
      IV = fitted.C.IVs,
      tau = tau
    )
    
    fitted.C.prices[which(fitted.C.prices < 0)] <- 0
    
    
    # plot results as a sanity check of result of spline
    # plot(strike.grid, fitted.P.prices, type = "l", col = "blue", lwd = "3", xlab = "Strike", ylab = "Price")
    # lines(strike.grid, fitted.C.prices)
    # abline(h = 0, v = U)
    # mtext(as.Date(date, origin = "1970-01-01"))
    # grid()
    
    crash.prob <- get.crash.prob(
      strikes = strike.grid,
      put.prices = fitted.P.prices,
      alpha = 0.99,
      underlying.price = U,
      risk.free.rate = rf
    )
    
    lower.bound.prob <-
      lower.bound(
        strikes = strike.grid,
        call.prices = fitted.C.prices,
        lower.bound = 99.5,
        upper.bound = 100,
        risk.free.rate = rf
      )
    
    col.ind <- which(colnames(crash.probs) == con)
    row.ind <- which(crash.probs$date == date)
    
    if (length(crash.prob) == 0) {
      crash.prob <- NA
    }
    # if (length(lower.bound.prob) == 0) {
    #   lower.bound.prob <- NA
    # }
    
    crash.probs[row.ind, col.ind] <- crash.prob
    lower.bound.probs[row.ind, col.ind] <- lower.bound.prob
    tau.data[row.ind, col.ind] <- tau
    diff_mat[row.ind, col.ind] <- max(diff(fitted.C.prices))
    }
  }
  pr$step()
}

matplot(diff_mat, type = "l", ylim = c(-0.01, 0.01))

crash_probs_long <- melt(data = crash.probs, id.vars = "date")
crash_probs_long <- na.omit(crash_probs_long)
ggplot(crash_probs_long, aes(x = date, y = value, colour = variable)) + 
  geom_line(size = 1.25) + 
  theme(legend.position = "none")

tau_long <- melt(data = tau.data, id.vars = "date")
tau_long <- na.omit(tau_long)
ggplot(tau_long, aes(x = date, y = value, colour = variable)) +
  scale_color_manual(values = rep("black", 38)) + 
  geom_line(size = 1.25) + 
  theme(legend.position = "none")

lbi_long <- melt(data = lower.bound.probs, id.vars = "date")
lbi_long <- na.omit(lbi_long)
ggplot(lbi_long, aes(x = date, y = value, colour = variable)) + 
  geom_line(size = 1.25) + 
  theme(legend.position = "none")

#===============================================================================
# Interpolate the  digital put prices i.e. risk neutral expectations of an event
#===============================================================================

taus <- c(0.5, 1, 1.5, 2, 2.48)

# linterpolate the crash probabiilies. i.e. the probabilities of interest rate increases
crash_mat <- get.interpolated.variables(data.long = crash_probs_long, tau.long = tau_long, taus = taus)
crash_mat_ma <- apply(X = crash_mat, MARGIN = 2, FUN = ma)
crash.df <- data.frame(date = dates, crash_mat_ma)
colnames(crash.df) <- c("date", c(0.5, 1, 1.5, 2, 2.5))

crash_long <- na.omit(melt(crash.df, id.vars = "date"))
ggplot(crash_long, aes(x = date, y = value, colour = variable)) + 
  geom_line(size = 1.25) + 
  xlab("") + ylab("Percent") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman")) + 
  ggtitle("Probability of interest rates increasing")

ggsave("../../figures/digital_puts.png", height = 8, width = 10)

# interpolate the zero lower bound probabilities
# loop through each date and find the tau year maturity
lbi_mat <- get.interpolated.variables(data.long = lbi_long, tau.long = tau_long, taus = taus)
lbi_mat_ma <- apply(X = lbi_mat, MARGIN = 2, FUN = ma)
lbi.df <- data.frame(date = dates, lbi_mat_ma)
colnames(lbi.df) <- c("date", c(0.5, 1, 1.5, 2, 2.5))

lbi_prob_long <- na.omit(melt(lbi.df[, -2], id.vars = "date"))
ggplot(lbi_prob_long, aes(x = date, y = value, colour = variable)) + 
  geom_line(size = 1.25) + 
  xlab("") + ylab("Percent") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman")) + 
  ggtitle("Probability of being at zero lower bound at expiration")

ggsave("../../figures/zlb_prob.png", height = 8, width = 10)

write.csv(lbi_prob_long, "../../data/lower_bound_probs.csv")

#===============================================================================
# Next, I investigate the change in this measure around FOMC announcements
#===============================================================================

fomc_dates <- read.csv("../../data/fomc_announcement.csv", stringsAsFactors = F)
fomc_dates <- as.Date(fomc_dates$date, format = "%m/%d/%Y")
n_meetings <- length(fomc_dates)

mpu_data <- crash.df
mpu_change_list <- list()
tau <- -14:14

for (i in 1:length(tau)) {
  mpu_change_list[[i]] <- calc_mpu_change(mpu_data = mpu_data, fomc_dates = fomc_dates, tau_delta = tau[i])
}

names(mpu_change_list) <- tau



ave_chg <- Reduce(rbind, lapply(mpu_change_list, get_col_mean))
ave_chg <- data.frame(day = tau, ave_chg)
colnames(ave_chg) <- c("day", colnames(crash.df)[-1])
df_long <- melt(ave_chg, id.vars = "day")
df_long_subset <- which(df_long$variable == "1.5")
ggplot(df_long, aes(x = day, y = value, colour = variable)) +
  geom_line(size = 1.25)  + 
  xlab("Horizon (days)") + ylab("Percent")+ 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman")) +
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) +
  ggtitle("Probability changes around FOMC announcements")

ggsave("../../figures/prob_hikes_fomc.png", width = 10, height = 8)

#===============================================================================
# Next, we can consider the probability of interest rates going down
#===============================================================================


