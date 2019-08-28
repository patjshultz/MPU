#===============================================================================
# Calculate "crash probability" of eurodollar futures. THis corresponds to the
# probability of a large interest rate hike
#===============================================================================

rm(list = ls())
library(tidyverse)
library(reshape2)
library(splines)
library(plyr)
source("functions.R")
library(extrafont)
loadfonts()
theme_set(theme_bw(base_size = 20))

#===============================================================================
# load options data with implied volatilities calculated
# also load GSW parameters to get risk free bond prices at any maturity
#===============================================================================

options_data <- read_csv("../../data/ED_options_IV.csv")

# drop to just use options data
options_data <- options_data[which(options_data$PC == "P"),]

# set up global variables
options_dates <- unique(options_data$date)
all.contracts <- unique(options_data$underlying)
total.n.contracts <- length(all.contracts)

# drop data with tau = 0
drop_ind <- which(options_data$tau <= 0)
options_data <- options_data[-drop_ind,]

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

# matrix to store smooth 60 strikes in
smooth_calls <- matrix(data = 0, nrow = nobs, ncol = 61)

# set up progress bar
nloops <- length(dates)
pr <- progress_text()
pr$init(nloops)

# allocate empty matrices for crash probabilities and for taus
crash.probs <-
  matrix(data = NA,
         nrow = nobs,
         ncol = (total.n.contracts + 1))

tau.df <-
  matrix(data = NA,
         nrow = nobs,
         ncol = (total.n.contracts + 1))

# label columns
colnames(crash.probs) <- c("date", all.contracts)
colnames(tau.df) <- c("date", all.contracts)

# convert to dataframes and allocate date column
crash.probs <- as.data.frame(crash.probs)
crash.probs$date <- dates

tau.df <- as.data.frame(tau.df)
tau.df$date <- dates


rate.change <- 0.01

# test for specific contract
# options_data <- options_data[which(options_data$underlying == "EDU15 Comdty"), ]
# dates <- unique(options_data$date)
# date <- dates[400]

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
    
    # for crash probability we only need puts
    df.P <-
      daily_data[which(daily_data$underlying == con &
                         daily_data$PC == "P"), ]
    
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
    strike.grid <- seq(92, 101, by = 0.1)
    
    # fit spline to put prices
    spline.P <-
      summary(lm(df.P$IV ~ df.P$X  + I(df.P$X ^ 2) + I(df.P$X ^ 3)))
    coef.P <- spline.P$coefficients[, "Estimate"]
    fitted.P.IVs <-
      fit_spline(coef.P, strike.grid = strike.grid, order = 3)
    
    fitted.P.prices <- BS_put_price(
      S = U,
      X = strike.grid,
      rf = rf,
      IV = fitted.P.IVs,
      tau = tau
    )
    
    fitted.P.prices[which(fitted.P.prices < 0)] <- 0
    
    # plot results as a sanity check of result of spline
    # plot(strike.grid, fitted.P.prices, type = "l", col = "blue", lwd = "3", xlab = "Strike", ylab = "Price")
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
    
    col.ind <- which(colnames(crash.probs) == con)
    row.ind <- which(crash.probs$date == date)
    
    if (length(crash.prob) == 0) {
      crash.prob <- NA
    }
    crash.probs[row.ind, col.ind] <- crash.prob
    tau.df[row.ind, col.ind] <- tau
  }
pr$step()
}

crash_probs_long <- melt(data = crash.probs, id.vars = "date")
crash_probs_long <- na.omit(crash_probs_long)
ggplot(crash_probs_long, aes(x = date, y = value, colour = variable)) + 
  geom_line(size = 1.25) + 
  theme(legend.position = "none")

tau_long <- melt(data = tau.df, id.vars = "date")
tau_long <- na.omit(tau_long)
ggplot(tau_long, aes(x = date, y = value, colour = variable)) +
  scale_color_manual(values = rep("black", 38)) + 
  geom_line(size = 1.25) + 
  theme(legend.position = "none")

#===============================================================================
# Interpolate the  digital put prices i.e. risk neutral expectations of an event
#===============================================================================

taus <- c(0.5,1, 1.5, 2, 2.48)

# loop through each date and find the tau year maturity

crash_mat <- matrix(data = NA, nrow = nobs, ncol = length(taus))

for(tau in taus) {
  tau.ind <- which(taus == tau)
  for (i in 1:nobs) {
    date <- dates[i]
    crash.data <- crash_probs_long[which(crash_probs_long$date == date),]
    tau.data <- tau_long[which(tau_long$date == date),]
    
    # find the two security above an below tau
    above_tau_ind <- min(which(tau.data$value > tau))
    below_tau_ind <- max(which(tau.data$value <= tau))
    
    if(above_tau_ind == -Inf){
      crash_mat[i, tau.ind] <- NA
    } else if (below_tau_ind == -Inf) {
      crash_mat[i, tau.ind] <- NA
    } else {
    # taus above and below
    above_tau <- tau.data$value[above_tau_ind]
    below_tau <- tau.data$value[below_tau_ind]
    tau_diff <- above_tau - below_tau
    
    # which contracts are above and below tau
    contract_above <- as.character(tau.data$variable)[above_tau_ind]
    contract_below <- as.character(tau.data$variable)[below_tau_ind]
    
    prob_above <- crash.data$value[which(as.character(crash.data$variable) == contract_above)]
    prob_below <- crash.data$value[which(as.character(crash.data$variable) == contract_below)]
    
    # calculate weights
    weight_above <-  1- ((above_tau - tau) / tau_diff)
    weight_below <- 1 - weight_above
    
    # create weighted average meaure 
    weighted_prob <- weight_above * prob_above + weight_below * prob_below
    crash_mat[i, tau.ind] <- weighted_prob
    }
  }
}

ma <- function(x, n = 10){stats::filter(x, rep(1 / n, n), sides = 1)}
crash_mat_ma <- apply(X = crash_mat, MARGIN = 2, FUN = ma)

crash.df <- data.frame(date = dates, crash_mat_ma)
colnames(crash.df) <- c("date", taus)

crash_long <- na.omit(melt(crash.df, id.vars = "date"))
ggplot(crash_long, aes(x = date, y = value, colour = variable)) + 
  geom_line(size = 1.25) + 
  xlab("") + ylab("Percent") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman")) + 
  ylim(0, max(crash_long$value)) + 
  

ggsave("../../digital_puts.png", height = 10, width = 6)




