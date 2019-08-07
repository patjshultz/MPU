#===============================================================================
# Script that uses implied volatilities on out of the money options to compute mpu
#===============================================================================

rm(list = ls())
library(tidyverse)
library(reshape2)
library(splines)
library(plyr)
source("functions.R")
theme_set(theme_bw(base_size = 20))

#===============================================================================
# load options data with implied volatilities calculated
# also load GSW parameters to get risk free bond prices at any maturity
#===============================================================================

options_data <- read_csv("../../data/ED_options_IV.csv")
options_dates <- unique(options_data$date)

unique(options_data$underlying)

# drop the 0.0025 observations that seem like fake data
#drop_ind <- which(options_data$G == 0.0025)
#options_data <- options_data[-drop_ind, ]

# drop data with tau = 0
drop_ind <- which(options_data$tau <= 0)
options_data <- options_data[-drop_ind, ]

# load parameters to calculate zero coupon bond prices at any maturity
gsw_params <- read_csv("../../data/gsw_parameters.csv")

# find shared dates between gsw data and options data
gsw_dates <- gsw_params$date
dates <- intersect(x = options_dates, y = gsw_dates)
nobs <- length(dates)

#===============================================================================
# for each observation map the discrete IVs into a continuous function
# using a cubic spline.
#===============================================================================

# matrix to store smooth 60 strikes in
smooth_calls <- matrix(data = 0, nrow = nobs, ncol = 61)
mpu_list <- list()
i<-1

# set up progress bar
nloops <- length(dates)
pr <- progress_text()
pr$init(nloops)

# test for specific contrract
options_data <- options_data[which(options_data$underlying == "EDU21 Comdty"), ]
dates <- unique(options_data$date)

for (date in dates) {
  index <- which(dates == date)
  smooth_calls[index, 1] <- date
  
  # select all calls for a given day
  daily_data <- options_data[which(options_data$date == date), ]
  contracts <- unique(daily_data$underlying)
  ncontracts <- length(contracts)
  
  # select yield curve parameters
  params <- gsw_params[which(gsw_params$date == date), ]
  
  for (con in contracts) {
    
    #------------------------------
    # select specific contract data
    #------------------------------
    
    # check if puts and calls have started trading 
    df.C <-
      daily_data[which(daily_data$underlying == con &
                         daily_data$PC == "C"), ]
    df.P <-
      daily_data[which(daily_data$underlying == con &
                         daily_data$PC == "P"), ]
    
    if (nrow(df.C) == 0 | nrow(df.P) == 0){ #FIXME talk to bloomberg help desk to figure out why the options do not all trade on the same day
      mpu_list[[i]] <- c(date, con, NA)
    } else {
    
    df.C.otm <- df.C[which(df.C$moneyness == "otm"),]
    df.P.otm <- df.P[which(df.P$moneyness == "otm"),]
    
    df.otm <- rbind(df.C.otm, df.P.otm)
    U <- df.C$P[1]
    X <- df.C$X
    IV <- df.C$IV
    tau <-  df.C$tau[1]
    
    
    # plot IV vs strike
    yrange = c(0, max(c(df.C$IV, df.P$IV)))
    plot(df.C$X, df.C$IV, type = "l", lwd = "3", col = "blue", ylim = yrange)
    lines(df.P$X, df.P$IV, type = "l", lwd = "3", col = "red")
    legend(x = 92, y = 0.01, legend = c("Calls", "Puts"), col = c("blue", "red"), lty = 1)
    mtext(date)
    grid()

    #plot price vs strike
    plot(df.C$X, df.C$G, type = "l", lwd = "3", col = "blue", xlim = c(92, 100), ylim = c(-1, 5), main = "Obs prices")
    lines(df.P$X, df.P$G, type = "l", lwd = "3", col = "red")
    abline(h = NA, v = U)
    mtext(date)
    legend(x = 96, y = 3, legend = c("Calls", "Puts"), col = c("blue", "red"), lty = 1)
    grid()

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
    strike.grid.df <- data.frame(strikes = strike.grid)
  
    # fit a spline to the call prices
    IVs <- df.C$IV; strikes <- df.C$X
    fit.C <- loess(IVs ~ strikes)
    fitted.C.IVs <- predict(fit.C, strike.grid.df)  
    
    
    # fit spline to put prices
    IVs <- df.P$IV; strikes <- df.P$X
    fit.P <- loess(IVs ~ strikes)
    fitted.P.IVs <- predict(fit.P, strike.grid.df)  
    
    # convert back to prices
    fitted.C.prices <-
      BS_call_price(
        S = U,
        X = strike.grid,
        rf = rf,
        IV = fitted.C.IVs,
        tau = tau
      )
    fitted.P.prices <-
      BS_put_price(
        S = U,
        X = strike.grid,
        rf = rf,
        IV = fitted.P.IVs,
        tau = tau
      )
    
    fitted.C.prices[which(fitted.C.prices <0)] <- 0
    fitted.P.prices[which(fitted.P.prices <0)] <- 0
    
    # plot results as a sanity check of result of spline
     # plot(strike.grid, fitted.C.prices, type = "l", col = "blue", lwd = "3", ylim = c(-1, 5), main = "Spline check")
     # lines(strike.grid, fitted.P.prices, type = "l", col = "red", lwd = "3")
     # abline(h = 0, v = U)
     # mtext(date)
     # legend(x = 98, y = (max(df.C$G) - 2), legend = c("Calls", "Puts"), col = c("blue", "red"), lty = 1)
     # grid()


    # drop in the money options
    otm.C <- fitted.C.prices[which(strike.grid >= U)]
    otm.P <- fitted.P.prices[which(strike.grid < U)]
    otm <- c(na.omit(otm.P), na.omit(otm.C))
    otm[which(otm < 0)] <- 0
    
    # numerically integrate the function
    mpu <- sqrt((2 / zc.P) * sum(0.1 * otm))
    mpu_list[[i]] <- c(date, con, tau, mpu)
    # adjust indices
    i <- i + 1
    }
  }
  pr$step()
}

mpu <- Reduce(rbind, mpu_list)
mpu <- as.data.frame(mpu)

# format mpu dataframe
colnames(mpu) <- c("date", "contract", "tau", "mpu")

mpu$date <- as.numeric(as.character(mpu$date))
mpu$date <- as.Date(mpu$date, origin = "1970-01-01")

mpu$contract <- as.character(mpu$contract)
mpu$mpu <- as.numeric(as.character(mpu$mpu))

mpu$tau <- as.numeric(as.character(mpu$tau))
contracts <- unique(mpu$contract)

for (i in 1:length(contracts)){
  con <- as.character(contracts[i])
  
  test <- mpu[which(mpu$contract == con), -2]
  
  ggplot(test, aes(x = date, y = mpu)) + 
    geom_line(size = 1.25) + 
    xlab("") + ylab(con) +
    theme(legend.title = element_blank())
  ggsave(paste("../../figures/", con, "png", sep = "."), width = 10, height = 4)
  
  
}
write_csv(x = mpu, path = "../../data/mpu.csv")
