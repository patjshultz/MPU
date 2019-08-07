#===============================================================================
# Script that uses cleaned data on Eurodollar options and futures to calculate
# monetary policy uncertainty
#===============================================================================

# housekeeping
rm(list = ls())
library(tidyverse)
library("RND")
source("functions.R")
library(plyr)

#===============================================================================
# load data
#===============================================================================

options <- read_csv("../../data/ed_options.csv")
unique(options$underlying)
futures <- read_csv("../../data/ed_underyling.csv")

gsw_params <- read_csv("../../data/gsw_parameters.csv", guess_max = 10000)
gsw_params <- na.omit(gsw_params)

#===============================================================================
#  for each dates select oom put and call contracts and calculate implied volatilities
#===============================================================================

#---------------------------------------------------------
# make indicator variable for being in or out of the money
#--------------------------------------------------------

p.high.ind <- which(options$P > options$X)
p.low.ind <- which(options$P < options$X)

c.ind <- which(options$PC == "C")
p.ind <- which(options$PC == "P")

itm.c.ind <- intersect(p.high.ind, c.ind)
ED.C.ind <- intersect(p.low.ind, c.ind)
itm.p.ind <- intersect(p.low.ind, p.ind)
ED.P.ind <- intersect(p.high.ind, p.ind)

options$moneyness <- NA
options$moneyness[itm.c.ind] <- "itm"
options$moneyness[ED.C.ind] <- "otm"
options$moneyness[itm.p.ind] <- "itm"
options$moneyness[ED.P.ind] <- "otm"

options <- na.omit(options) # drop observations with stale data that results in NAs

#--------------------------------------------
# loop through each day and calculate the IVs
#-------------------------------------------

date <- as.Date("2012-09-18", format = "%Y-%m-%d")

dates <- as.Date(intersect(options$date, gsw_params$date), origin = "1970-01-01")
options <- na.omit(options)
options_list <- list()

nloops <- length(dates)
pr <- progress_text()
pr$init(nloops)

# test <- options[which(options$underlying == "EDU21 Comdty"), ]
# testd <- test[which(test$date == as.Date("2017-11-21")), ]

for(date in dates){
  
  #  select data for the given date
  ED_O <- options[which(options$date == date), ]
  ED_F <- futures[which(futures$date == date), ]
  params <- gsw_params[which(gsw_params$date == date), ]
  ED_O$IV <- NA
  
  #-------------------------------------------------------
  # select put and call contracts
  #-------------------------------------------------------
  
  ED.C <- ED_O[which(ED_O$PC == "C"), ]
  ED.P <- ED_O[which(ED_O$PC == "P"), ]
  
  #-------------------------------------------------------
  # 3. calculate IVs
  #-------------------------------------------------------
  
  n.C <- nrow(ED.C)
  n.P <- nrow(ED.P)
  
  #  calculate Black IVs for call options 
  for(i in 1:n.C){
    # select options data
    S <- ED.C$P[i]
    X <- ED.C$X[i] 
    tau <- ED.C$tau[i]
    rf <- gsw_yields(params = params, maturity = tau) / 100
    C <- ED.C$G[i]
    
    # get the black implied volatility
    if(tau <= 0){
      ED.C$IV[i] <- NA
    } else {
      ED.C$IV[i] <- get_IVs(rf = rf, tau = tau, S = S, X = X, C = C, PC = "C")
      
      # check if IV is positive
      if(ED.C$IV[i] < 0){
        warning(paste("Error on ", date, "IV = ", ED.C$IV[i], "\n", sep = ""))
        ED.C$IV[i] <- 0
      }
      
      # map the IV back to price and make sure the difference is small 
      implied_price <- BS_call_price(S=S, X=X, rf=rf, IV= ED.C$IV[i], tau = tau)
      err <- abs(C - implied_price)  
      
      if (err > 0.2) {
        warning(paste("Error on ", date,  "=", round(err, 2), "\n", sep = ""))
      }
    }
    
   
  }  
  
  # calculate black IVs for put options
  for(i in 1:n.P){
    # select options data
    S <- ED.P$P[i]
    X <- ED.P$X[i] 
    tau <- ED.P$tau[i] 
    rf <- gsw_yields(params = params, maturity = tau) / 100
    C <- ED.P$G[i]
    
    # get the black implied volatility
    if(tau <= 0){
      ED.P$IV[i] <- NA
    } else {
      ED.P$IV[i] <- get_IVs(rf = rf, tau = tau, S = S, X = X, C = C, PC = "P")
      
      # check if IV is positive
      if(ED.P$IV[i] < 0){
        warning(paste("Error on ", date, "IV = ", ED.P$IV[i], "\n", sep = ""))
        ED.P$IV[i] <- 0
      } 
      implied_price <- BS_put_price(S=S, X=X, rf=rf, IV=ED.P$IV[i] , tau = tau)
      err <- C - implied_price  
      
      if (abs(err) > 0.3) {
        warning(paste("Error on ", date, "=", round(err, 2),"| contract:", 
                      ED.P$ticker[i], "|index:", i, "\n", sep = ""))
      } 
    }
  }  
   
  
  df <- rbind(ED.C, ED.P)
  df <- df[order(df$X), ]
  options_list[[which(dates == date)]] <- df
  pr$step()
}

options_df <- Reduce(rbind, options_list)
write_csv(options_df, path = "../../data/ED_options_IV.csv")