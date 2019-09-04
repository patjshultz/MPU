# functions for ED data
load_options <- function(){
  
  call_list <- list()
  put_list <- list()
  options_list <- list()
  
  # find tickers of the underlying contract (title of each sheet)
  data_path <- "../../data/ED_data.xlsx"
  tickers <- excel_sheets(data_path)[-c(1, 2)]
  ncontracts <- length(tickers)
  
  for(i in 1:length(tickers)){
    
    # expiration schedule 
    expir <- read_excel(data_path, sheet = "Dictionary")
    
    # read in the security specific information and data
    df <-  read_excel(data_path, sheet = tickers[i], col_types = "numeric")[-(1:6),]
    info <- read_excel(data_path, sheet = tickers[i])[1:4, ]
    
    # collect strikes
    strikes <- as.numeric(info[which(info[, 1] == "Strike"), -1])
    colnames(df) <- c("date", paste(tickers[i],strikes, sep = "_"))
    
    # reformat dataframe
    df$date <- as.Date(df$date, origin = "1899-12-30")
    df <- melt(data = df, id.vars = "date")
    df <- na.omit(df)
    
    # add a column for the expiration date
    con_ind <- which(expir$Contract == substr(tickers[i], 1, 4))
    expir_date <- as.Date(expir$Expiration[con_ind])
    df$expiration <- expir_date
    df$tau <- (df$expiration - df$date) / 365.25
    
    # allocate to list
    if(i %% 2 == 0){
      put_list[[i/2]] <- df
      names(put_list)[[i/2]] <- tickers[i]
    } else {
      call_list[[i - (i-1)/2]] <- df 
      names(call_list)[[i - (i-1)/2]] <- tickers[i]
    }
  }
  
  options_list[[1]] <- put_list
  options_list[[2]] <- call_list
  names(options_list) <- c("puts", "calls")
  
  
  # print summary
  cat("Contracts:", tickers, "\n")
  cat("Strikes:", strikes, "\n")
  return(options_list)
}

#===============================================================================
# function to load Eurodollar data in new format
#===============================================================================
load_options_new <- function(){
  require(readxl)
  require(stringr)
  call_list <- list()
  put_list <- list()
  options_list <- list()
  
  # find tickers of the underlying contract (title of each sheet)
  data_path <- "../../data/ED_data_new_format.xlsx"
  tickers <- excel_sheets(data_path)[-c(1, 2)]
  ncontracts <- length(tickers)
  
  for(i in 1:length(tickers)){
    
    # expiration schedule 
    expir <- read_excel(data_path, sheet = "Dictionary")
    
    # read in the security specific information and data
    sheet <- suppressWarnings(suppressMessages(read_excel(data_path, sheet = tickers[i], col_types = "numeric", )))
    df <-  sheet[-(1:6),]
    df <- na.omit(df)
    contracts <- suppressWarnings(suppressMessages(read_excel(data_path, sheet = tickers[i])[3, ]))
    contracts <- as.character(contracts)[-1]
    colnames(df) <- c("date", contracts)
    
    # collect strikes
    strikes <- str_extract(substring(contracts, 6), "\\-*\\d+\\.*\\d*")
    strikes <- as.numeric(strikes)
    colnames(df) <- c("date", paste(tickers[i], strikes, sep = "_"))
    df$date <- as.Date(df$date, origin = "1899-12-30")
    
    # reformat dataframe
    df <- melt(data = df, id.vars = "date")
    
    # add a column for the expiration date
    con_ind <- which(expir$Contract == substr(tickers[i], 1, 4))
    expir_date <- as.Date(expir$Expiration[con_ind])
    df$expiration <- expir_date
    df$tau <- (df$expiration - df$date) / 365.25
    
    # allocate to list
    if(i %% 2 == 0){
      put_list[[i/2]] <- df
      names(put_list)[[i/2]] <- tickers[i]
    } else {
      call_list[[i - (i-1)/2]] <- df 
      names(call_list)[[i - (i-1)/2]] <- tickers[i]
    }
    cat(tickers[i], "\n")
  }
  
  options_list[[1]] <- put_list
  options_list[[2]] <- call_list
  names(options_list) <- c("puts", "calls")
  
  
  # print summary
  cat("Contracts:", tickers, "\n")
  cat("Strikes:", strikes, "\n")
  return(options_list)
}


#===============================================================================
# function to load underlying Eurodollarfutures 
#===============================================================================

load_underlying <- function(){
  underlying_list <- list()
  data_path <- "../../data/ED_data.xlsx"
  
  df <- read_excel(data_path, sheet = "underlying", skip = 3, trim_ws = T)
  nunderlying = ncol(df) / 2
  
  for (j in 1:nunderlying) {
    ind <- 2 * j - 1
    underlying_data <- as.data.frame(df[-1, ind:(ind+1)])
    
    contract <- colnames(underlying_data)[2]
    colnames(underlying_data) <- c("date", contract)
    underlying_data$date <- as.Date(underlying_data$date)
    
    # trim NAs
    underlying_data <- na.omit(underlying_data)
    
    underlying_list[[j]] <- underlying_data
    names(underlying_list)[[j]] <- contract
    
  }
  my_merge <- function(x,y) merge(x = x, y = y, by = "date", all = T)
  x <- Reduce(f = my_merge, underlying_list)
  
  return_df <- na.omit(melt(x, id.vars = "date"))
  return(return_df)
}

#===============================================================================
# Pricing formulas for futures
#===============================================================================
BS_call_price <- function(S=S, X=X, rf=rf, IV=IV, tau=tau){
  # S is the current Price of the underlying asset
  # X is the strike price
  # rf is the annualized risk free yield
  # IV is the black implied volatility
  # tau is the time in years to expiration
  # return: C, the call price
  
  d1 <- (log(S / X) + (rf + (1 / 2) * IV^2) * (tau)) / (IV * sqrt(tau))
  d2 <- (log(S / X) + (rf - (1 / 2) * IV^2) * (tau)) / (IV * sqrt(tau))
  
  C <- exp(-rf * tau) * (S * pnorm(d1) - X * pnorm(d2))
  return(C)
}

BS_put_price <- function(S=S, X=X, rf=rf, IV=IV, tau=tau){
  # S is the current Price of the underlying asset
  # X is the strike price
  # rf is the annualized risk free yield
  # IV is the black implied volatility
  # tau is the time in years to expiration
  # return: C, the call price
  
  d1 <- (log(S / X) + (rf + (1 / 2) * IV^2) * (tau)) / (IV * sqrt(tau))
  d2 <- (log(S / X) + (rf - (1 / 2) * IV^2) * (tau)) / (IV * sqrt(tau))
  
  P <- exp(-rf * tau) * (X * pnorm(-d2) - S * pnorm(-d1))
  return(P)
}


get_IVs <- function (rf, tau, S, X, C, PC) {
  # rf : risk free rate
  # tau : time to expiration
  # S : current value of the underlying
  # X : Strike price
  # C : option price
  # PC: put or option
  
  if(PC == "C"){
    f = function(IV) {
      
      d1 <- (log(S / X) + (rf + (1 / 2) * IV^2) * (tau)) / (IV * sqrt(tau))
      d2 <- (log(S / X) + (rf - (1 / 2) * IV^2) * (tau)) / (IV * sqrt(tau))
      
      err <- (exp(-rf * tau) * (S * pnorm(d1) - X * pnorm(d2))) - C
      return(err)
    }
    sigma = uniroot(f, lower = -1, upper = 10, maxiter = 100000)$root
    return(sigma)
  } else if (PC == "P"){
    f = function(IV) {
      BS.P <- BS_put_price(S = S, X = X, rf = rf, IV = IV, tau = tau)
      err <- BS.P - C
      return(err)
    }
    sigma = uniroot(f, lower = -1, upper = 10, maxiter = 100000)$root
    return(sigma)
  }
}


#===============================================================================
# Svensson yield curve function
#===============================================================================
gsw_yields <- function(params, maturity){
  # params: vector of parameters from GSW model
  # maturity: numeric value of maturity (in years) of yield to be calculated
  
  # parameters
  beta0 <- params$BETA0
  beta1 <- params$BETA1
  beta2 <- params$BETA2
  beta3 <- params$BETA3
  tau1 <- params$TAU1
  tau2 <- params$TAU2
  n <- maturity
  
  # to ease notation in computation of yields
  ntau1 <- n / tau1
  ntau2 <- n / tau2
  numerator_tau1 <- 1 - exp(-ntau1)
  numerator_tau2 <- 1 - exp(-ntau2)
  
  # calculate each term separately
  B0 <- beta0
  B1 <- beta1 * (numerator_tau1 / ntau1)
  B2 <- beta2 * ((numerator_tau1 / ntau1) - exp(-ntau1))
  B3 <- beta3 * ((numerator_tau2 / ntau2) - exp(-ntau2))
  
  yield <- B0 + B1 + B2 + B3
  return(yield)
}

fit_spline <- function(coef, strike.grid, order){
  X <- strike.grid
  
  if(order == 2) {
    return(coef[1] + coef[2] * X + coef[3] * X^2)
  } else if (order == 3) {
    return(coef[1] + coef[2] * X + coef[3] * X^2 + coef[4] * X^3)
  } else if (order == 4) {
    return(coef[1] + coef[2] * X + coef[3] * X^2 + coef[4] * X^3 + coef[5] * X^4)
  } else if (order == 5) {
    return(coef[1] + coef[2] * X + coef[3] * X^2 + coef[4] * X^3 + coef[5] * X^4 + coef[6] * X^5)
    
  }
}



rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
} 



calc_mpu_change <- function(mpu_data, fomc_dates, tau_delta){
  # mpu_data: data frame containing the mpu data at various horizons
  # fomc_dates : vectorfomc dates
  # tau delta is the difference in days between the observation and the day prior to FOMC meetings
  
  # create empty matrix to store calculations in 
  n_meetings <- length(fomc_dates)
  mpu_change <- matrix(data = 0, nrow = n_meetings, ncol = (ncol(mpu_data)-1))
  print(tau_delta)
  mpu_dates <- mpu_data$date
  for (i in 1:n_meetings) {
    
    # pick fomc date
    fomc_date <- fomc_dates[i]
    
    # find the lead and lag dates in the mpu data
    fomc_tau <- which(mpu_dates == fomc_date) 
    delta_mpu <- as.matrix(mpu_data[fomc_tau, - 1] - mpu_data[(fomc_tau - tau_delta), -1])
    mpu_change[i, ] <- delta_mpu
  }
  
  mpu_change <- as.data.frame(data.frame(date = fomc_dates, mpu_change))
  colnames(mpu_change) <- colnames(mpu_data)
  return(mpu_change)
}
get_col_mean <- function(X){
  colMeans(X[, -1], na.rm = T)
}


calc_diff_ts <- function(df, pch = F) {
  #df: data frame with the first column as a date
  
  if (pch == F) {
    df_diff <- diff(as.matrix(df[, -1]))
    df_diff <- data.frame(date = df$date[-1], df_diff)
    colnames(df_diff) <- colnames(df)
  } else if (pch == T) {
    df_diff <- diff(as.matrix(df[, -1]))
    df_diff <- 100 * (df_diff / as.matrix(df[-nrow(df), -1])) 
    df_diff <- data.frame(date = df$date[-1], df_diff)
    colnames(df_diff) <- colnames(df)
  }
  
  return(df_diff)
  
}


get_term_premium_regression <- function(x, y, control, include.control = T){
  # x: the independent variable
  # termp_premium: matrix containing term premium of 1-10 years
  # control : additional regressors to be added to mpu
  
  coef_mat <- matrix(data = NA, nrow = 10, ncol = 4)
  coef_mat[, 1] <- 1:10
  
  if(include.control == T){
    for(i in 1:10){
      beta <- coef(summary(lm(y[,  i] ~ x + control)))["x", "Estimate"] 
      std.error <- coef(summary(lm(y[,  i] ~ x + control)))["x", "Std. Error"]
      
      coef_mat[i, 2] <- beta
      coef_mat[i, 3] <- beta - 1.96 * std.error
      coef_mat[i, 4] <- beta + 1.96 * std.error
      
    }
  } else if (include.control == F){
    for(i in 1:10){
      beta <- coef(summary(lm(y[,  i] ~ x)))["x", "Estimate"] 
      std.error <- coef(summary(lm(y[,  i] ~ x)))["x", "Std. Error"]
      
      coef_mat[i, 2] <- beta
      coef_mat[i, 3] <- beta - 1.96 * std.error
      coef_mat[i, 4] <- beta + 1.96 * std.error
      
    }
  }
  
  colnames(coef_mat) <- c("Horizon", "Beta", "CILB", "CIUB")
  return(as.data.frame(coef_mat))
  
}


list_merge <- function(x, y){
  return(merge(x, y, by = "date"))
}

get_eds_regression <- function(x, y, control, include.control = F){
  # x: the independent variable normally eurodollar based surprises
  # termp_premium: matrix containing term premium of 1-10 years
  # control : additional regressors to be added to mpu normally left out in this specification
  
  coef_mat <- matrix(data = NA, nrow = 10, ncol = 4)
  coef_mat[, 1] <- 1:10
  
  if(include.control == T){
    for(i in 1:10){
      beta <- coef(summary(lm(y[,  i] ~ x + control)))["x", "Estimate"] 
      std.error <- coef(summary(lm(y[,  i] ~ x + control)))["x", "Std. Error"]
      
      coef_mat[i, 2] <- beta
      coef_mat[i, 3] <- beta - 1.96 * std.error
      coef_mat[i, 4] <- beta + 1.96 * std.error
      
    }
  } else if (include.control == F){
    for(i in 1:10){
      beta <- coef(summary(lm(y[,  i] ~ x)))["x", "Estimate"] 
      std.error <- coef(summary(lm(y[,  i] ~ x)))["x", "Std. Error"]
      
      coef_mat[i, 2] <- beta
      coef_mat[i, 3] <- beta - 1.96 * std.error
      coef_mat[i, 4] <- beta + 1.96 * std.error
      
    }
  }
  
  colnames(coef_mat) <- c("Horizon", "Beta", "CILB", "CIUB")
  return(as.data.frame(coef_mat))
  
}


get.crash.prob <- function(strikes = strike.grid, put.prices=fitted.P.prices, alpha = 0.995, underlying.price = U, risk.free.rate = rf){
  # calculate the "crash price of the eurodollar future
  crash.strike <- alpha *  underlying.price
  
  # find the indices of prices around the crash price in the price grid
  ind <- which(strike.grid > crash.strike)[1]
  crash.strike.grid <- strike.grid[c(ind-1, ind)]
  crash.price.ind <- c(ind-1, ind)
  crash.prices.grid <- put.prices[crash.price.ind]
  
  # calculate weighted average crash price
  weights <- 1 - 10 * abs(crash.strike.grid - crash.strike)
  crash.price.weighted <- weights[1] * crash.prices.grid[1] + weights[2] * crash.prices.grid[2]
  
  # calculate the slope of the put price function around that point
  dK <- diff(strike.grid)[1]
  dP <- diff(crash.prices.grid)
  put.prime <- dP / dK
  risk.neutral.prob <- (1 + risk.free.rate) * put.prime
  
  # calculate crash probability
  crash.prob <- alpha * (put.prime  - (crash.price.weighted / crash.strike))
  return(100 *risk.neutral.prob)
  
}



lower.bound <-  function(strikes = strike.grid, call.prices = fitted.C.prices, lower.bound = U,
                         upper.bound = 100, risk.free.rate = rf) {
  
  nstrikes <- length(strike.grid)
  
  # find the indices of prices around the the lower bound of pixel in the price grid
  ind <- which(strike.grid > lower.bound)[1]
  lb.lower.strikes <- strike.grid[c(ind-1, ind)]
  lb.price.ind <- c(ind-1, ind)
  lb.prices.grid <- call.prices[lb.price.ind]
  
  # calculate weighted average crash price
  weights <- 1 - 10 * abs(lb.lower.strikes - lower.bound)
  lb.price.weighted <- weights[1] * lb.prices.grid[1] + weights[2] * lb.prices.grid[2]
  
  # calculate the slope of the put price function around that point
  dK <- diff(strike.grid)[1]
  dP <- diff(lb.prices.grid)
  lb.call.prime <- dP / dK
  
  # repeat, but for the upper bound of the pixel in the price grid
  
  dK <- diff(strike.grid)[1]
  if(upper.bound == 100){
    ub.prices.grid <- fitted.C.prices[c((nstrikes - 1), (nstrikes - 0))]
    dP <- diff(ub.prices.grid)
  } else {
    # find the indices of prices around the the lower bound of pixel in the price grid
    ind <- which(strike.grid > upper.bound)[1]
    ub.lower.strikes <- strike.grid[c(ind-1, ind)]
    ub.price.ind <- c(ind-1, ind)
    ub.prices.grid <- call.prices[ub.price.ind]
    
    # calculate weighted average crash price
    weights <- 1 - 10 * abs(ub.lower.strikes - upper.bound)
    ub.price.weighted <- weights[1] * ub.prices.grid[1] + weights[2] * ub.prices.grid[2]
    dP <- diff(ub.prices.grid)
  }
  
  ub.call.prime <- dP / dK
  
  # lower bound prob <
  lbi <- (1 + rf)  * (ub.call.prime - lb.call.prime)
  
  return(100 * lbi)
}



get.interpolated.variables <- function(data.long, tau.long = tau_long, taus){
  interpolated_data_mat <- matrix(data = NA, nrow = nobs, ncol = length(taus))
  for(tau in taus) {
    tau.ind <- which(taus == tau)
    for (i in 1:nobs) {
      date <- dates[i]
      contract.data.daily <- data.long[which(data.long$date == date),]
      tau.data <- tau_long[which(tau_long$date == date),]
      
      # find the two security above an below tau
      above_tau_ind <- min(which(tau.data$value > tau))
      below_tau_ind <- max(which(tau.data$value <= tau))
      
      if(above_tau_ind == -Inf){
        interpolated_data_mat[i, tau.ind] <- NA
      } else if (below_tau_ind == -Inf) {
        interpolated_data_mat[i, tau.ind] <- NA
      } else {
        # taus above and below
        above_tau <- tau.data$value[above_tau_ind]
        below_tau <- tau.data$value[below_tau_ind]
        tau_diff <- above_tau - below_tau
        
        # which contracts are above and below tau
        contract_above <- as.character(tau.data$variable)[above_tau_ind]
        contract_below <- as.character(tau.data$variable)[below_tau_ind]
        
        prob_above <- contract.data.daily$value[which(as.character(contract.data.daily$variable) == contract_above)]
        prob_below <- contract.data.daily$value[which(as.character(contract.data.daily$variable) == contract_below)]
        
        # calculate weights
        weight_above <-  1- ((above_tau - tau) / tau_diff)
        weight_below <- 1 - weight_above
        
        # create weighted average meaure 
        weighted_prob <- weight_above * prob_above + weight_below * prob_below
        interpolated_data_mat[i, tau.ind] <- weighted_prob
      }
    }
  }
  return(interpolated_data_mat)
}


ma <- function(x, n = 10){stats::filter(x, rep(1 / n, n), sides = 1)}

