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
  }
}


