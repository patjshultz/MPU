#===============================================================================
# Script to linearly interpolate between horizons to create an mpu measure at 
# a consistent maturity
#===============================================================================
rm(list = ls())
library(tidyverse)
library(dplyr)
#loadfonts()
# load data
mpu_data <- read_csv("../../data/contracts_mpu.csv")
dates <- unique(mpu_data$date)
nobs <- length(dates)
taus <- c(0.5,1, 1.5, 2, 2.48)

#===============================================================================
# loop through each date and find the tau year maturity
#===============================================================================
mpu_mat <- matrix(data = NA, nrow = nobs, ncol = length(taus))
for(tau in taus) {
  tau.ind <- which(taus == tau)
  for (i in 1:nobs) {
    date <- dates[i]
    mpu <- mpu_data[which(mpu_data$date == date),]
    tau_diff <- mpu$tau - tau
    
    
    # find the two security above an below tau
    above_tau <- min(mpu$tau[which(tau_diff > 0)])
    below_tau <- max(mpu$tau[which(tau_diff < 0)])
    
    # check that we have the necesary contract maturities trading
    if(below_tau == -Inf){
      mpu_mat[i, tau.ind] <- NA
    } else if (above_tau == -Inf) {
      mpu_mat[i, tau.ind] <- NA
    } else {
      # find the associated mpu measures for these contracts
      mpu_tau_high <- mpu$mpu[which(mpu$tau == above_tau)]
      mpu_tau_low <- mpu$mpu[which(mpu$tau == below_tau)]
      
      # calculate relative weights to assign to the two measures
      total_diff <- above_tau - below_tau
      weight_tau_high <- 1 - ((above_tau - tau) / total_diff)
      weight_tau_below <- 1 - ((tau - below_tau) / total_diff)
      
      # create weighted average tau measure
      mpu_tau <-
        weight_tau_below * mpu_tau_low + weight_tau_high * mpu_tau_high
      mpu_mat[i, tau.ind] <- mpu_tau
    }
    
    
  }
}

df <- data.frame(date = dates, mpu = mpu_mat)
colnames(df) <- c("date", round(taus, 1))
df_long <- melt(df[-(1:10), ], id.vars = "date")
df_long <- na.omit(df_long)
max_mpu <- max(df_long$value)



ggplot(df_long, aes(x = date, y = value, colour = variable)) + ylim(0, 2) +
  geom_line(size = 1.25) + scale_colour_grey() + 
  xlab("") + ylab("Percent")+ 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

ggsave(paste("../../figures/mpu", "png", sep = "."), width = 10, height = 6)
ggsave(paste("../../slides/mpu", "png", sep = "."), width = 10, height = 6)
write_csv(x = df, path = "../../data/tau_mpu.csv")

#---------------------------------------------# 
#changes in mpu
#---------------------------------------------

df <- data.frame(date = dates[-1], diff(mpu_mat))
colnames(df) <- c("date", paste("X", round(taus, 1), sep = ""))
df_long <- melt(df[-(1:10), ], id.vars = "date")
df_long <- na.omit(df_long)

df %>% mutate(monthyear = as.character(format(date, "%m-%Y"))) %>%
  arrange(date) %>%
  group_by(monthyear) %>%
  summarise(date=date[1], flow = sum(2))

ggplot(df_long, aes(x = date, y = value, colour = variable)) + 
  geom_line(size = 1.25) + 
  xlab("") + ylab("Percent")+ 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

