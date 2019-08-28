rm(list = ls())
library(tidyverse)
library(readxl)
library(reshape2)
source("functions.R")
theme_set(theme_bw(base_size = 20))

VIX <- read_csv("../../data/VIX.csv")
mpu <- read_csv("../../data/tau_mpu.csv")

df <- merge(VIX, mpu[, c(1, 6)], by = "date")
colnames(df) <- c(colnames(VIX), "mpu")
df$mpu <- df$mpu * 20

df_long <- reshape2::melt(df[, c(1, 2, 6)], id.vars = "date")
ggplot(df_long, aes(x = date, y = value, colour = variable)) + geom_line()


fomc_dates <- read.csv("../../data/fomc_announcement.csv", stringsAsFactors = F)
fomc_dates <- as.Date(fomc_dates$date, format = "%m/%d/%Y")
n_meetings <- length(fomc_dates)

#===============================================================================
# For each FOMC date find the ten trading dates before and after
# First calculate the relative levels of uncertainty around FOMC days
#===============================================================================
vix_data <- VIX
fomc_dates <- fomc_dates
vix_change_list <- list()
tau <- -15:15


for (i in 1:length(tau)) {
  vix_change_list[[i]] <-
    calc_mpu_change(mpu_data = vix_data,
                    fomc_dates = fomc_dates,
                    tau_delta = tau[i])
}

names(vix_change_list) <- tau



ave_chg <- Reduce(rbind, lapply(vix_change_list, get_col_mean))
ave_chg <- data.frame(day = tau, ave_chg)

colnames(ave_chg) <- c("day", colnames(VIX)[-1])
df_long <- melt(ave_chg, id.vars = "day")
ggplot(df_long, aes(x = day, y = value, colour = variable)) +
  geom_line(size = 1.25)  + scale_colour_grey() + 
  xlab("Horizon (days)") + ylab("Percent")+ 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman")) +
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) 

ggsave("../../figures/vix.png", width = 10, height = 6)
