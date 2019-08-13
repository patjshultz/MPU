#===============================================================================
# Script that investigates behavior of MPU around FOMC announcement days
#===============================================================================

# housekeeping
rm(list = ls())
library(tidyverse)
library(reshape2)
source("functions.R")
loadfonts(device = "win")

theme_set(theme_bw(base_size = 20))

fomc_dates <- read.csv("../../data/fomc_announcement.csv", stringsAsFactors = F)
fomc_dates <- as.Date(fomc_dates$date, format = "%m/%d/%Y")
n_meetings <- length(fomc_dates)

mpu <- read_csv("../../data/tau_mpu.csv")
mpu_dates <- mpu$date


#===============================================================================
# For each FOMC date find the ten trading dates before and after
# First calculate the relative levels of uncertainty around FOMC days
#===============================================================================
mpu_data <- mpu
fomc_dates <- fomc_dates
mpu_change_list <- list()
tau <- 0:15

for (i in 1:length(tau)) {
  mpu_change_list[[i]] <- calc_mpu_change(mpu_data = mpu, fomc_dates = fomc_dates, tau_delta = tau[i])
}

names(mpu_change_list) <- tau



ave_chg <- Reduce(rbind, lapply(mpu_change_list, get_col_mean))
ave_chg <- data.frame(day = tau, ave_chg)

colnames(ave_chg) <- c("day", colnames(mpu)[-1])
df_long <- melt(ave_chg, id.vars = "day")
ggplot(df_long, aes(x = day, y = value, colour = variable)) +
  geom_line(size = 1.25)  + 
  xlab("") + ylab("Percent")+ 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman")) +
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  ylim(-0.01, 0.003)
 
ggsave("../../figures/mpu_diff_fomc.png", width = 10, height = 6)

#===============================================================================
# Bar plot for each announcement in sample
#===============================================================================
mpu_change <- mpu_change_list$`1`
mpu_change$year <- lubridate::year(mpu_change$date)
index_u <- !duplicated(mpu_change$year)
at_tick <- which(index_u)
labels <- mpu_change$year[index_u]

#------------------------------------
# box plot for tau = [1, 1.5, 2, 2.5]
#------------------------------------
par(mfrow = c(2,2))
par(mar = c(2.5, 5.4, 2.5, 1))
par(ps = 12.5)

bp <- barplot(mpu_change$`1`, las = 2, ylab = "Percent", main = "tau = 1")
axis(side = 1, at = bp[at_tick] , labels = labels)
box()

bp <- barplot(mpu_change$`1.5`, las = 2, main = "tau = 2")
axis(side = 1, at = bp[at_tick] , labels = labels)
box()

bp <- barplot(mpu_change$`2`, las = 2, main = "tau = 3")
axis(side = 1, at = bp[at_tick] , labels = labels)
box()

barplot(mpu_change$`2.5`, las = 2, main = "tau = 4")
axis(side = 1, at = bp[at_tick] , labels = labels)
box()


