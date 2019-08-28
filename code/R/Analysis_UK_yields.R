#===============================================================================
# Regression Analysis of international yields
#===============================================================================
rm(list = ls())
library(tidyverse)
library(extrafont)
library(readxl)
source("functions.R")
#font_import()
loadfonts(device = "win")
theme_set(theme_bw(base_size = 20))

#===============================================================================
# Read in relevant data
#===============================================================================

mpu <- read_csv("../../data/tau_mpu.csv")

# ed levels
ed <- read_csv("../../data/ED_generic.csv")
ed[, -1] <- 100 - ed[, -1] # convert to yield space

fomc_dates <- read_csv("../../data/fomc_announcement.csv")
fomc_dates <- as.Date(fomc_dates$date, format = "%m/%d/%Y")

n.meetings <- length(fomc_dates)

# bank of canada yield curve data
yields <- read_csv("../../data/UK_yield_curve.csv")

# daily changes in data
diff_list <- list()
diff_list$mpu_diff <- calc_diff_ts(mpu)
diff_list$yields_diff <- calc_diff_ts(yields)
diff_list$ed_diff <- calc_diff_ts(ed)

#===============================================================================
# Analyze the relationship between the yields and mpu
#===============================================================================

# merge mpu diff and yields diff dataframes
all_data <- Reduce(f = list_merge, diff_list)
all_data <- all_data[which(all_data$date %in% fomc_dates), ]

yields_merged <- all_data[, which(colnames(all_data) %in% colnames(yields))]
mpu_merged <- all_data[, which(colnames(all_data) %in% colnames(mpu))]
ed_merged <- all_data[, which(colnames(all_data) %in% colnames(ed))]

# run regressions on mpu and term premium
#---------------
# one year plot
#---------------
responses_1 <- get_term_premium_regression(x = mpu_merged$`1`, 
                                           y = yields_merged[, -1], 
                                           control = ed_merged$`ED4 Comdty`)

p1 <-ggplot(responses_1, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_1, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 1)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

#---------------
# 1.5 year plot
#---------------
responses_1.5 <- get_term_premium_regression(x = mpu_merged$`1.5`, 
                                             y = yields_merged[, -1], 
                                             control = ed_merged$`ED6 Comdty`)

p2 <- ggplot(responses_1.5, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_1.5, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 1.5)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

#---------------
# 2 year plot
#---------------
responses_2 <- get_term_premium_regression(x = mpu_merged$`2`, 
                                           y = yields_merged[, -1], 
                                           control = ed_merged$`ED8 Comdty`)

p3 <- ggplot(responses_2, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_2, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 2)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

#---------------
# 2.5 year plot
#---------------
responses_2.5 <- get_term_premium_regression(x = mpu_merged$`2.5`, 
                                             y = yields_merged[, -1], 
                                             control = ed_merged$`ED10 Comdty`)

p4 <- ggplot(responses_2.5, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_2.5, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 2.5)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))


p.all <- gridExtra::grid.arrange(p1, p2, p3, p4)
ggsave(filename = "../../figures/yield_responses_UK.png", plot = p.all)

p1
ggsave(filename = "../../figures/yield_responses_UK1.png", plot = p1)

# ==============================================================================
# responses to levels shocks to ed contracts 
# ==============================================================================
# run regressions on mpu and term premium
#---------------
# one year plot
#---------------
responses_1 <- get_term_premium_regression(x = ed_merged$`ED4 Comdty`, 
                                           y = yields_merged[, -1], 
                                           control = ed_merged$`ED4 Comdty`, 
                                           include.control = F)

p1 <-ggplot(responses_1, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_1, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 1)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

#---------------
# 1.5 year plot
#---------------
responses_1.5 <- get_term_premium_regression(x = ed_merged$`ED6 Comdty`, 
                                             y = yields_merged[, -1], 
                                             control = ed_merged$`ED6 Comdty`, 
                                             include.control = F)

p2 <- ggplot(responses_1.5, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_1.5, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 1.5)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

#---------------
# 2 year plot
#---------------
responses_2 <- get_term_premium_regression(x = ed_merged$`ED8 Comdty`, 
                                           y = yields_merged[, -1], 
                                           control = ed_merged$`ED8 Comdty`, 
                                           include.control = F)

p3 <- ggplot(responses_2, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_2, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 2)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

#---------------
# 2.5 year plot
#---------------
responses_2.5 <- get_term_premium_regression(x = ed_merged$`ED10 Comdty`, 
                                             y = yields_merged[, -1], 
                                             control = ed_merged$`ED10 Comdty`, 
                                             include.control = F)

p4 <- ggplot(responses_2.5, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_2.5, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 2.5)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))


p.all <- gridExtra::grid.arrange(p1, p2, p3, p4)
ggsave(filename = "../../figures/yield_responses_UK_ed_shock.png", plot = p.all)

p1
ggsave(filename = "../../figures/yield_responses_UK1_ed_shock.png", plot = p1)


  
