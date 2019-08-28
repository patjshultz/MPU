#===============================================================================
# Regression Analysis around FOMC days
#===============================================================================
rm(list = ls())
library(tidyverse)
library(extrafont)
library(readxl)
source("functions.R")
#font_import()
#loadfonts(device = "win")
theme_set(theme_bw(base_size = 20))

#===============================================================================
# Read in relevant data
#===============================================================================

mpu <- read_csv("../../data/tau_mpu.csv")
fomc_dates <- read_csv("../../data/fomc_announcement.csv")
fomc_dates <- as.Date(fomc_dates$date, format = "%m/%d/%Y")

n.meetings <- length(fomc_dates)

# create a vector of event dates for QE, FG, TT
event.dates <- as.Date(c("2012-01-25", "2012-09-13", "2012-12-12", "2013-06-19",
                       "2014-12-17", "2015-03-18", "2015-09-17", "2015-10-28", 
                       "2015-12-16", "2017-03-15", "2018-01-31", "2018-06-13",
                       "2018-09-26", "2019-01-30"))

# term premium data
tp <- read_csv("../../data/term_premium.csv")

# generic eurodollar data
ed <- read_xlsx("../../data/ED_Generic_data.xlsx", na = "#N/A N/A", skip = 4)
colnames(ed) <- c("date", paste("ED", 1:19, sep = ""))
ed$date <- as.Date(ed$date)
ed[, -1] <- 100 - ed[, -1]

# daily changes in data

mpu_diff <- calc_diff_ts(mpu)
mpu_diff_announcement <- mpu_diff[which(mpu_diff$date %in% event.dates), ]

tp_diff <- calc_diff_ts(tp)
tp_diff_announcement <- tp_diff[which(tp_diff$date %in% event.dates), ]

ed_diff <- 

#===============================================================================
# Analyze the relationship between the term premium and mpu
#===============================================================================

# merge mpu diff and tp diff dataframes
all_data <- merge(mpu_diff, tp_diff, by = "date")

# run regressions on mpu and term premium

#---------------
# one year plot
#---------------
responses_1 <- get_term_premium_regression(mpu_tau = all_data$`1`, 
                                         term_premium = all_data[, 7:16])

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
responses_1.5 <- get_term_premium_regression(mpu_tau = all_data$`1.5`, 
                                         term_premium = all_data[, 7:16])

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
responses_2 <- get_term_premium_regression(mpu_tau = all_data$`2`, 
                                             term_premium = all_data[, 7:16])

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
responses_2.5 <- get_term_premium_regression(mpu_tau = all_data$`2.5`, 
                                             term_premium = all_data[, 7:16])

p4 <- ggplot(responses_2.5, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_2.5, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 2.5)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))


p.all <- gridExtra::grid.arrange(p1, p2, p3, p4)
ggsave(filename = "../../figures/term_premium_responses.png", plot = p.all)



#===============================================================================
# Calculate the change in MPU each day and the associated term premium beta
#===============================================================================


# focus on 2.5 year maturity first
event_table <- data.frame(dates = event.dates)
event_table$mpu_diff <- mpu_diff_announcement$`2.5`
event_table$tp5 <- tp_diff_announcement$ACMTP05
event_table$tp10 <- tp_diff_announcement$ACMTP10


all_data <- all_data[which(all_data$date %in% fomc_dates), ]

#---------------
# one year plot
#---------------
responses_1 <- get_term_premium_regression(mpu_tau = all_data$`1`, 
                                           term_premium = all_data[, 7:16])

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
responses_1.5 <- get_term_premium_regression(mpu_tau = all_data$`1.5`, 
                                             term_premium = all_data[, 7:16])

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
responses_2 <- get_term_premium_regression(mpu_tau = all_data$`2`, 
                                           term_premium = all_data[, 7:16])

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
responses_2.5 <- get_term_premium_regression(mpu_tau = all_data$`2.5`, 
                                             term_premium = all_data[, 7:16])

p4 <- ggplot(responses_2.5, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_2.5, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 2.5)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))


p.all <- gridExtra::grid.arrange(p1, p2, p3, p4)
ggsave(filename = "../../figures/term_premium_responses_fomc.png", plot = p.all)



