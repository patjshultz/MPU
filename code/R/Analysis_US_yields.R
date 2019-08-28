#===============================================================================
# Regression Analysis of US yields and term premium
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

# ed second moments
mpu <- read_csv("../../data/tau_mpu.csv")

# ed levels
ed <- read_csv("../../data/ED_generic.csv")
ed[, -1] <- 100 - ed[, -1] # convert to yield space

fomc_dates <- read_csv("../../data/fomc_announcement.csv")
fomc_dates <- as.Date(fomc_dates$date, format = "%m/%d/%Y")

n.meetings <- length(fomc_dates)

# yield_curve data
yields <- read_csv("../../data/US_yields.csv")
tp <- read_csv("../../data/term_premium.csv")

# daily changes in data
diff_list <- list()
diff_list$mpu_diff <- calc_diff_ts(mpu)
diff_list$yields_diff <- calc_diff_ts(yields)
diff_list$ed_diff <- calc_diff_ts(ed)
diff_list$tp_diff <- calc_diff_ts(tp)


#===============================================================================
# Analyze the relationship between the term premium and mpu controling for level shocks on all trading days
#===============================================================================

# merge mpu diff and yields diff dataframes
all_data <- Reduce(f = list_merge, diff_list)
yields_merged <- all_data[, which(colnames(all_data) %in% colnames(yields))]
mpu_merged <- all_data[, which(colnames(all_data) %in% colnames(mpu))]
ed_merged <- all_data[, which(colnames(all_data) %in% colnames(ed))]
tp_merged <- all_data[, which(colnames(all_data) %in% colnames(tp))]

# run regressions on mpu and term premium

#---------------
# one year plot
#---------------
responses_1 <- get_term_premium_regression(x = mpu_merged$`1`, 
                                           y = tp_merged[, -1], 
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
                                             y = tp_merged[, -1], 
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
                                           y = tp_merged[, -1], 
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
                                             y = tp_merged[, -1], 
                                             control = ed_merged$`ED10 Comdty`)

p4 <- ggplot(responses_2.5, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_2.5, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 2.5)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))


p.all <- gridExtra::grid.arrange(p1, p2, p3, p4)
ggsave(filename = "../../figures/tp_responses_US.png", plot = p.all)

p1
ggsave(filename = "../../figures/tp_responses_US1.png", plot = p.all)



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
ggsave(filename = "../../figures/yield_responses_US_ed_shock.png", plot = p.all)

p1
ggsave(filename = "../../figures/yield_responses_US1_ed_shock.png", plot = p1)



#===============================================================================
# Analyze the relationship between the yields and eds on fomc days
#===============================================================================

all_data <- all_data[which(all_data$date %in% fomc_dates), ]
yields_merged <- all_data[, which(colnames(all_data) %in% colnames(yields))]
mpu_merged <- all_data[, which(colnames(all_data) %in% colnames(mpu))]
ed_merged <- all_data[, which(colnames(all_data) %in% colnames(ed))]
tp_merged <- all_data[, which(colnames(all_data) %in% colnames(tp))]

# run regressions on mpu and yields

#---------------
# one year plot
#---------------
responses_1_fomc <- get_term_premium_regression(x = ed_merged$`ED4 Comdty`,
                                                y = yields_merged[,-1],
                                                control = ed_merged$`ED4 Comdty`, 
                                                include.control = F)


p1 <- ggplot(responses_1_fomc, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_1_fomc, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 1)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

#---------------
# 1.5 year plot
#---------------
responses_1.5_fomc <- get_term_premium_regression(x = ed_merged$`ED6 Comdty`,
                                                y = yields_merged[,-1],
                                                control = ed_merged$`ED4 Comdty`, 
                                                include.control = F)


p2 <- ggplot(responses_1.5_fomc, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_1.5_fomc, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 1)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

#---------------
# 2 year plot
#---------------
responses_2_fomc <- get_term_premium_regression(x = ed_merged$`ED8 Comdty`,
                                                  y = yields_merged[,-1],
                                                  control = ed_merged$`ED4 Comdty`, 
                                                  include.control = F)


p3 <- ggplot(responses_2_fomc, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_2_fomc, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 1)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

#---------------
# 2.5 year plot
#---------------
responses_2.5_fomc <- get_term_premium_regression(x = ed_merged$`ED10 Comdty`,
                                                y = yields_merged[,-1],
                                                control = ed_merged$`ED4 Comdty`, 
                                                include.control = F)


p4 <- ggplot(responses_2.5_fomc, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_2.5_fomc, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 1)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))


p.all <- gridExtra::grid.arrange(p1, p2, p3, p4)
ggsave(filename = "../../figures/yield_responses_EDS_fomc_US.png", plot = p.all)

p1
ggsave(filename = "../../figures/yield_responses_EDS_fomc_US1.png", plot = p1)


#==============================================================================================================================================================
#==============================================================================================================================================================
# Analyze the relationship between the term premium and mpu controling for level shocks on FOMC days
#==============================================================================================================================================================
#==============================================================================================================================================================

all_data <- Reduce(f = list_merge, diff_list)
all_data <- all_data[which(all_data$date %in% fomc_dates), ]
yields_merged <- all_data[, which(colnames(all_data) %in% colnames(yields))]
mpu_merged <- all_data[, which(colnames(all_data) %in% colnames(mpu))]
ed_merged <- all_data[, which(colnames(all_data) %in% colnames(ed))]
tp_merged <- all_data[, which(colnames(all_data) %in% colnames(tp))]

# run regressions on mpu and term premium

#---------------
# one year plot
#---------------
responses_1 <- get_term_premium_regression(x = mpu_merged$`1`, 
                                           y = tp_merged[, -1], 
                                           control = ed_merged$`ED4 Comdty`)

p1 <-ggplot(responses_1, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_1, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 1)") + 
  geom_vline(xintercept = 1, linetype = "dashed") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

#---------------
# 1.5 year plot
#---------------
responses_1.5 <- get_term_premium_regression(x = mpu_merged$`1.5`, 
                                             y = tp_merged[, -1], 
                                             control = ed_merged$`ED6 Comdty`)

p2 <- ggplot(responses_1.5, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_1.5, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 1.5)") + 
  geom_vline(xintercept = 1.5, linetype = "dashed") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

#---------------
# 2 year plot
#---------------
responses_2 <- get_term_premium_regression(x = mpu_merged$`2`, 
                                           y = tp_merged[, -1], 
                                           control = ed_merged$`ED8 Comdty`)

p3 <- ggplot(responses_2, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_2, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 2)") + 
  geom_vline(xintercept = 2, linetype = "dashed") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

#---------------
# 2.5 year plot
#---------------
responses_2.5 <- get_term_premium_regression(x = mpu_merged$`2.5`, 
                                             y = tp_merged[, -1], 
                                             control = ed_merged$`ED10 Comdty`)

p4 <- ggplot(responses_2.5, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_2.5, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 2.5)") + 
  geom_vline(xintercept = 2.5, linetype = "dashed") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))


p.all <- gridExtra::grid.arrange(p1, p2, p3, p4)
ggsave(filename = "../../figures/tp_responses_US_fomc.png", plot = p.all)



#---------------
# slope plot
#---------------
responses_1 <- get_term_premium_regression(x =  mpu_merged$`2.5` - mpu_merged$`1`, 
                                           y = tp_merged[, -1], 
                                           control = ed_merged$`ED4 Comdty`)

p.slope <-ggplot(responses_1, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_1, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 1)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

ggsave(filename = "../../figures/tp_responses_US_fomc_slope.png", plot = p.slope)


#===============================================================================
# run regressions on mpu and yields, controlling for ED shocks
#===============================================================================

#---------------
# one year plot
#---------------
responses_1 <- get_term_premium_regression(x = mpu_merged$`1`, 
                                           y = yields_merged[, -1], 
                                           control = ed_merged$`ED4 Comdty`, 
                                           include.control = T)
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
                                             control = ed_merged$`ED6 Comdty`, 
                                             include.control = T)

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
                                           control = ed_merged$`ED8 Comdty`, 
                                           include.control = T)

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
                                             control = ed_merged$`ED10 Comdty`, 
                                             include.control = T)

p4 <- ggplot(responses_2.5, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  geom_ribbon(data=responses_2.5, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta (tau = 2.5)") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))


p.all <- gridExtra::grid.arrange(p1, p2, p3, p4)
ggsave(filename = "../../figures/yield_responses_US_fomc.png", plot = p.all)


#---------------
# one year plot
#---------------
responses_control <- get_term_premium_regression(x = mpu_merged$`2.5`, 
                                           y = yields_merged[, -1], 
                                           control = ed_merged$`ED8 Comdty`, 
                                           include.control = T)
p.control <-ggplot(responses_control, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  ylim(-0.6, 2.2) + 
  geom_ribbon(data=responses_control, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

response_no_control <- get_term_premium_regression(x = mpu_merged$`2.5`, 
                                                   y = yields_merged[, -1], 
                                                   control = ed_merged$`ED8 Comdty`, 
                                                   include.control = F)


p.no.control <-ggplot(response_no_control, aes(x = Horizon, y = Beta)) +
  geom_line(aes(y = CILB)) + 
  geom_line(aes(y = CIUB)) + 
  geom_line(size = 1.25)  + 
  ylim(-0.6, 2.2) + 
  geom_ribbon(data=response_no_control, aes(ymin=CILB, ymax=CIUB), fill="grey", alpha="0.25") + 
  xlab("Horizon") + ylab("Beta") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

p.all <- gridExtra::grid.arrange(p.no.control, p.control, nrow= 1, ncol = 2)


ggsave(filename = "../../figures/yield_responses_US_fomc_control_v_no_control.png", plot = p.all)



