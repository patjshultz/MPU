rm(list = ls())
library(tidyverse)
library(reshape2)
theme_set(theme_bw(base_size = 20))


mpu <- read_csv("../../data/tau_mpu.csv")
term_premium <- read_csv("../../data/term_premium.csv")

dates <- as.Date(intersect(mpu$date, term_premium$date), origin = "1970-01-01")

mpu <- mpu[which(mpu$date %in% dates), ]
term_premium <- term_premium[which(term_premium$date %in% dates), ]



summary(mpu)

# short horizon data 
two_year <- data.frame(date = dates, mpu2 = mpu$`2`, tp2 = term_premium$ACMTP02)
two_year <- na.omit(two_year)
two_year_long <- melt(two_year, id.vars = "date")

ggplot(two_year_long, aes(x = date, y = value, colour = variable))  +
  geom_line(size = 1.25) + 
  xlab("") + ylab("Percent")+ 
  theme(legend.title = element_blank()) 

cor(two_year$mpu2, two_year$tp2)
