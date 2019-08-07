#===============================================================================
# script to compare mpu measure and libor ois spread
#===============================================================================
rm(list = ls())
library(tidyverse)
library(reshape2)
theme_set(theme_bw(base_size = 20))

# libor
libor_ois <- read_csv("../../data/libor_ois_spread.csv")
libor_ois$date <- as.Date(libor_ois$date, format = "%m/%d/%Y")
libor_ois_long <- melt(data = libor_ois, id.vars = "date")
spread <- libor_ois_long[which(libor_ois_long$variable == "Spread"), ]

# mpu
mpu <- read_csv("../../data/mpu.csv")

# shared dates
dates <- intersect(libor_ois$date, mpu$date)

# merge to shared date vector
libor_ois <- libor_ois[which(libor_ois$date %in% dates), ]
mpu <- mpu[which(mpu$date %in% dates), ]
df <- merge(x = libor_ois, y = mpu, by = "date")

# corrlation between spread and mpu
cor(df$Spread, df$`2`, use = "complete.obs")


# plot spread
ggplot(spread, aes(x = date, y = value)) + 
  geom_line(size = 1.25) + 
  ylab("LIBOR-OIS Spread") +
  theme(legend.title = element_blank())
ggsave("../../figures/libor_ois_spread.png",  width = 10, height = 6)
