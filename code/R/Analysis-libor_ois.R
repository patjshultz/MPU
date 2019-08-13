rm(list = ls())
library(tidyverse)
theme_set(theme_bw(base_size = 20))


data <- read_csv("../../data/libor_ois_spread.csv")
data$date <- as.Date(data$date, format = "%m/%d/%Y")

data_long <- reshape2::melt(data, id.vars = "date")
data_spread <- data_long[which(data_long$variable == "Spread"), ]

ggplot(data_spread, aes(x = date, y = value, colour = "black")) + 
  geom_line(size = 1.25, colour = "black") +  xlab("") + ylab("Percent")+ 
  theme(legend.position = "none", text = element_text(size = 23, family = "Times New Roman"))
ggsave("../../write_up/libor_ois.png", width = 10, height = 6)

mean(data$Spread)
sd(data$Spread)
