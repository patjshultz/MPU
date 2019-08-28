# ==============================================================================
# Script that just generates the LIBOR vs. fed funds rates figure
# ==============================================================================

rm(list = ls())
library(ggplot2)
library(extrafont)
loadfonts()

#---------------------
# read in data 
#--------------------

interest.rate.data <- readxl::read_xls("../../data/effr_libor.xls") 
df_long <- reshape2::melt(interest.rate.data, id.vars = "date")


ggplot(df_long, aes(x = date, y = value, colour = variable)) + 
  geom_line(size = 1.25) + 
  xlab("") + ylab("Percent") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

ggsave("../../figures/libor_fed_funds.png", width = 10, height = 6)

cor(interest.rate.data$FEDFUNDS, interest.rate.data$LIBOR)

#-------------------------------------------------------------------------------
# plot eurodollar data
#-------------------------------------------------------------------------------