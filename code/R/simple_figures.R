# ==============================================================================
# Script that just generates the LIBOR vs. fed funds rates figure
# ==============================================================================

rm(list = ls())
library(ggplot2)
library(extrafont)
loadfonts()
theme_set(theme_bw(base_size = 20))

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

eurodollar_data <- read_csv("../../data/ED_generic.csv")
eurodollar_data[, -1] <- 100 - eurodollar_data[, -1]

plot_data <- data.frame(date = eurodollar_data$date, 
                        six.month = eurodollar_data$`ED2 Comdty`,
                        twelve.month = eurodollar_data$`ED4 Comdty`, 
                        eighteen.month = eurodollar_data$`ED6 Comdty`,
                        twenty.four.month = eurodollar_data$`ED8 Comdty`,
                        thirty.month = eurodollar_data$`ED10 Comdty`)
df_long <- reshape2::melt(plot_data, id.vars = "date")
ggplot(df_long, aes(x = date, y = value, colour = variable)) + 
  geom_line(size = 1.25) + 
  xlab("") + ylab("Percent") + 
  theme(legend.title = element_blank(), text = element_text(size = 20, family = "Times New Roman"))

#-------------------------------------------------------------------------------
# Trading volume of major markets per https://www.wsj.com/articles/how-etfs-swallowed-the-stock-market-11567162801?mod=cxrecs_join#cxrecs_s
#-------------------------------------------------------------------------------


#Inserts newlines into strings every N interval
new_lines_adder = function(test.string, interval) {
  #split at spaces
  string.split = strsplit(test.string," ")[[1]]
  # get length of snippets, add one for space
  lens <- nchar(string.split) + 1
  # now the trick: split the text into lines with
  # length of at most interval + 1 (including the spaces)
  lines <- cumsum(lens) %/% (interval + 1)
  # construct the lines
  test.lines <- tapply(string.split,lines,function(line)
    paste0(paste(line,collapse=" "),"\n"),simplify = TRUE)
  # put everything into a single string
  result <- paste(test.lines,collapse="")
  return(result)
}


#a user-level wrapper that also works on character vectors, data.frames, matrices and factors
add_newlines = function(x, interval) {
  if (class(x) == "data.frame" | class(x) == "matrix" | class(x) == "factor") {
    x = as.vector(x)
  }
  
  if (length(x) == 1) {
    return(new_lines_adder(x, interval))
  } else {
    t = sapply(x, FUN = new_lines_adder, interval = interval) #apply splitter to each
    names(t) = NULL #remove names
    return(t)
  }
}


volume <- data.frame(FX = 1306,
                     EDF = 765,
                     FFF = 327, 
                     Equity.F = 178,
                     Global.Stocks = 143, 
                     U.S.Treasury = 120, 
                     U.S.stocks = 90, 
                     SP500.futures.etfs = 71)
X <- data.frame(market = c("Foreign Exchange", "Eurodollar Futures", "Fed Fund Futures", "Equity Futures", "Global Stocks", "US Treasuries", "US Stocks", "S&P500 Futures/ETFS"), 
                volume = as.numeric(volume[1, ]))
X$market <- factor(X$market, levels = X$market[order(-X$volume)])

ggplot(data = X, aes(x = market, y = volume)) + geom_bar(stat = "identity") + 
  ylab("Trillions ($)") + xlab("") +
  scale_x_discrete(labels = add_newlines(X$market, 11), name = "") + 
  ggtitle("Annual volume by value (2018)") + 
  theme(text = element_text(size = 20, family = "Times New Roman")) 

ggsave("../../figures/volumes.png", width = 10, height = 8)  
