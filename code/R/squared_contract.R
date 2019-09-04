# script to create squared contract payoff figure
library(extrafont)
loadfonts()
theme_set(theme_bw(base_size = 20))
U <- 0 
K <- seq(U, 4, by = 0.1)
nK <- length(K)

nzero <- 6
con1 <- c(rep(0, nzero), seq(0.1, (nK - nzero) * 0.1, by = 0.1))
nzero <- nzero + 10
con2 <- c(rep(0, nzero), seq(0.1, (nK - nzero) * 0.1, by = 0.1))
nzero <- nzero + 10
con3 <- c(rep(0, nzero), seq(0.1, (nK - nzero) * 0.1, by = 0.1))
nzero <- nzero + 10
con4 <- c(rep(0, nzero), seq(0.1, (nK - nzero) * 0.1, by = 0.1))
nzero <- nzero + 10
con5 <- c(rep(0, nzero), seq(0.1, (nK - nzero) * 0.1, by = 0.1))
nzero <- nzero + 10
con6 <- c(rep(0, nzero), seq(0.1, (nK - nzero) * 0.1, by = 0.1))
k2 <- K^2

payoffs <- cbind(2 * con1, 2 * con2, 2 * con3, 2 * con4)
payoffs <- cbind(payoffs, rowSums(payoffs))
payoff.df <- data.frame(K = K, payoffs, k2)
plot_df <- reshape2::melt(payoff.df, id.vars = "K")


ggplot(plot_df, aes(x = K, y = value, colour = variable, linetype = variable)) +
  geom_line(size = 1.25)  + 
  scale_linetype_manual(values = c(rep("dotted", 4), "dashed", "solid")) + 
  xlab("Strike") + ylab("Payoff")+ 
  theme(legend.position = "none", text = element_text(size = 20, family = "Times New Roman")) 
ggsave("../../figures/squared_contract.png", height = 8, width = 10)
