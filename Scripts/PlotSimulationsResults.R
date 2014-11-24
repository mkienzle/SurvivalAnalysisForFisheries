dataset <- read.csv( file = "../Results/Simulations/CompareEstimates.csv")

with(dataset, table(sampling.type, Optimization.status))

library(ggplot2)

### natural mortality
p1 <- ggplot(aes(x= as.factor(round(Nat.Mort.sim,1)), y=Nat.Mort.est, fill = sampling.type),
             data = subset(dataset, Optimization.status == "successful")) + geom_boxplot() +
             facet_wrap(~n.sample.per.year, ncol = 3)

p1 <- p1 + labs(x = "Simulated natural mortality", y = "Estimated natural mortality")
p1 <- p1 + theme(panel.grid.minor = element_line(colour="white", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(-1, 1, 0.1), breaks = seq(-1,2,0.5))

postscript(file = "../Results/Graphics/Estimating-NaturalMortality.ps")
print(p1)
dev.off()

### catchability
p1 <- ggplot(aes(x= as.factor(round(1e4 * Catchability.sim)), y=1e4 * Catchability.est, fill = sampling.type),
             data = subset(dataset, Optimization.status == "successful")) + geom_boxplot() +
             facet_wrap(~n.sample.per.year, ncol = 3)

p1 <- p1 + labs(x = "Simulated catchability (x 1e-4)", y = "Estimated catchability (x 1e-4)")
p1 <- p1 + theme(panel.grid.minor = element_line(colour="white", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(1, 20, 1), breaks = seq(0,20,5))

postscript(file = "../Results/Graphics/Estimating-Catchability.ps")
print(p1)
dev.off()
