#dataset <- read.csv( file = "../Results/Simulations/CompareEstimates.csv")
dataset <- read.csv( file = "../Results/Simulations/Archive/CompareEstimates-Thu-Dec-11-03:52:10-2014.csv")

with(dataset, table(sampling.type, Optimization.status))

library(ggplot2)


dataset$rnms <- with(dataset, round(Nat.Mort.sim,1))
p0 <- ggplot(aes(x= as.factor(n.sample.per.year), y=Nat.Mort.est),
             data = subset(dataset, Optimization.status == "successful" & sampling.type == "Strategy 2 - weighted sample")) + 
             geom_boxplot() + scale_fill_grey() +
             facet_wrap(~rnms, ncol = 2)

p0 <- p0 + theme(panel.grid.minor = element_line(colour="white", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(0,1, 0.1), breaks = seq(0,1,0.1))
plot(p0)

postscript(file = "../Results/Graphics/Estimating-NaturalMortality2.ps")
print(p0)
dev.off()

### natural mortality
p1 <- ggplot(aes(x= as.factor(round(Nat.Mort.sim + 0.05,1)-0.05), y=Nat.Mort.est, fill = sampling.type),
             data = subset(dataset, Optimization.status == "successful")) + geom_boxplot() +
             facet_wrap(~n.sample.per.year, ncol = 2)

p1 <- p1 + labs(x = "Simulated natural mortality", y = "Estimated natural mortality")
p1 <- p1 + theme(panel.grid.minor = element_line(colour="white", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(-1.05, 1.05, 0.1), breaks = seq(0.15,0.75,0.2))

postscript(file = "../Results/Graphics/Estimating-NaturalMortality.ps")
print(p1)
dev.off()

postscript(file = "../Results/Graphics/Estimating-NaturalMortality4Presentation.ps")
p2 <- p1 + scale_fill_grey(start = 0.4, end = 0.9) + theme(strip.text.x = element_text(size = 14), legend.text=element_text(size=16), text = element_text(size=16))
print(p2)
dev.off()

postscript(file = "../Results/Graphics/Estimating-NaturalMortality4PresentationColour.ps")
p2 <- p1 + theme(strip.text.x = element_text(size = 14), legend.text=element_text(size=16), text = element_text(size=16))
print(p2)
dev.off()


### catchability
p1 <- ggplot(aes(x= as.factor(round(1e4 * Catchability.sim +0.5)-0.5), y=1e4 * Catchability.est, fill = sampling.type),
             data = subset(dataset, Optimization.status == "successful")) + geom_boxplot() + 
             facet_wrap(~n.sample.per.year, ncol = 2)

p1 <- p1 + labs(x = expression(paste("Simulated catchability (x", 10^-4, ")")), y = expression(paste("Estimated catchability (x", 10^-4, ")")) )
p1 <- p1 + theme(panel.grid.minor = element_line(colour="white", size=0.5)) +
    scale_y_continuous(minor_breaks = seq(0.5, 20.5, 1), breaks = seq(1.5, 20.5, 2))

postscript(file = "../Results/Graphics/Estimating-Catchability.ps")
print(p1)
dev.off()

postscript(file = "../Results/Graphics/Estimating-Catchability4Presentation.ps")
p2 <- p1 + scale_fill_grey(start = 0.4, end = 0.9) + theme(strip.text.x = element_text(size = 14), legend.text=element_text(size=16), text = element_text(size=16))
print(p2)
dev.off()

postscript(file = "../Results/Graphics/Estimating-Catchability4PresentationColour.ps")
p2 <- p1 + theme(strip.text.x = element_text(size = 14), legend.text=element_text(size=16), text = element_text(size=16))
print(p2)
dev.off()

### Compare negative log-likelihoods from survival analysis and Fournier & Archibald (1982)

p1 <- ggplot( aes(x=as.factor(n.sample.per.year), y = NegLL - FourAndArchiNLL),
              data = subset(dataset, Optimization.status == "successful")) + geom_boxplot() + scale_fill_grey(start = 0.4, end = 0.9) +
              facet_wrap(~sampling.type, ncol = 2)
p1 <- p1 + labs(x = "Number of sample per year", y = "Difference in neg LL (SA - ML)")

print(p1)
postscript(file = "../Results/Graphics/ComparisonOfNegLL.ps")
print(p1)
dev.off()

postscript(file = "../Results/Graphics/ComparisonOfNegLL4Presentation.ps")
p1 <- p1 + theme(strip.text.x = element_text(size = 14), legend.text=element_text(size=16), text = element_text(size=16))
print(p1)
dev.off()

