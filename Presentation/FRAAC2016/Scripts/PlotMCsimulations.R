# CREATED  25 May 2015

# PURPOSE plot results of Monte Carlo simulations

# load data
MC.result <- read.csv("../Results/Simulations/Archive/CompareEstimates-Sat-May-23-13:13:20-2015.csv")
library(ggplot2)

# Simple scatter plot
postscript(file = "../Results/Graphics/ComparisonBetweenCSandSAmctestScatterPlot4Presentation.ps")
#par(fg = "black", col.main = "black", col.axis = "black", col.lab = "black", cex = 1.5, cex.lab = 1.8, cex.axis = 1.5, cex.main = 2.5, mai =
# c(1.5, 1.8, 1.02, 0.2), mgp = c(3.8, 1, 0))
#with(MC.result,
#plot( Z.sim, Z.est, pch = 19, col = ifelse(Estimator.type == "cross sectional", "lightgrey", "red"),
#      xlab = "Simulated total mortality (age-group 4-9, 1/year)", ylab = "Estimated total mortality", las = 1)) 

#legend(0.6, 1.4, pch = 19, col = c("lightgrey", "red"), legend = c("cross sectional", "survival analysis"))
#abline(0,1, col = "blue", lwd = 2)
p0 <-ggplot(aes(x= Z.sim, y=Z.est), data = MC.result) + geom_point(aes(colour = Estimator.type), alpha = 1)
p0 <- p0 + geom_abline(intercept = 0, slope = 1, colour = "black", size = 1)
p0 <- p0 + scale_x_continuous(name = "Simulated (1/year)",minor_breaks = seq(-1, 2, 0.1), breaks = seq(-1,2,0.2)) + scale_y_continuous(name = "Estimated (1/year)",minor_breaks = seq(-1, 2, 0.1), breaks = seq(-1,2,0.2))
p0 <- p0 + theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"), legend.text=element_text(size=18))
p0 <- p0 + labs(colour="Estimator type") + theme(legend.title=element_text(size=18))
print(p0)

dev.off()

# Plot cross sectional results on their own
p1 <- ggplot(aes(x= Z.sim, y=Z.est),
             data = subset(MC.result, Estimator.type %in% c("cross sectional"))) 
p1 <- p1 + geom_boxplot(aes(x= as.factor(round(Z.sim,1)), y=Z.est), alpha = 0.6)
p1 <- p1 + scale_x_discrete(name = "Simulated (1/year)") + scale_y_continuous(minor_breaks = seq(-1, 2, 0.1), breaks = seq(-1,2,0.2), name = "Estimated (1/year)")

d2 <- data.frame(x2 = seq(0.6,1.3,0.1))
p1 <- p1 + geom_line(data = d2, aes(x = as.numeric(ordered(x2)), y =x2), colour = "blue", size = 2)
p1 <- p1 + geom_boxplot(aes(x= as.factor(round(Z.sim,1)), y=Z.est), alpha = 0.6)
p1 <- p1 + theme(axis.text=element_text(size=12), axis.title=element_text(size=18,face="bold"), legend.text=element_text(size=18))
p1 <- p1 + theme(legend.title=element_text(size=18))

#print(p1)
postscript(file = "../Results/Graphics/MCtestCrossSectional4Presentation.ps")
print(p1)
dev.off()

# Plot survival analysis results on their own
#p1.1 <- ggplot(aes(x= Z.sim, y=Z.est),
p1.1 <- ggplot(aes(x= as.factor(round(10*(Z.sim +0.05),0)/10 -0.05), y=Z.est),
             data = subset(MC.result, Estimator.type %in% c("survival analysis") & Z.sim > 0.6 & Z.sim < 1.3)) 
#p1.1 <- p1.1 + geom_boxplot(aes(x= as.factor(round(10*(Z.sim +0.05),0)/10 +0.05), y=Z.est), alpha = 0.6)
p1.1 <- p1.1 + geom_boxplot()
p1.1 <- p1.1 + scale_x_discrete(name = "Simulated (1/year)") + scale_y_continuous(minor_breaks = seq(-0.95, 2, 0.05), breaks = seq(-1,2,0.1), name = "Estimated (1/year)")

d2 <- data.frame(x2 = seq(0.65,1.35,0.1))
p1.1 <- p1.1 + geom_line(data = d2, aes(x = as.numeric(ordered(x2)), y =x2), colour = "blue", size = 2)
#p1.1 <- p1.1 + geom_boxplot(aes(x= as.factor(round(Z.sim,1)), y=Z.est), alpha = 0.6)
#p1.1 <- p1.1 + geom_boxplot(aes(x= as.factor(round(10*(Z.sim - 0.05),0)/10 +0.05), y=Z.est))

#print(p1.1)
postscript(file = "../Results/Graphics/MCtestSurvivalAnalysis4Presentation.ps")
par(fg = "black", col.main = "black", col.axis = "black", col.lab = "black", cex = 1.5, cex.lab = 1.8, cex.axis = 1.5, cex.main = 2.5, mai = c(1.5, 1.8, 1.02, 0.2), mgp = c(3.8, 1, 0))
print(p1.1)
dev.off()

# Plot comparison between "cross sectional" and survival analysis
p2 <- ggplot(aes(x= as.factor(round(Z.sim,1)), y=Z.est, fill = Estimator.type),
             data = MC.result) + geom_boxplot()
p2 <- p2 + scale_x_discrete(name = "Simulated (1/year)") + scale_y_continuous(minor_breaks = seq(-1, 2, 0.1), breaks = seq(-1,2,0.2), name = "Estimated (1/year)")
#print(p2)

postscript(file = "../Results/Graphics/ComparisonBetweenCSandSAmctest4Presentation.ps")
par(fg = "black", col.main = "black", col.axis = "black", col.lab = "black", cex = 1.5, cex.lab = 1.8, cex.axis = 1.5, cex.main = 2.5, mai = c(1.5, 1.8, 1.02, 0.2), mgp = c(3.8, 1, 0))
print(p2)
dev.off()




