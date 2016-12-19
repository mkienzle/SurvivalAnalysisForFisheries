# CREATED 22 May 2015

# PURPOSE test the capacity of cross sectional analysis to estimate total mortality (Z) for a range of age-groups

rm(list = ls())
source("../../../Scripts/UsefulFunctions.R")

MC.result <- data.frame(Z.sim = NA, "Estimator.type" = NA, Z.est = NA)

for(sim.nb in 1:20){

nb.of.cohort <- 30
max.age <- 16
population <- GenerateData3(max.age = max.age, nb.of.cohort = nb.of.cohort, catchability.range = c(1.5,2.5), effort.range = c(2e3, 4e3), nat.mort.range = c(0.3, 0.36), recruitment.range = c(4e6, 8e6), log.para.range = c(7.5,8.5), log.parb.range=c(2,3))

# plot gear selectivity
# curve(logistic(population$logistic.par[1],population$logistic.par[2],x), from = 0, to = max.age)

n.sample.per.year <- 1e4
nb.at.age.sample <- draw.sample(population$catch, sample.size = n.sample.per.year * (nb.of.cohort + 1 - max.age))

n <- nrow(nb.at.age.sample)
p <- ncol(nb.at.age.sample)

sample.prop.at.age <- nb.at.age.sample / outer(rowSums(nb.at.age.sample), rep(1, p))
est.nb.at.age.in.catch <- outer(rowSums(population$catch), rep(1, p)) * sample.prop.at.age
est.nb.at.age.in.catch <- replace(est.nb.at.age.in.catch, est.nb.at.age.in.catch < 1, NA)

library(ggplot2)

tmp.data <- data.frame(Year = 1999 + rep(1:n, p), Age.Group = rep(1:p, each = n), Numbers.Harvested = c(est.nb.at.age.in.catch))

tmp.data <- subset(tmp.data, Year >= 2007)

p1 <- ggplot(tmp.data, aes(Age.Group, log(Numbers.Harvested))) + geom_point() + facet_wrap(~Year, nrow = 2)
p1 <- p1 + scale_x_continuous(name = "Age groups (year)") + scale_y_continuous(name = "Estimated nb at age in catch (log-scale)")
p1 <- p1 + geom_smooth(data = subset(tmp.data, Age.Group >= 4 & Age.Group <= 9), method = "lm", se = FALSE)
print(p1)

library(SAFR)
sim.Z <- population$M + Coaa2Caaa(population$F)
dimnames(sim.Z) <- dimnames(est.nb.at.age.in.catch)

#print(rowMeans(sim.Z[8:nrow(sim.Z),4:9]))

# Estimate Z according to cross-sectional analysis
all.years <- unique( subset(tmp.data, Age.Group >= 4 & Age.Group <= 9)$Year)
results <- data.frame(Year = all.years, Z.est = NA)

for(year in all.years){

#print(year)
x <- with(subset(tmp.data, Age.Group >= 4 & Age.Group <= 9 & Year == year), Age.Group)
y <- log(with(subset(tmp.data, Age.Group >= 4 & Age.Group <= 9 & Year == year), Numbers.Harvested))
my.lm <- lm(y~x)
results[which(results$Year == year), 2] <- -coef(my.lm)[2]
}

MC.result <- rbind(MC.result, cbind(Z.sim = rowMeans(sim.Z[8:nrow(sim.Z),4:9]), "Estimator.type" = "cross sectional", Z.est = results$Z.est))

#### Estimate mortality rates with survival analysis
source("UsefulFunction.R"); 
sa.tmp <- SurvivalAnalysis()

q.est <- sa.tmp[["optimization"]]$par[1] * sa.tmp[["optimization"]]$catchability.scaling.factor[1]

est.log.para <- sa.tmp[["optimization"]]$par[3]
est.log.parb <- sa.tmp[["optimization"]]$par[4]

selectivity.at.age <- logistic(est.log.para,est.log.parb,seq(1,max.age))

F.est <- q.est * population$effort * outer(rep(1, nrow(population$effort)), selectivity.at.age)
M.est <- sa.tmp[["optimization"]]$par[2]
Z.est <- F.est + M.est
rowMeans(Z.est[8:nrow(sim.Z),4:9])

print(sa.tmp$optimization.status)

ifelse(sa.tmp$optimization.status == "successful",
MC.result <- rbind(MC.result, cbind(Z.sim = rowMeans(sim.Z[8:nrow(sim.Z),4:9]), "Estimator.type" = "survival analysis", Z.est = rowMeans(Z.est[8:nrow(sim.Z),4:9]))), MC.result <- rbind(MC.result, cbind(Z.sim = rowMeans(sim.Z[8:nrow(sim.Z),4:9]), "Estimator.type" = "survival analysis", Z.est = rep(NA, length(8:nrow(sim.Z))))))

}

#plot(results, pch = 19, ylim = c(0, 1.8)); abline(h=2 * 0.33, col = "red", lwd = 1.5)
#points(results$Year, rowMeans(sim.Z[8:nrow(sim.Z),4:9]), pch = 18)

### Estimates of Z
MC.result$Z.sim <- as.numeric(MC.result$Z.sim)
MC.result$Z.est <- as.numeric(MC.result$Z.est)

p2 <- ggplot(aes(x= as.factor(round(Z.sim,1)), y=Z.est, fill = Estimator.type),
             data = MC.result[-1,]) + geom_boxplot()
p2 <- p2 + scale_y_continuous(minor_breaks = seq(-1, 2, 0.1), breaks = seq(-1,2,0.2), name = "Estimated (1/year)")
print(p2)

write.csv(file = "../Results/Simulations/CompareEstimates.csv", MC.result[-1,])
write.csv(file = paste("../Results/Simulations/Archive/CompareEstimates-", format(Sys.time(), "%a-%b-%d-%H:%M:%S-%Y"), ".csv", sep=""), MC.result[-1,])


# Alternative plot
with(MC.result[-1,], plot( Z.sim, Z.est, pch = 19, col = ifelse(Estimator.type == "cross sectional", "black", "red"))); abline(0,1)
