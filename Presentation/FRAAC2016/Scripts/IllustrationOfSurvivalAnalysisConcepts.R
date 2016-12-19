#CREATED   9 Feb 2015
#MODIFIED  9 Feb 2015

#PURPOSE Illustrate hazard, cumulative hazard and probability distribution function

postscript(file = "../Results/Graphics/IllustrationOfSurivalAnalysisConcepts.ps")
par(mfrow=c(3,3), cex.lab = 1.2, cex.main = 1.5)

### simplest case
x <- seq(0,10)
Z <- 0.5
plot(x, rep(Z, length(x)), ylab = "Hazard function", main = "mortality rate (1/year)", xlab = "t", type = "l", las = 1)
plot(x, Z*x, ylab = "Cumulative hazard fct", main = "mortality (no unit)", xlab = "t", type = "l", las = 1)
plot(x, exp(-Z*x), ylab = "Survivor fct", main = "Probability of \nsurviving up to t", xlab = "t", type = "l", las = 1)

### a bit more complicate
x <- seq(0,10, length=1e3)
M <- 0.3
Z <- M + 0.8
rec.age <- 2

library(SAFR)
my.Z <- ifelse(x < rec.age, M, Z) #c(rep(M, rec.age), rep(Z, max(x) - rec.age))
cum.Z <- cumsum(my.Z)

curve(ifelse(x < rec.age, M, Z), from = 0, to = max(x),  ylab = "Hazard function", main = "mortality rate (1/year)", xlab = "t", type = "l", las = 1, ylim = c(0,1.5))
curve(cumsum(ifelse(x < rec.age, M, Z)), from = 0, to = max(x), ylab = "Cumulative hazard fct", main = "mortality (no unit)", xlab = "t", type = "l", las = 1, n = 1e4)

plot(x, exp(- (cum.Z - my.Z) / (length(x)/max(x))), ylab = "Survivor fct", main = "Probability of \nsurviving up to t", xlab = "t", type = "l", las = 1, xlim = c(0, 6) )

### even more complicate
x <- seq(0,10, length=1e3)
M <- 0.1
F <- 0.2
rec.age <- 2

library(SAFR)
my.Z <- ifelse( x < rec.age, M, ifelse(x > 6, M + 5 * F, M + floor((x-1)) * F))
cum.Z <- cumsum(my.Z) 

curve(ifelse( x < rec.age, M, ifelse(x > 6, M + 5 * F, floor((x-1)) * F)), from = 0, to = max(x),  ylab = "Hazard function", main = "mortality rate (1/year)", xlab = "t", type = "l", las = 1)
plot(x, cumsum(ifelse( x < rec.age, M, ifelse(x > 6, M + 5*F, floor((x-1)) * F)))/(length(x)/max(x)), type = "l", xlim = c(0,6), ylim = c(0,3), lwd = 2, ylab = "Cumulative hazard fct", main = "mortality (no unit)", xlab = "t", las = 1)
plot(x, exp(- (cum.Z - my.Z)/(length(x)/max(x))), ylab = "Survivor fct", main = "Probability of \nsurviving up to t", xlab = "t", type = "l", lwd = 2, las = 1)

dev.off()





