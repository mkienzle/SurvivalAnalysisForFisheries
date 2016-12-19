source("SAFR/R/llfunc2bis.R")

# Suppose age varies between 0 and 10
age <- seq(0,20)

# Generate a random natural mortality
M <- 0; #runif(1, min = 0.3, max = 0.3)

effort <- runif(length(age)-1, min = 1e3, max = 2e3)
catchability <- runif(1, min = 1/3e3, max = 1/3e3)

# Catchability scaling factor
csf <- 1e-4
F <-  catchability * effort

print(paste("Simulated q is", round(catchability / csf,3), as.character(csf)))
print(paste("Simulated M is ", round(M,3)))

N0 <- runif(1, min = 4e3, max = 1e4)
print(paste("Simulated recruitment is", round(N0)))

# Calculate number at age using a simple exponential model ( see Quinn and Deriso, 1999)
nb.at.age <- cbind(age, N0 * exp(-c(0, cumsum(M + F))))

# Calculate the total number of individual dying at age
total.death <- N0 * (exp(-c(0,cumsum(M+F)[-length(effort)])) - exp(-cumsum(M+F)))

# Number of fish dying from fishing is a fraction of total mortality
catch <- F/(M+F) * total.death

# Estimate q and M
lower.bound <- c(1.001*sum(catch))
upper.bound <- c(2 * sum(catch))
best.qM.est <- optim(par = c(1.5 * sum(catch)), lower = lower.bound, upper = upper.bound, fn = llfunc2bis, catch = catch, effort
= effort, catchability.scaling.factor = csf, method = c("L-BFGS-B"), hessian = TRUE)
