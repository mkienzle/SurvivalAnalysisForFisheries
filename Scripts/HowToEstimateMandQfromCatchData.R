# CREATED  27 October 2014
# MODIFIED 27 October 2014

#########################################
# Using the example from the SAFR library
#########################################

library(SAFR)
example(EstimateMandQ)

#########################################
# Creating your own example
#########################################

# Suppose age varies between 0 and 10
age <- seq(0,10)

# Generate a random natural mortality
M <- runif(1, min = 1e-2, max = 0.3)

effort <- runif(length(age)-1, min = 1e3, max = 2e3)
catchability <- runif(1, min = 1/3e3, max = 1/2e3)
catchability.multiplying.factor <- 1e-4
F <-  catchability * effort

print(paste("Simulated q is", round(catchability / catchability.multiplying.factor,3), as.character(catchability.multiplying.factor)))
print(paste("Simulated M is ", round(M,3)))

N0 <- runif(1, min = 4e4, max = 1e5)
print(paste("Simulated recruitment is", round(N0)))

# Calculate number at age using a simple exponential model ( see Quinn and Deriso, 1999)
nb.at.age <- cbind(age, N0 * exp(-c(0, cumsum(M + F))))

# Calculate the total number of individual dying at age
total.death <- N0 * (exp(-c(0,cumsum(M+F)[-length(effort)])) - exp(-cumsum(M+F)))

# Number of fish dying from fishing is a fraction of total mortality
catch <- F/(M+F) * total.death

# Estimate q and M
best.qM.est <- EstimateMandQ(catch, effort, catchability.multiplying.factor)

errors <- sqrt(diag(solve(best.qM.est$hessian)))

print(" ##### ")
print(paste("Estimated catchability is", round(best.qM.est$par[1],3), "+-", round(errors[1],3), as.character(catchability.multiplying.factor)))
print(paste("Estimated M is", round(best.qM.est$par[2],3), "+-", round(errors[2],3)))
