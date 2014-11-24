# CREATED   28 October 2014
# MODIFIED  28 October 2014

# AUTHOR marco.kienzle@gmail.com;

# STATUS works

rm(list=ls())

library(SAFR)
#source("UsefulFunctions.R")

#### Simulate some data

# Similar to sand whiting
max.age <- 9
sim <- GenerateData2(max.age = max.age, nb.of.cohort = 30) # Generate catch using gear selectivity

# Estimate assuming you know selectivity
result <- optim(par = c(0.2,1), fn = llfunc3, catch = sim$catch, effort = sim$effort, catchability.scaling.factor = 1e-4, selectivity.at.age =  c(0,0,seq(1/(max.age - 3), (max.age - 4)/(max.age - 3), length = max.age - 4), 1,1), method = c("L-BFGS-B"),
       lower = c(5e-2,5e-2), upper = c(10,0.5), hessian = TRUE)
errors <- sqrt(diag(solve(result$hessian)))

print(paste("Estimated catchability is", round(result$par[1],3), "+-", round(errors[1],3), " x 10^-4"))
print(paste("Estimated M is", round(result$par[2],3), "+-", round(errors[2],3)))

## source("SAFR/R/llfunc4.R")

## result2 <- optim(par = c(0.2,1, c(rep(1e-12,5), rep(1, max.age-5))), fn = llfunc4, catch = sim$catch, effort = sim$effort, catchability.scaling.factor = 1e-4, method = c("L-BFGS-B"),
##        lower = c(5e-2,5e-2, rep(1e-12, max.age)), upper = c(10,0.5, rep(1, max.age)), hessian = TRUE, control = list(maxit = 1e3))
## errors2 <- sqrt(abs(diag(solve(result2$hessian))))

## print(cbind(result2$par, errors2))

## source("SAFR/R/llfunc5.R")

## # here we fixed selectivity for the last 2 age-groups to 1
## result3 <- optim(par = c(0.2,1, rep(1e-12,max.age-2)), fn = llfunc5, catch = sim$catch, effort = sim$effort, catchability.scaling.factor = 1e-4, method = c("L-BFGS-B"),
##        lower = c(5e-2,5e-2, rep(1e-12, max.age-2)), upper = c(10,0.5, rep(1, max.age-2)), hessian = TRUE, control = list(maxit = 1e3))
## errors3 <- sqrt(abs(diag(solve(result3$hessian))))

## print(cbind(result3$par, errors3))

## # here we fixed selectivity for the last 2 age-groups to 1 and give approx. start value
## start.sel <- c(1e-12,1e-12,seq(1/(max.age - 3), (max.age - 4)/(max.age - 3), length = max.age - 4))
## result4 <- optim(par = c(0.2,1, start.sel), fn = llfunc5, catch = sim$catch, effort = sim$effort, catchability.scaling.factor = 1e-4, method = c("L-BFGS-B"),
##        lower = c(5e-2,5e-2, rep(1e-12, max.age-2)), upper = c(10,0.5, rep(1, max.age-2)), hessian = TRUE, control = list(maxit = 1e3))
## errors4 <- sqrt(abs(diag(solve(result4$hessian))))

## print(cbind(result4$par, errors4))

#### Here we fix natural mortality
source("SAFR/R/llfunc6.R")

# here we fixed selectivity for the last 2 age-groups to 1
sel.start <- c(rep(1e-12,5), rep(1, max.age-5))
result5 <- optim(par = c(1, sel.start), fn = llfunc6, M=result$par[2], catch = sim$catch, effort = sim$effort, catchability.scaling.factor = 1e-4, method = c("L-BFGS-B"),
       lower = c(5e-2, rep(1e-12, max.age)), upper = c(10, rep(1, max.age)), hessian = TRUE, control = list(maxit = 1e3))
errors5 <- sqrt(abs(diag(solve(result5$hessian))))

print(cbind(result5$par, errors5))

result6 <- optim(par = c(result5$par[1], 0.3, result5$par[-1]), fn = llfunc4, catch = sim$catch, effort = sim$effort, catchability.scaling.factor = 1e-4, method = c("L-BFGS-B"),
        lower = c(5e-2,5e-2, rep(1e-12, max.age)), upper = c(10,0.5, rep(1, max.age)), hessian = TRUE, control = list(maxit = 1e3))
 errors6 <- sqrt(abs(diag(solve(result6$hessian))))

print(cbind(result6$par, errors6))



