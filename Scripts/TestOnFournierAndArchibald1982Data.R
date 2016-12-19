# CREATED  25 November 2014
# MODIFIED 11 December 2014

# STATUS not working really great !

# Supposing you knew gear selectivity, it
#        - can't estimate natural mortality using the entire dataset
#        - can estimate natural mortality using up to first 16 years but not more
#       see result below

# BUT letting gear selectivity to be estimated
#        - produce estimates of M ~ 0.95 (see result2)

rm(list=ls())
library(SAFR)

## Using the first 10 year of catch at age data
sub.set <- 1:10
catch.at.age <- read.csv("../Data/FournierEtArchibald1982-Table2.csv", row.names = 1)[sub.set,]
effort.tmp <- read.table("../Data/effort.csv")
effort <- outer(effort.tmp[sub.set,1], rep(1, ncol(catch.at.age)))

max.age <- ncol(catch.at.age)

sel <- c(rep(0,3), c(0.050, 0.159, 0.329, 0.517, 0.686, 0.816, 0.906, 0.964, 1, 1))
start.value <- c(2,0.5, sel)


result <- optim(par = c(2,0.5), fn = llfunc3, catch = catch.at.age, effort = effort, selectivity.at.age = sel, catchability.scaling.factor = 1e-1, method = c("L-BFGS-B"),
        lower = c(1,0.1), upper = c(10,10), hessian = TRUE, control = list(maxit = 1e3))
              
errors <- sqrt(abs(diag(solve(result$hessian))))

print("Min log(L)")
print(result$value)
cbind(result$par, errors)

result2 <- optim(par = start.value, fn = llfunc4, catch = catch.at.age, effort = effort, catchability.scaling.factor = 1e-2, method = c("L-BFGS-B"),
        lower = c(1,5e-2, rep(1e-12, max.age-3), rep(0.95,3)), upper = c(5,1, rep(1, max.age)), hessian = TRUE, control = list(maxit = 1e3))

 errors2 <- sqrt(abs(diag(solve(result2$hessian))))

print("Min log(L)")
print(result2$value)
print(cbind(result2$par, errors2))

## ##
## nb.at.age.sample <- read.csv("../Data/FournierEtArchibald1982-Table10.csv", row.names = 1)
## effort.tmp <- read.table("../Data/effort.csv")
## effort <- outer(effort.tmp[,1], rep(1, ncol(nb.at.age.sample)))


## max.age <- ncol(nb.at.age.sample)

## ### 1st attempt 
## # These starting value provide a better fit but explain the data by sudden drops in selectivity at age 9, 10 and 13
## #sel.start <- c(rep(0,3), rep(1, max.age-3))
## start.value <- c(1,0.5, rep(0,3), c(0.050, 0.159, 0.329, 0.517, 0.686, 0.816, 0.906, 0.964, 1, 1))
## sel <- c(rep(0,3), c(0.050, 0.159, 0.329, 0.517, 0.686, 0.816, 0.906, 0.964, 1, 1))
## #result <- optim(par = c(7, 0.3, sel.start), fn = llfunc4, catch = nb.at.age.sample, effort = effort, catchability.scaling.factor = 1, method = c("L-BFGS-B"),
## #result <- optim(par = start.value, fn = llfunc4, catch = nb.at.age.sample, effort = effort, catchability.scaling.factor = 1, method = c("L-BFGS-B"),
## #       lower = c(0.9,5e-2, rep(1e-12, max.age)), upper = c(1.1,1, rep(1, max.age)), hessian = TRUE, control = list(maxit = 1e3))

## result <- optim(par = c(1,0.5), fn = llfunc3, catch = nb.at.age.sample, effort = effort, selectivity.at.age = sel, catchability.scaling.factor = 1, method = c("L-BFGS-B"),
##         lower = c(5e-2,0.1), upper = c(10,10), hessian = TRUE, control = list(maxit = 1e3))
              
## errors <- sqrt(abs(diag(solve(result$hessian))))

## print("Min log(L)")
## print(result$value)
