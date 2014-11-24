# CREATED   5 November 2014
# MODIFIED 12 November 2014

# PURPOSE test estimations by simulations

source("UsefulFunctions.R")

#############################################################################
# Simulate a population
#############################################################################
population <- GenerateData3(max.age = 10, nb.of.cohort = 17)

#############################################################################
# Simulate sampling
#############################################################################

### Sampling strategy 1
#nb.at.age.sample <- draw.sample(population$catch, 1e4)

### Sampling strategy 2
# Take n sample each year
n <- 1e3

# Analysis without correction for catch
nb.at.age.sample <- draw.sample2(population$catch, sample.size.each.year = n)

### Analysis with correction for catch
# proportion at age in sample
prop.at.age.sample <- nb.at.age.sample / outer(rowSums(nb.at.age.sample), rep(1,ncol(nb.at.age.sample)))

# estimated total number in the population
raised.number <- outer(rowSums(population$catch), rep(1, ncol(nb.at.age.sample))) * prop.at.age.sample
# reduce back to sampling number
nb.at.age.sample <- raised.number * sum(nb.at.age.sample) / sum(raised.number)

### Sampling strategy 3 - sample in proportion of effort
# Take n sample each year
#n <- 1e2
#nb.at.age.sample <- draw.sample3(population$catch, vec = as.vector(n * population$effort[,1]))


library(SAFR)
source("SAFR/R/llfunc7.R")

# Estimate assuming you know selectivity
result <- optim(par = c(0.2,1, 10, 2), fn = llfunc7, catch = nb.at.age.sample, effort = population$effort, catchability.scaling.factor = 1e-4, method = c("L-BFGS-B"),
       lower = c(5e-2,5e-2,1,1), upper = c(15,1,20,20), hessian = TRUE)
errors <- sqrt(diag(solve(result$hessian)))

print(" #### ")

print(paste("Estimated catchability is", round(result$par[1],3), "+-", round(errors[1],3), " x 10^-4"))

print(paste("logistic param a ", round(result$par[3],3), "+-", round(errors[3],3)))
print(paste("logistic param b", round(result$par[4],3), "+-", round(errors[4],3)))

print(paste("Estimated M is", round(result$par[2],3), "+-", round(errors[2],3)))

## #sel.start <- c(rep(1e-12,floor(max.age/2)), rep(1, max.age-floor(max.age/2)))
## #sel.start <- c(0,0,0.5, 0.75, rep(1, max.age -4))
## sel.start <- c(0,0,seq(1/(max.age - 5), (max.age - 6)/(max.age - 5), length = max.age - 8), rep(1,6))

## #res <- optim(par = c(7, 0.3, sel.start), fn = llfunc4, catch = nb.at.age.sample, effort = population$effort, catchability.scaling.factor = 1e-4, hessian = TRUE, control = list(maxit = 1e4))
## res <- optim(par = c(7, 0.3, sel.start), fn = llfunc4, catch = population$catch, effort = population$effort, catchability.scaling.factor = 1e-4, method = c("L-BFGS-B"),
##        lower = c(5e-2,5e-2, rep(1e-12, max.age)), upper = c(10,1, rep(1, max.age)), hessian = TRUE, control = list(maxit = 1e3))

## errors <- sqrt(abs(diag(solve(res$hessian))))
## print(cbind("Estimate" = res$par, "Error" = errors))


