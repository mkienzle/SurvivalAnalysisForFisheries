# CREATED   3 Sep. 2014
# MODIFIED  8 Sep. 2014

# AUTHOR marco.kienzle@gmail.com;

# STATUS works

rm(list=ls())

source("UsefulFunctions.R")

#### Simulate some data

# Similar to sand whiting
max.age <- 13
sim <- GenerateData2(max.age = max.age, nb.of.cohort = 17) # Generate catch using gear selectivity

# idealised
#max.age <- 10
#sim <- GenerateData2(max.age = max.age, nb.of.cohort = 40) # Generate catch using gear selectivity


# The method is fairly robust to pretty large random errors
# sim$catch <- sim$catch * matrix(runif( nrow(sim$catch) * ncol(sim$catch), min = 0.7 , max = 1.3), nrow =  nrow(sim$catch), ncol = ncol(sim$catch))

# The method is robust to mild random errors
sim$catch <- sim$catch * matrix(runif( nrow(sim$catch) * ncol(sim$catch), min = 0.9 , max = 1.1), nrow =  nrow(sim$catch), ncol = ncol(sim$catch))

plot.catch.by.cohort(sim$catch)

# log-likelihood function
llfunc <- function(par){

    # Re-arrange input data into cohorts
    catch.by.cohort <- Caaa2Coaa(sim$catch)
    effort.by.cohort <- Caaa2Coaa(sim$effort)

    # optim works best on scaled parameters
    catchability.mf <- 1e-4

    # Allocate param to readable variable names
    M <- par[1]
    q <- par[2] * catchability.mf
    # Simulated selectivity
    #selectivity.at.age <- c(0,0,seq(1/(max.age - 3), (max.age - 4)/(max.age - 3), length = max.age - 4), 1,1) # FOR TEST PURPOSE
    alpha <- par[3]
    selectivity.at.age <- c(0,0, alpha * seq(1,max.age-4), 1, 1)

    # matrix of fishing mortality
    F <- q * effort.by.cohort * outer(rep(1, nrow(effort.by.cohort)), selectivity.at.age)

    # total mortality
    Z <- M + F

    # cumulative mortality
    cum.Z <- my.cumsum(Z)

    # Calculate the probability of observation in each interval
    prob1 <- F/Z * (1 - exp(-cum.Z))
    prob2 <- F/Z * (1 - exp(-(cum.Z-Z)))
    P <- prob1-prob2

    # discard zeroes and NA from sum of logs
    index <- which(!is.na(catch.by.cohort) & catch.by.cohort!=0)


    # Negative log-likelihood
    -sum(catch.by.cohort[index] * log( P[index] / total.over.lines(P)[index]))
}

result <- optim(par = c(0.2,1, 0.2), fn = llfunc, method = c("L-BFGS-B"),
      lower = c(5e-2,5e-2,1e-2), upper = c(0.5,10,0.5), hessian = TRUE)
errors <- sqrt(diag(solve(result$hessian)))

print("")
print(paste("Estimated catchability is", round(result$par[2],3), "+-", round(errors[2],3), " x 10^-4"))
print(paste("Estimated M is", round(result$par[1],3), "+-", round(errors[1],3)))
print(paste("Estimated alpha is", round(result$par[3],3), "+-", round(errors[3],3)))

