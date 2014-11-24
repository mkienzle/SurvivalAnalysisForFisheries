rm(list=ls())

#library(SAFR)
source("UsefulFunctions.R")

# Similar to sand whiting
max.age <- 9
population <- GenerateData3(max.age = 10, nb.of.cohort = 17)

nb.at.age.sample <- draw.sample(population$catch, sample.size = 1e4 * nrow(population$catch))

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
