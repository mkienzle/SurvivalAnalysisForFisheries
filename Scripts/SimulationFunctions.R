# CREATED  13 November 2014
# MODIFIED 13 November 2014

# PURPOSE function for the simulation study

source("UsefulFunctions.R")

###########################################################################################################################################
### Aging a random sample from the population as if you had all the data in front of you at once
###########################################################################################################################################

SamplingStrategy1 <- function(max.age = 10, nb.of.cohort = 17, n.sample.per.year = 1e3, verbose = FALSE){
#############################################################################
# Simulate a population
#############################################################################
population <- GenerateData3(max.age = max.age, nb.of.cohort = nb.of.cohort)

#############################################################################
# Simulate sampling
#############################################################################

# sample a fix number of fish each years
nb.at.age.sample <- draw.sample(population$catch, sample.size = n.sample.per.year * (nb.of.cohort + 1 - max.age))

library(SAFR)
source("SAFR/R/llfunc7.R")

# Estimate assuming you know selectivity
lower.bound <- c(5e-2,1e-2,1,1)
upper.bound <- c(15,1,20,20)

csf <- 1e-4 # catchability scaling factor

result <- optim(par = c(0.2,1, 10, 2), fn = llfunc7, catch = nb.at.age.sample, effort = population$effort, catchability.scaling.factor = csf, method = c("L-BFGS-B"),
       lower = lower.bound, upper = upper.bound, hessian = TRUE)
errors <- sqrt(diag(solve(result$hessian)))

# Define the status of optimization
 optimization.status <- "successful"

if(result$converge != 0){print("failed convergence"); optimization.status <- "failed"}
if(length(which(is.na(errors)))){print("Some errors are NA"); optimization.status <- "failed"}
for(i in 1:4){
    if( (abs(result$par[i] - lower.bound[i]) / lower.bound[i]) < 1e-3 ){print(paste("Parameter ", i, "at lower boundary"));  optimization.status <- "failed"}
        if( (abs(result$par[i] - upper.bound[i]) / upper.bound[i] ) < 1e-3 ){print(paste("Parameter ", i, "at higher boundary"));  optimization.status <- "failed"}
    }

if(verbose){
print(" #### ")

print(paste("Estimated catchability is", round(result$par[1],3), "+-", round(errors[1],3), " x 10^-4"))

print(paste("logistic param a ", round(result$par[3],3), "+-", round(errors[3],3)))
print(paste("logistic param b", round(result$par[4],3), "+-", round(errors[4],3)))

print(paste("Estimated M is", round(result$par[2],3), "+-", round(errors[2],3)))
}




return(list(population = population, nb.at.age.sample = nb.at.age.sample, optimization.status = optimization.status, optimization = c(catchability.scaling.factor = csf, result), uncertainties = errors))
}

###########################################################################################################################################
### Aging a fix number of fish each year
###########################################################################################################################################

SamplingStrategy2 <- function(max.age = 10, nb.of.cohort = 17, n.sample.per.year = 1e3, verbose = FALSE){
#############################################################################
# Simulate a population
#############################################################################
population <- GenerateData3(max.age = max.age, nb.of.cohort = nb.of.cohort)

#############################################################################
# Simulate sampling
#############################################################################

# sample a fix number of fish each years
nb.at.age.sample <- draw.sample2(population$catch, sample.size.each.year = n.sample.per.year)

library(SAFR)
source("SAFR/R/llfunc7.R")

# Estimate assuming you know selectivity
lower.bound <- c(5e-2,1e-2,1,1)
upper.bound <- c(15,1,20,20)

csf <- 1e-4 # catchability scaling factor

result <- optim(par = c(0.2,1, 10, 2), fn = llfunc7, catch = nb.at.age.sample, effort = population$effort, catchability.scaling.factor = csf, method = c("L-BFGS-B"),
       lower = lower.bound, upper = upper.bound, hessian = TRUE)
errors <- sqrt(diag(solve(result$hessian)))

# Define the status of optimization
 optimization.status <- "successful"

if(result$converge != 0){print("failed convergence"); optimization.status <- "failed"}
if(length(which(is.na(errors)))){print("Some errors are NA"); optimization.status <- "failed"}
for(i in 1:4){
    if( (abs(result$par[i] - lower.bound[i]) / lower.bound[i]) < 1e-3 ){print(paste("Parameter ", i, "at lower boundary"));  optimization.status <- "failed"}
        if( (abs(result$par[i] - upper.bound[i]) / upper.bound[i] ) < 1e-3 ){print(paste("Parameter ", i, "at higher boundary"));  optimization.status <- "failed"}
    }

if(verbose){
print(" #### ")

print(paste("Estimated catchability is", round(result$par[1],3), "+-", round(errors[1],3), " x 10^-4"))

print(paste("logistic param a ", round(result$par[3],3), "+-", round(errors[3],3)))
print(paste("logistic param b", round(result$par[4],3), "+-", round(errors[4],3)))

print(paste("Estimated M is", round(result$par[2],3), "+-", round(errors[2],3)))
}




return(list(population = population, nb.at.age.sample = nb.at.age.sample, optimization.status = optimization.status, optimization = c(catchability.scaling.factor = csf, result), uncertainties = errors))
}


###########################################################################################################################################
### Aging a fix number of fish each year, correcting the number at age matrix by year catch
###########################################################################################################################################

SamplingStrategy2withCorrection <- function(max.age = 10, nb.of.cohort = 17, n.sample.per.year = 1e3, verbose = FALSE){
#############################################################################
# Simulate a population
#############################################################################
population <- GenerateData3(max.age = max.age, nb.of.cohort = nb.of.cohort)

#############################################################################
# Simulate sampling
#############################################################################

# Analysis without correction for catch
nb.at.age.sample <- draw.sample2(population$catch, sample.size.each.year = n.sample.per.year)

### Analysis with correction for catch
# proportion at age in sample
prop.at.age.sample <- nb.at.age.sample / outer(rowSums(nb.at.age.sample), rep(1,ncol(nb.at.age.sample)))

# estimated total number in the population
raised.number <- outer(rowSums(population$catch), rep(1, ncol(nb.at.age.sample))) * prop.at.age.sample

# reduce back to sampling number
corrected.nb.at.age.sample <- raised.number * sum(nb.at.age.sample) / sum(raised.number)

library(SAFR)
source("SAFR/R/llfunc7.R")

# Estimate assuming you know selectivity
lower.bound <- c(5e-2,1e-2,1,1)
upper.bound <- c(15,1,20,20)

csf <- 1e-4 # catchability scaling factor

result <- optim(par = c(0.2,1, 10, 2), fn = llfunc7, catch = corrected.nb.at.age.sample, effort = population$effort, catchability.scaling.factor = csf, method = c("L-BFGS-B"),
       lower = lower.bound, upper = upper.bound, hessian = TRUE)
errors <- sqrt(diag(solve(result$hessian)))

# Define the status of optimization
 optimization.status <- "successful"

if(result$converge != 0){print("failed convergence"); optimization.status <- "failed"}
if(length(which(is.na(errors)))){print("Some errors are NA"); optimization.status <- "failed"}
for(i in 1:4){
    if( (abs(result$par[i] - lower.bound[i]) / lower.bound[i]) < 1e-3 ){print(paste("Parameter ", i, "at lower boundary"));  optimization.status <- "failed"}
        if( (abs(result$par[i] - upper.bound[i]) / upper.bound[i] ) < 1e-3 ){print(paste("Parameter ", i, "at higher boundary"));  optimization.status <- "failed"}
    }

if(verbose){
print(" #### ")

print(paste("Estimated catchability is", round(result$par[1],3), "+-", round(errors[1],3), " x 10^-4"))

print(paste("logistic param a ", round(result$par[3],3), "+-", round(errors[3],3)))
print(paste("logistic param b", round(result$par[4],3), "+-", round(errors[4],3)))

print(paste("Estimated M is", round(result$par[2],3), "+-", round(errors[2],3)))
}




#return(list(population = population, nb.at.age.sample = nb.at.age.sample, corrected.nb.at.age.sample = corrected.nb.at.age.sample, optimization.status = optimization.status, optimization = result))
return(list(population = population, nb.at.age.sample = nb.at.age.sample, optimization.status = optimization.status, optimization = c(catchability.scaling.factor = csf, result), uncertainties = errors))

}
