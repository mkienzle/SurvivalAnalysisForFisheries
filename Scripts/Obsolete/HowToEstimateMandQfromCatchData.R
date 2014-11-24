# CREATED  13 Nov. 2008
# MODIFIED  2 Sep. 2014

# AUTHOR marco.kienzle@csiro.au; marco.kienzle@gmail.com;

# BACKGROUND we are looking for a method to estimate M and F 
#            

# STATUS works wonderful

# PURPOSE express a likelihood function to estimate cohort-specific mortality
#         using catch data (and not survey)

# METHOD 
# ASSUMPTION 

# COMMENT if you have only catches, you can estimate fishing and natural mortality if
#            you have effort data

# Suppose age varies between 0 and 10
age = seq(0,10)

# Generate a random natural mortality
M <- runif(1, min = 1e-2, max = 0.3)

effort <- runif(length(age)-1, min = 1e3, max = 2e3)
catchability <- runif(1, min = 1/3e3, max = 1/2e3)
F <-  catchability * effort

print(paste("Simulated q is", round(catchability * 1e4,3), "10^-4"))
print(paste("Simulated M is ", round(M,3)))

N0 <- runif(1, min = 4e3, max = 1e4)
print(paste("Simulated recruitment is", round(N0)))

nb.at.age = cbind(age, N0 * exp(-c(0, cumsum(M + F)))) # this correspond to what you would see
                                         # with an exhaustive survey
# Calculate the total number of individual dying at age
#total.death = N0 * (exp(-(M+F) * age) - exp(-(M+F) * (age+1)))
total.death <- N0 * (exp(-c(0,cumsum(M+F)[-length(effort)])) - exp(-cumsum(M+F)))

# According to stock assessment books, catch = F / Z
catch <- F/(M+F) * total.death

# add noise to catch
catch <- catch * runif(length(catch), min = 0.95, max = 1.05)

# Plot the data
par(mfrow=c(1,2))
plot(age, nb.at.age[,2], main = "Surviving both nature and fishing", las = 1)
plot(age[-length(age)], catch, main = "Catch", type = "h", las=1)


    # Scale parameter to same order of magnitude
    catchability.multiplying.factor<-1e-4

# Maximum likelihood function to estimate q and M
llfunc <- function(par){ # a function of 2 parameters (q,M)

    F <- par[1] * catchability.multiplying.factor * effort
    M <- par[2]
  # And total mortality
  Z <- M + F
    
  # Calculating the probability of surviving until certain age
  prob1 <- F / Z * (1 - exp(-cumsum(Z)))
  prob2 <- F / Z * (1 - exp(-c(0, cumsum(Z)[-length(effort)])))
    
  # Finally the likelihood
  P <- prob2-prob1

    # Find out null values, if any
    index <- which(P != 0)

    # Negative log-likelihood
    -sum( catch[index]  * log( P[index] / sum(P[index]) ))
   
} # End of function

# Scale parameter to same order of magnitude
catchability.multiplying.factor<-1e-4

Estimate.Recruitment <- function(par){
# the idea is that each catch figure provides an estimate of recruitment
    
    
    F <- par[1] * catchability.multiplying.factor * effort
    M <- par[2]
  # And total mortality
  Z <- M + F
    
  # Calculating the probability of surviving until certain age
  prob1 <- F / Z * (1 - exp(-cumsum(Z)))
  prob2 <- F / Z * (1 - exp(-c(0, cumsum(Z)[-length(effort)])))
    
  # Finally the likelihood
  P <- prob2-prob1

  estimates <- catch / (prob1 - prob2)
    
# Use mean and sd of estimates to provide the best estimate
    return(c(mean(estimates), sd(estimates)))
}


# Estimate cohort-specific mortality rates

result <- optim(par = c(10,1), fn = llfunc, method = c("L-BFGS-B"),
      lower = c(1e-2,1e-2), upper = c(1e2,1e2), hessian = TRUE)
errors <- sqrt(diag(solve(result$hessian)))

print("")
print(paste("Estimated catchability is", round(result$par[1],3), "+-", round(errors[1],3), " x 10^-4"))
print(paste("Estimated M is", round(result$par[2],3), "+-", round(errors[2],3)))

print(paste("Estimated recruitment is", paste(round(Estimate.Recruitment(result$par),0), collapse = " +- ")))
