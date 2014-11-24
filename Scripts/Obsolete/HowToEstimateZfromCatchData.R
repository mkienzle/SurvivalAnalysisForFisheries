# CREATION DATE     13 Nov. 2008
# MODIFICATION DATE  2 Sep. 2014

# AUTHOR Marco.Kienzle@csiro.au; marco.kienzle@gmail.com

# BACKGROUND we are looking for a method to estimate M and F with data similar to
#            the Torres Strait lobster assessment.
#            

# PURPOSE express a likelihood function to estimate cohort-specific mortality
#         using catch data (and not survey)

# METHOD 
# ASSUMPTION 

# COMMENT if you have only catches, you can't estimate both fishing and natural mortality,
#         you will end-up with M=F but Z will be well estimated
#         BUT if you can provide the ratio of catch over total abundance (basically you have
#             a catch time series and a absolute survey),
#             then the ratio of catch over total death (i.e. N(t+1) - N(t)) is equal to F/Z

# Suppose age varies between 0 and 10
age = seq(0,10)

# Suppose you have a M=0.105, F and N0 are arbitrary (randomly generated)
M <- 0.105
F <- runif(1, min = 0.1, max = 3)
print(paste("Simulated Z is", round(M+F,3)))

N0 <- runif(1, min = 1e3, max = 1e4)

nb.at.age = cbind(age, N0 * exp(-(M+ F)) ^ age) # this correspond to what you would see
                                         # with an exhaustive survey
# Calculate the total number of individual dying at age
total.death = N0 * (exp(-(M+F) * age) - exp(-(M+F) * (age+1)))

# According to stock assessment books, catch = F / Z
catch = F/(M+F) * total.death

# mess around catch a little bit
#catch <- catch * runif(length(catch), min = 0.8, max = 1.2)

# Plot the data
par(mfrow=c(2,2))
plot(age, nb.at.age[,2], main = "Surviving both nature and fishing", las = 1)
plot(age, catch, main = "Catch", type = "h", las=1)


# Maximum likelihood function to estimate F
llfunc = function(Z){ # a function of only 1 parameter (Z)

  # Fishing mortality is the parameter to estimate

  # Following Quinn and Deriso's equation (1.23)
  # Natural mortality relates to F and the total number of individual dying from 1 year to the next
  # and catch during the same year (here catch[1])
  # C = F/Z * nb.dying
  
  # Assume natural mortality is known
  #M = 0.105

  # And total mortality
  #Z = M+F

  # Calculating the probability of surviving until certain age
  prob1 = (1 - exp(-Z * age))
  prob2 = (1 - exp(-Z * (age+1)))

  # Finally the likelihood
  P = prob2-prob1
  -sum( catch  * log( P / sum(P) ))
  
} # End of function

Estimate.Recruitment <- function(Z){
# the idea is that each catch figure provides an estimate of recruitment
    
    estimates <- catch / ((Z-M) * ((exp(-Z*age) - exp(-Z*(age+1)))/Z) )
    
# Use mean and sd of estimates to provide the best estimate
    return(c(mean(estimates), sd(estimates)))
}

# Estimate cohort-specific mortality rates

result <- optim(par = c(0.1), fn = llfunc, method = c("L-BFGS-B"),
      lower = c(1e-2), upper = c(3), hessian = TRUE)
print(paste("Estimated Z is", round(result$par,3), "+-", round(sqrt(diag(solve(result$hessian))),3)))

    print(paste("Simulated recruitment", round(N0)))
    print(paste("Estimated recruitment", paste(round(Estimate.Recruitment(result$par)), collapse = " +- ")))
