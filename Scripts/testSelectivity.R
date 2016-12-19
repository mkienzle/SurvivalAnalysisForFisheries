# CREATION DATE     13 Nov. 2008
# MODIFICATION DATE 13 Nov. 2008

# AUTHOR marco.kienzle@gmail.com

# BACKGROUND we are looking for a method to estimate M and F with data similar to the Torres Strait lobster assessment.
#            

# STATUS works

# PURPOSE express a likelihood function to estimate cohort-specific mortality
#         using catch data (and not survey)

# METHOD 
# ASSUMPTION 

# COMMENT if you have only catches, you can't estimate both fishing and natural mortality, you will end-up with M=F but Z will be well estimated
#         BUT if you can provide the ratio of catch over total abundance (basically you have a catch time series and a absolute survey),
#             then the ratio of catch over total death (i.e. N(t+1) - N(t)) is equal to F/Z

# Suppose age varies between 0 and 10
age = seq(0,10)

# Suppose you have a 60% (M+F = 0.51) chance survival from both fishing (F=0.405) and natural mortality (M=0.105)
M <- 0.105
x <- runif(1, min = 0.1, max = 1)
s <- runif(1, min = 0.1, max = 0.3)
F <-  x * s * age
print(paste("Simulated F is", round(x,3)))
print(paste("Simulated slope of selectivity is ", round(s,3)))

N0 <- 1e4
nb.at.age = cbind(age, N0 * exp(-(M + F)) ^ age) # this correspond to what you would see
                                         # with an exhaustive survey
# Calculate the total number of individual dying at age
total.death = N0 * (exp(-(M+F) * age) - exp(-(M+F) * (age+1)))

# According to stock assessment books, catch = F / Z
catch = F/(M+F) * total.death

# Plot the data
par(mfrow=c(1,2))
plot(age, nb.at.age[,2], main = "Surviving both nature and fishing", las = 1)
plot(age, catch, main = "Catch", type = "h", las=1)


# Maximum likelihood function to estimate F
llfunc = function(par){ # a function of only 1 parameter (F)

    F <- par[1]
    alpha <- par[2]
  # Fishing mortality is the parameter to estimate

  # Following Quinn and Deriso's equation (1.23)
  # Natural mortality relates to F and the total number of individual dying from 1 year to the next
  # and catch during the same year (here catch[1])
  # C = F/Z * nb.dying
  
  # Here we assume we have a survey that provides us with
  # the total number of individual that died within an interval
  #nb.dying = nb.at.age[1,2] - nb.at.age[2,2]

  # Their ratio is
  #r = catch[1]/nb.dying

  # So natural mortality
  #M = F * (1- r ) / r
    M <- 0.105
  # And total mortality
  Z = M + F * alpha * age

  # Calculating the probability of surviving until certain age
  prob1 = F * alpha * age / Z * (1 - exp(-Z * age))
  prob2 = F * alpha * age / Z * (1 - exp(-Z * (age+1)))

  # Finally the likelihood
  P = prob2-prob1

#    print(P)
#    print(sum(P))
 
    index <- which(P != 0)
    -sum( catch[index]  * log( P[index] / sum(P[index]) ))

   
} # End of function

# Estimate cohort-specific mortality rates

result <- optim(par = c(0.5,0.05), fn = llfunc, method = c("L-BFGS-B"),
      lower = c(1e-1,1e-1), upper = c(1,0.3), hessian = TRUE)
errors <- sqrt(diag(solve(result$hessian)))
print(paste("Estimated F is", round(result$par[1],3), "+-", round(errors[1],3)))
print(paste("Estimated s is", round(result$par[2],3), "+-", round(errors[2],3)))
