# CREATION DATE     13 Nov. 2008
# MODIFICATION DATE 13 Nov. 2008

# AUTHOR marco.kienzle@gmail.com

# BACKGROUND we are looking for a method to estimate M and F with data similar to the Torres Strait lobster assessment.
#            

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
F <- runif(1, min = 0.1, max = 3)
print(paste("Simulated F is", round(F,3)))

nb.at.age = cbind(age, 1000 * exp(-(M+ F)) ^ age) # this correspond to what you would see
                                         # with an exhaustive survey
# Calculate the total number of individual dying at age
total.death = 1000 * (exp(-(M+F) * age) - exp(-(M+F) * (age+1)))

# According to stock assessment books, catch = F / Z
catch = F/(M+F) * total.death

# Plot the data
par(mfrow=c(2,2))
plot(age, nb.at.age[,2], main = "Surviving both nature and fishing", las = 1)
plot(age, catch, main = "Catch", type = "h", las=1)


# Maximum likelihood function to estimate F
llfunc = function(F){ # a function of only 1 parameter (F)

  # Fishing mortality is the parameter to estimate

  # Following Quinn and Deriso's equation (1.23)
  # Natural mortality relates to F and the total number of individual dying from 1 year to the next
  # and catch during the same year (here catch[1])
  # C = F/Z * nb.dying
  
  # Here we assume we have a survey that provides us with
  # the total number of individual that died within an interval
  nb.dying = nb.at.age[1,2] - nb.at.age[2,2]

  # Their ratio is
  r = catch[1]/nb.dying

  # So natural mortality
  M = F * (1- r ) / r

  # And total mortality
  Z = M+F

  # Calculating the probability of surviving until certain age
  prob1 = (1 - exp(-Z * age))
  prob2 = (1 - exp(-Z * (age+1)))

  # Finally the likelihood
  P = prob2-prob1
  -sum( catch  * log( P / sum(P) ))
  
} # End of function

# Estimate cohort-specific mortality rates

result <- optim(par = c(0.1), fn = llfunc, method = c("L-BFGS-B"),
      lower = c(1e-2), upper = c(3), hessian = TRUE)
print(paste("Estimated F is", round(result$par,3)))
