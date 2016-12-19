# CREATION DATE     13 Nov. 2008
# MODIFICATION DATE  5 Sep. 2014

# AUTHOR marco.kienzle@gmail.com

# BACKGROUND we are looking for a method to estimate M and F with data similar to the Torres Strait lobster assessment.
#            

# STATUS works wonderful

# PURPOSE express a likelihood function to estimate cohort-specific mortality
#         using catch data (and not survey)

# METHOD 
# ASSUMPTION 

# COMMENT if you have only catches, you can't estimate both fishing and natural mortality, you will end-up with M=F but Z will be well estimated
#         BUT if you can provide the ratio of catch over total abundance (basically you have a catch time series and a absolute survey),
#             then the ratio of catch over total death (i.e. N(t+1) - N(t)) is equal to F/Z

# Suppose age varies between 0 and 10


# Suppose you have a 60% (M+F = 0.51) chance survival from both fishing (F=0.405) and natural mortality (M=0.105)
#M <- runif(1, min = 0.1, max =0.5)
#x <- runif(1, min = 0.1, max = 1)
#s <- runif(1, min = 0.1, max = 1/3)
#effort <- runif(length(age), min = 1e3, max = 2e3)
#catchability <- runif(1, min = 1/2e4, max = 1/1.5e4)
#F <-  catchability * effort * c(s * age[1:4], rep(1,length(age[5:11])))


#print(paste("Simulated q is", round(catchability * 1e4,3), "10^(-4)"))
#print(paste("Simulated slope of selectivity is ", round(s,3)))
#print(paste("Simulated natural mortality is", round(M,3)))

#N0 <- runif(1, min = 1e3, max = 1e6)
#nb.at.age = cbind(age, N0 * exp(-(M + F)) ^ age) # this correspond to what you would see
                                         # with an exhaustive survey
# Calculate the total number of individual dying at age
#total.death = N0 * (exp(-(M+F) * age) - exp(-(M+F) * (age+1)))

# According to stock assessment books, catch = F / Z
#catch = F/(M+F) * total.death

# Sand whiting cohort born in 2005
age = seq(0,8)
catch <- 1e-2 * c(0,0, 308450.76, 357110.7,240061.4, 64508.44, 28809.75, 16925.27, 14423.486)
effort <- c(2965, 2267, 2377, 2270, 2530, 2384, 2096, 1915, 1648); names(effort) <- seq(2005, 2013)
rec.effort <- c(rep(38948+87794,5),rep(26936+56895,4))

# A second cohort
#age = seq(0,7)
#catch <- 1e-4 * c(0,0,103118.7, 472149.5, 263145.0, 45725.14, 55444.76, 31825.63)
#effort <- c(2267, 2377, 2270, 2530, 2384, 2096, 1915,1648); names(effort) <- seq(2006, 2013)
#rec.effort <- c(rep(38948,4),rep(26936,4))

# Plot the data
par(mfrow=c(1,2))
#plot(age, nb.at.age[,2], main = "Surviving both nature and fishing", las = 1)
plot(age, catch, main = "Catch", type = "h", las=1)


# Maximum likelihood function to estimate F
llfunc = function(par){ # a function of only 1 parameter (F)

    F <- par[1] * 1e-4 * effort
    alpha <- par[2]
    M <- par[3]

# And total mortality
    selectivity <- c(0, alpha * age[1:4], rep(1, length(age)-6),0.75, 0.5, 0.25)
    #selectivity <- c(0, alpha * age[1:4], rep(1, length(age)-5))
  Z = M + F * selectivity
    
  # Calculating the probability of surviving until certain age
  prob1 = F * selectivity / Z * (1 - exp(-Z * age))
  prob2 = F * selectivity / Z * (1 - exp(-Z * (age+1)))

  # Finally the likelihood
  P = prob2-prob1

#    print(P)
#    print(sum(P))
 
    index <- which(catch != 0)
    -sum( catch[index]  * log( P[index] / sum(P[index]) ))

   
} # End of function

# Maximum likelihood function to estimate F
llfunc2 = function(par){ # a function of only 1 parameter (F)

    comm.q.mf <- 1e-4
    rec.q.mf <- 1e-9
    
    F <- par[1] * comm.q.mf * effort 

    alpha <- par[2]
    M <- par[3]

# And total mortality
    selectivity <- c(0, alpha * age[1:4], rep(1, length(age)-5))
    #selectivity <- c(0, 1/(1+exp(-(age[3:6]-alpha))),rep(1,4))
    rec.sel <- c(0,0,0,rep(1,6))
  Z = M + F * selectivity  + rec.sel * par[4] * rec.q.mf * rec.effort
    
  # Calculating the probability of surviving until certain age
  prob1 = F * selectivity / Z * (1 - exp(-Z * age))
  prob2 = F * selectivity / Z * (1 - exp(-Z * (age+1)))

  # Finally the likelihood
  P = prob2-prob1

#    print(P)
#    print(sum(P))
 
    index <- which(P != 0)
    -sum( catch[index]  * log( P[index] / sum(P[index]) ))

   
} # End of function

# Estimate cohort-specific mortality rates

result <- optim(par = c(10,0.5, 0.1), fn = llfunc, method = c("L-BFGS-B"),
      lower = c(1e-2,1e-2,1e-2), upper = c(1e2,1,1), hessian = TRUE)
errors <- sqrt(diag(solve(result$hessian)))

print(" ***** ")
print(paste("Estimated catchability is", round(result$par[1],3), "+-", round(errors[1],3)))
print(paste("Estimated s is", round(result$par[2],3), "+-", round(errors[2],3)))
print(paste("Estimated M is", round(result$par[3],3), "+-", round(errors[3],3)))

result2 <- optim(par = c(10,0.5, 0.1,1), fn = llfunc2, method = c("L-BFGS-B"),
      lower = c(1e-4,1e-2,1e-2, 1e-1), upper = c(1e2,8,1,1e2), hessian = TRUE)
errors2 <- sqrt(diag(solve(result2$hessian)))

print(" ***** ")
print(paste("Estimated comm. catchability is", round(result2$par[1],3), "+-", round(errors2[1],3)))
print(paste("Estimated s is", round(result2$par[2],3), "+-", round(errors2[2],3)))
print(paste("Estimated M is", round(result2$par[3],3), "+-", round(errors2[3],3)))
print(paste("Estimated rec. catchability is", round(result2$par[4],3), "+-", round(errors2[4],3)))
