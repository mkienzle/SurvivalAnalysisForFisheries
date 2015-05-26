# MODIFIED 8 May 2015
# Maximum likelihood function to estimate Z using catch at age from a single cohort
llfunc1 <- function(Z, catch, plus.group = FALSE){ # a function of only 1 parameter (Z)

  # Age assumed in year, catch data grouped in yearly bins
  age <- seq(0, length(catch)-1)
    
  # Calculating the probability of surviving until certain age
  prob1 <- exp(-Z * age)
  prob2 <- exp(-Z * (age+1))

  # Finally the likelihood
  P <- prob1-prob2
  if(plus.group) P[length(catch)] <- prob1[length(catch)] # change probability to account for a +group
  -sum(catch * log( P / sum(P) ))

} # End of function
