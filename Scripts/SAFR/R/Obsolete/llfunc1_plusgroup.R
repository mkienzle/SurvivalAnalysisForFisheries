# Maximum likelihood function to estimate Z using catch at age from a single cohort
llfunc1.plusgroup <- function(Z, catch, plus.group = TRUE){ # a function of only 1 parameter (Z)

  # Age assumed in year, catch data grouped in yearly bins
  age <- seq(0, length(catch)-1)
    
  # Calculating the probability of surviving until certain age
  prob1 <- exp(-Z * age)
  prob2 <- exp(-Z * (age+1))

  # Finally the likelihood
  P <- prob1-prob2
  P[length(catch)] <- prob1[length(catch)] # this is the +group

  -sum(catch * log( P / sum(P) ))

} # End of function
