# Maximum likelihood function to estimate q and M
llfunc2 <- function(par, catch, effort, catchability.scaling.factor){ # a function of 2 parameters (q,M)

    F <- par[1] * catchability.scaling.factor * effort
    M <- par[2]

    # And total mortality
    Z <- M + F
    
    # Calculating the probability of surviving until certain age
    #prob1 <- F / Z * exp(-cumsum(Z))
    #prob2 <- F / Z * exp(-c(0, cumsum(Z)[-length(effort)]))
    prob1 <- F/Z * exp(-c(0, cumsum(Z)[-length(effort)]))
    prob2 <- F/Z * exp(-cumsum(Z))
    
    # Finally the likelihood
    P <- prob1-prob2

    # Negative log-likelihood
    -sum( catch * log( P / sum(P) ))
   
} # End of function
