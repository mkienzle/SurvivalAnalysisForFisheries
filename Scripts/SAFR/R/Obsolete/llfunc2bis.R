
prob.for.llfunc2 <- function(par, effort, catchability.scaling.factor){

    F <- par[1] * catchability.scaling.factor * effort
    M <- par[2]

    # And total mortality
    Z <- M + F
    
    # Calculating the probability of surviving until certain age
    prob1 <- F/Z * exp(-c(0, cumsum(Z)[-length(effort)]))
    #prob1 <- exp(-c(0, cumsum(Z)[-length(effort)]))
    prob2 <- F/Z * exp(-cumsum(Z))
    #prob2 <- exp(-cumsum(Z))
    
    
    # Finally the likelihood
    P <- prob1-prob2

return(P)
}

# Maximum likelihood function to estimate q and M
llfunc2bis <- function(par, catch, effort, catchability.scaling.factor){ # a function of 2 parameters (q,M)

	print(par)
    P <- prob.for.llfunc2(c(1/3e3/csf,0), effort, catchability.scaling.factor)
    
    # Negative log-likelihood
    #-sum( catch * log( P / sum(P) ))
    tmp <- 0
    for(i in 1:length(catch)) tmp <- tmp + sum(log(seq(1,catch[i])))

log.lik <- 0
log.lik <- sum(log(seq(1, par[1]))) - sum(log(seq(1, par[1] - sum(catch)))) - tmp + (par[1] - sum(catch)) * log(1-sum(P)) + sum(catch * log(P))

return(-log.lik)
   
} # End of function
