# log-likelihood function for a catch-at-age matrix
llfunc3 <- function(par, catch, effort, selectivity.at.age, catchability.scaling.factor, plus.group = FALSE){

    # Re-arrange input data into cohorts
    catch.by.cohort <- Caaa2Coaa(catch)
    effort.by.cohort <- Caaa2Coaa(effort)

    # Allocate param to readable variable names
    q <- par[1] * catchability.scaling.factor
    M <- par[2]
    
    # matrix of fishing mortality
    F <- q * effort.by.cohort * outer(rep(1, nrow(effort.by.cohort)), selectivity.at.age)

    # total mortality
    Z <- M + F

    # cumulative mortality
    cum.Z <- my.cumsum(Z)

    # Calculate the probability of observation in each interval
    prob1 <- F/Z * exp(-(cum.Z-Z))
    prob2 <- F/Z * exp(-cum.Z)
    P <- prob1-prob2

    # Using a +group in the calculation of the likelihood: essentially the boundary of the interval is \infty -> prob2 is set to 0
    if(plus.group) P[,ncol(P)] <- prob1[,ncol(P)] # change probability to account for a +group

    # discard zeroes and NA from sum of logs
    index <- which(!is.na(catch.by.cohort) & P != 0)

    # Negative log-likelihood
    -sum(catch.by.cohort[index] * log( P[index] / total.over.lines(P)[index]))
}
