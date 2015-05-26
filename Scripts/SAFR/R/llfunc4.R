# log-likelihood function for a catch-at-age matrix
llfunc4 <- function(par, catch, effort, catchability.scaling.factor, plus.group = FALSE){
    #print(par)
    if(length(which(par<0)) > 0){ return(1e6)}
    # Re-arrange input data into cohorts
    catch.by.cohort <- Caaa2Coaa(catch)
    effort.by.cohort <- Caaa2Coaa(effort)

    # Allocate param to readable variable names
    q <- par[1] * catchability.scaling.factor
    M <- par[2]
    selectivity.at.age <- par[-c(1,2)]
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
    if(plus.group) P[,ncol(P)] <- prob1[,ncol(P)] # change probability to account for a +group

    # discard zeroes and NA from sum of logs
    index <- which(!is.na(catch.by.cohort) & P!=0)

    # Negative log-likelihood
    -sum(catch.by.cohort[index] * log( P[index] / total.over.lines(P)[index]))
}
