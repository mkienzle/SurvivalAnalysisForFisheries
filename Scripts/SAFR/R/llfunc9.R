# log-likelihood function of nb of otoliths at age  to estimate 2 different catchability
llfunc9 <- function(par, catch, effort1, effort2, catchability.scaling.factor1, catchability.scaling.factor2, plus.group = FALSE){
    #print(par)
    if(length(which(par<0)) > 0){ return(1e6)}
    # Re-arrange input data into cohorts
    catch.by.cohort <- Caaa2Coaa(catch)
    effort1.by.cohort <- Caaa2Coaa(effort1)
    effort2.by.cohort <- Caaa2Coaa(effort2)

    # Allocate param to readable variable names
    q1 <- par[1] * catchability.scaling.factor1
    q2 <- par[2] * catchability.scaling.factor2
    M <- par[3]

    nb.sel.par <- length(par[-c(1,2,3)])
    selectivity.at.age1 <- par[-c(1,2,3)][seq(1,nb.sel.par/2)]
    selectivity.at.age2 <- par[-c(1,2,3)][seq(nb.sel.par/2+1,nb.sel.par)]

    # matrix of fishing mortality
    F1 <- q1 * effort1.by.cohort * outer(rep(1, nrow(effort1.by.cohort)), selectivity.at.age1)
    F2 <- q2 * effort2.by.cohort * outer(rep(1, nrow(effort2.by.cohort)), selectivity.at.age2)

    # total mortality
    Z <- M + F1 + F2

    # cumulative mortality
    cum.Z <- my.cumsum(Z)

    # Calculate the probability of observation in each interval
    prob1 <- (F1+F2)/Z * exp(-(cum.Z-Z))
    prob2 <- (F1+F2)/Z * exp(-cum.Z)
    P <- prob1-prob2

    # Using a +group in the calculation of the likelihood: essentially the boundary of the interval is \infty -> prob2 is set to 0
    if(plus.group) P[,ncol(P)] <- prob1[,ncol(P)] # change probability to account for a +group

    # discard zeroes and NA from sum of logs
    index <- which(!is.na(catch.by.cohort) & P!=0)

    # Negative log-likelihood
    -sum(catch.by.cohort[index] * log( P[index] / total.over.lines(P)[index]))
}
