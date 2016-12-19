# Convert a matrix of cohort catch at age (n+p-1 x p) into catch at age in each ear (n x p) 
# ASSUMES age-group from 0 to n

Coaa2Caaa <- function(cohort.mat){

    c.n <- nrow(cohort.mat)
    p <- ncol(cohort.mat)

    n <- c.n - p + 1
    caa.mat <- matrix( nrow = n, ncol = p)

    id.cohort <- which.cohort(caa.mat)

    for(i in 1:n){
    for(j in 1:p){
        caa.mat[i,j] <- cohort.mat[id.cohort[i,j], j]

    }
}

    return(caa.mat)
}
