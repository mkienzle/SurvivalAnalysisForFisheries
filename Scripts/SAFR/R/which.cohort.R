# A function that numbers cohorts in a matrix of year x age
# starting from top-right towards bottom-left
# ASSUMES age-group from 0 to n
which.cohort <- function(mat){
n <- nrow(mat)
p <- ncol(mat)

new.mat <- matrix(nrow = n, ncol = p)

for(i in 1:n){
    for(j in 1:p){
        new.mat[i,j] <- i - j + p
    }
}

return(new.mat)
}
