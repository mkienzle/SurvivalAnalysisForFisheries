# Convert a matrix of year x age into a matrix of cohort x age 
# ASSUMES age-group from 0 to n

Caaa2Coaa <- function(mat){

n <- nrow(mat)
p <- ncol(mat)

cohort <- matrix( nrow = n+p-1, ncol = p)

id.cohort <- which.cohort(mat)

for(i in 1:n){
    for(j in 1:p){
        cohort[id.cohort[i,j], j] <- mat[i,j]

    }
}

return(cohort)

}
