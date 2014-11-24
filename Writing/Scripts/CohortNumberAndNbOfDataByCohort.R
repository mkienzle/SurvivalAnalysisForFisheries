
##############################################################################
# Give a number to each cohort
##############################################################################
# ARGUMENTS n: number of rows of the data matrix
#           p: number of columns of the data matrix
number.cohorts <- function(n=5, p=5){

    mat <- matrix(NA, nrow = n, ncol = p)
    for(i in 1:n){
        for(j in 1:p){
            mat[i,j] <- i - j + p
        }
    }
    
    return(mat)        
}

##############################################################################
# Give the number of data in each cohort
##############################################################################
# ARGUMENTS n: number of rows of the data matrix
#           p: number of columns of the data matrix
nb.data.per.cohort <- function(n=5, p=5){

    k <- number.cohorts(n,p)
    
    mat <- matrix(NA, nrow = n, ncol = p)
    for(i in 1:n){
        for(j in 1:p){

            if(k[i,j] < min(n,p)){ mat[i,j] <- i - j + p;}

            if(k[i,j] >= min(n,p) & k[i,j] < max(n,p)){ mat[i,j] <- min(n,p)}

            if(k[i,j] >= max(n,p)){ mat[i,j] <- j - i + n;}
        }
    }
    
    return(mat)        
}


