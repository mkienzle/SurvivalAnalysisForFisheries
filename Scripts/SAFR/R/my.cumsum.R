# a cumsum that omit NA
my.cumsum <- function(mat){

n <- nrow(mat)
p <- ncol(mat)

cum.mat <- mat
for(i in 1:n){
    for(j in 2:p){
        cum.mat[i,j] <- ifelse(is.na(cum.mat[i,j-1]),0,cum.mat[i,j-1]) + cum.mat[i,j]
    }}
return(cum.mat)
}
