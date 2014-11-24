# Sum prob
total.over.lines <- function(mat){

    n <- nrow(mat)
p <- ncol(mat)

cum.mat <- mat
    row.sums <- rowSums(mat, na.rm = TRUE)
for(i in 1:n){
    for(j in 1:p){
        ifelse(is.na(cum.mat[i,j]),cum.mat[i,j] <- cum.mat[i,j], cum.mat[i,j] <- row.sums[i])
    }}
return(cum.mat)

}
