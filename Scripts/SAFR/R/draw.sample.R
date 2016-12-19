### Draw sample from the population

# ARGUMENTS pop.matrix: a matrix (nxp) of number at age (p columns) in years (n rows)
#           sample.size: the size of the desired sample

draw.sample <- function(pop.matrix, sample.size = 10){

    # get the dimensions
    n <- nrow(pop.matrix)
    p <- ncol(pop.matrix)

    # unfold the matrix
    unfolded.matrix <- cbind(year=rep(1:n,p), age.group= rep(1:p,each=n), nb.at.age = c(pop.matrix))

    # take a sample of the indices of the matrix using number at age as a weight
    my.sample <- sample(1:nrow(unfolded.matrix), size = sample.size, replace = TRUE, prob = unfolded.matrix[,"nb.at.age"])

    # build a vector of frequencies of indices
    freq <- table(factor(my.sample, levels = 1:nrow(unfolded.matrix)))

    # reshape into a matrix of number at age x years
    my.sample <- tapply(as.numeric(freq), list(unfolded.matrix[,1], unfolded.matrix[,2]), I)
    return(my.sample)

}
