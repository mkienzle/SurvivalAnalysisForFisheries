# CREATED   3 Sep. 2014
# MODIFIED 11 Nov. 2014

# AUTHOR marco.kienzle@gmail.com;

# logistic function
logistic <- function(a,b,x){
1 / (1  + exp( a - b * x));
}

# Plot catch by cohort
plot.catch.by.cohort <- function(df){
    # df is a catch at age matrix

    catch.by.cohort <- Caaa2Coaa(df)
    
    library(reshape2)
    library(ggplot2)

    mdf <- melt(catch.by.cohort)
    dimnames(mdf)[[2]] <- c("Cohort", "Age", "value")
    str(mdf)
    p1 <- ggplot(mdf, aes(x=Age, y=value)) + geom_point() + facet_wrap(~Cohort)
    print(p1)
}

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

# Generate data
GenerateData <- function(max.age = 10, nb.of.cohort = 20){

# Some parameters
#max.age <- 10; nb.of.cohort <- 20 # be aware that nb.of.cohort / max.age >= 3
                               # for some algorithm below to work
#age <- matrix(seq(0, max.age), nrow = nb.of.cohort, ncol = max.age+1, byrow=T)

# Fishing mortality as the product of catchability and yearly effort
catchability <- runif(1, min = 1, max = 10)
#catchability <- 1
catchability.mf <- 1e-4

print(paste("Simulated catchability is ", round(catchability,2), "x 10^-4"))

yearly.effort <- runif(nb.of.cohort, min = 1e3, max = 1.75e3)
#yearly.effort <- rep(1e3, nb.of.cohort)
effort.faced.by.each.cohort <- matrix(nrow = nb.of.cohort, ncol = max.age)
for(i in 1:nb.of.cohort) effort.faced.by.each.cohort[i,] <- yearly.effort[seq(i, i + max.age - 1)]

#selectivity.at.age <- c(0,0,0.2,0.4,0.6,0.8,1,1,1,1)
F <- catchability * catchability.mf * effort.faced.by.each.cohort #* outer(rep(1, nb.of.cohort), selectivity.at.age)

M <- runif(1, min = 0.05, max = 0.5)
#M <- 0.2
print(paste("Simulated natural mortality is", round(M,3)))

# Recruitment: N(0)
Recruitment <- runif(nb.of.cohort, min = 1e3, max = 1e4)

# Nb at age in cohorts
cohort <- matrix(nrow = nb.of.cohort, ncol = max.age + 1)
cohort[,1] <- Recruitment
for(i in 1:nrow(cohort)) cohort[i,] <- cohort[i,1] * exp(-c(0, cumsum(M + F[i,])))

# Number that died
cohort.nb.dead <- cohort[,1:max.age] - cohort[,seq(2, max.age+1)]

# Catch
cohort.nb.caught <- F / (M+F) * cohort.nb.dead

#
#nb.at.age <- cbind(age, Recruitment * exp(-c(0, cumsum(M + F))))

# suppose there are 1000 recruits, 50% survive per year
#vec.survival = rep(0.5, nb.of.cohort) #

# suppose there are 1000 recruits, survival increase with cohort number
#vec.survival = seq(0.2, 0.8, length = nb.of.cohort)

# suppose there are 1000 recruits, survival is random
# vec.survival <- runif(nb.of.cohort, min = 0.1, max = 0.9)

### Calculate the number alive at age
# using constant recruitment
#cohort <- data.frame(1000 * vec.survival ^ age)
# using variable recruitment
#cohort <- runif(nb.of.cohort, min = 1e2, max = 1e4) * vec.survival ^ age

# Name nicely you data with arbitrary cohort birth-date
dimnames(cohort)[[1]] = seq( from  = 1988, by = 1, length = nb.of.cohort)
dimnames(cohort)[[2]] = paste("age-group", seq(0,max.age), sep="")  # give age names


# Re-format data into catch at age matrix
catch.at.age <- matrix(ncol = max.age, nrow = nb.of.cohort)
effort.at.age <- matrix(ncol = max.age, nrow = nb.of.cohort)

for(i in seq(1,nb.of.cohort - 1)) {

  diag(catch.at.age[seq(i, nb.of.cohort),seq(1, min(nb.of.cohort - i +1, max.age))]) <-
    as.numeric(cohort.nb.caught[i,seq(1, min(nb.of.cohort - i +1, max.age))])

  diag(effort.at.age[seq(i, nb.of.cohort),seq(1, min(nb.of.cohort - i +1, max.age))]) <-
    as.numeric(effort.faced.by.each.cohort[i,seq(1, min(nb.of.cohort - i +1, max.age))])
  
}
catch.at.age[nb.of.cohort,1] = cohort.nb.caught[nb.of.cohort,1] # diag deals on with matrix
effort.at.age[nb.of.cohort,1] = effort.faced.by.each.cohort[nb.of.cohort,1] # diag deals on with matrix

# Get a complete dataset
c.catch.at.age = catch.at.age[complete.cases(catch.at.age),]
effort.at.age = effort.at.age[complete.cases(effort.at.age),]

return(list(catch = c.catch.at.age, effort = effort.at.age))
}

# Generate data
GenerateData2 <- function(max.age = 10, nb.of.cohort = 20, catchability.range = c(3,5), effort.range = c(3e3, 5e3), nat.mort.range = c(0.2, 0.5), recruitment.range=c(5e3, 1e6)){

# Some parameters
#max.age <- 10; nb.of.cohort <- 20 # be aware that nb.of.cohort / max.age >= 3
                               # for some algorithm below to work
#age <- matrix(seq(0, max.age), nrow = nb.of.cohort, ncol = max.age+1, byrow=T)

# Fishing mortality as the product of catchability and yearly effort
catchability <- runif(1, min = catchability.range[1], max = catchability.range[2])
#catchability <- 1
catchability.mf <- 1e-4

print(paste("Simulated catchability is ", round(catchability,2), "x 10^-4"))

yearly.effort <- runif(nb.of.cohort, min = effort.range[1], max = effort.range[2])
#yearly.effort <- rep(1e3, nb.of.cohort)
effort.faced.by.each.cohort <- matrix(nrow = nb.of.cohort, ncol = max.age)
for(i in 1:nb.of.cohort) effort.faced.by.each.cohort[i,] <- yearly.effort[seq(i, i + max.age - 1)]

n <- max.age-1
#selectivity.at.age <- c(0,0,seq(1/(max.age - 3), (max.age - 4)/(max.age - 3), length = max.age - 4), 1,1)
selectivity.at.age <- c(0,0,seq(1/(max.age - 5), (max.age - 6)/(max.age - 5), length = max.age - 8), rep(1,6))
F <- catchability * catchability.mf * effort.faced.by.each.cohort * outer(rep(1, nb.of.cohort), selectivity.at.age)

M <- runif(1, min = nat.mort.range[1], max = nat.mort.range[2])
#M <- 0.2
print(paste("Simulated natural mortality is", round(M,3)))

# Recruitment: N(0)
Recruitment <- runif(nb.of.cohort, min = recruitment.range[1], max = recruitment.range[2])

# Nb at age in cohorts
cohort <- matrix(nrow = nb.of.cohort, ncol = max.age + 1)
cohort[,1] <- Recruitment
for(i in 1:nrow(cohort)) cohort[i,] <- cohort[i,1] * exp(-c(0, cumsum(M + F[i,])))

# Number that died
cohort.nb.dead <- cohort[,1:max.age] - cohort[,seq(2, max.age+1)]

# Catch
cohort.nb.caught <- F / (M+F) * cohort.nb.dead

#
#nb.at.age <- cbind(age, Recruitment * exp(-c(0, cumsum(M + F))))

# suppose there are 1000 recruits, 50% survive per year
#vec.survival = rep(0.5, nb.of.cohort) #

# suppose there are 1000 recruits, survival increase with cohort number
#vec.survival = seq(0.2, 0.8, length = nb.of.cohort)

# suppose there are 1000 recruits, survival is random
# vec.survival <- runif(nb.of.cohort, min = 0.1, max = 0.9)

### Calculate the number alive at age
# using constant recruitment
#cohort <- data.frame(1000 * vec.survival ^ age)
# using variable recruitment
#cohort <- runif(nb.of.cohort, min = 1e2, max = 1e4) * vec.survival ^ age

# Name nicely you data with arbitrary cohort birth-date
dimnames(cohort)[[1]] = seq( from  = 1988, by = 1, length = nb.of.cohort)
dimnames(cohort)[[2]] = paste("age-group", seq(0,max.age), sep="")  # give age names


# Re-format data into catch at age matrix
catch.at.age <- matrix(ncol = max.age, nrow = nb.of.cohort)
effort.at.age <- matrix(ncol = max.age, nrow = nb.of.cohort)

for(i in seq(1,nb.of.cohort - 1)) {

  diag(catch.at.age[seq(i, nb.of.cohort),seq(1, min(nb.of.cohort - i +1, max.age))]) <-
    as.numeric(cohort.nb.caught[i,seq(1, min(nb.of.cohort - i +1, max.age))])

  diag(effort.at.age[seq(i, nb.of.cohort),seq(1, min(nb.of.cohort - i +1, max.age))]) <-
    as.numeric(effort.faced.by.each.cohort[i,seq(1, min(nb.of.cohort - i +1, max.age))])
  
}
catch.at.age[nb.of.cohort,1] = cohort.nb.caught[nb.of.cohort,1] # diag deals on with matrix
effort.at.age[nb.of.cohort,1] = effort.faced.by.each.cohort[nb.of.cohort,1] # diag deals on with matrix

# Get a complete dataset
c.catch.at.age = catch.at.age[complete.cases(catch.at.age),]
effort.at.age = effort.at.age[complete.cases(effort.at.age),]

return(list(catch = c.catch.at.age, effort = effort.at.age, M = M, F = F))
}
# Generate data
GenerateData3 <- function(max.age = 10, nb.of.cohort = 20, catchability.range = c(3,10), effort.range = c(3e3, 5e3), nat.mort.range = c(0.1, 0.8), recruitment.range=c(1e6, 1e7), log.para.range = c(8,12), log.parb.range = c(1,3), verbose = FALSE){

# Some parameters
#max.age <- 10; nb.of.cohort <- 20 # be aware that nb.of.cohort / max.age >= 3
                               # for some algorithm below to work
#age <- matrix(seq(0, max.age), nrow = nb.of.cohort, ncol = max.age+1, byrow=T)

# Fishing mortality as the product of catchability and yearly effort
catchability <- runif(1, min = catchability.range[1], max = catchability.range[2])
#catchability <- 1
catchability.mf <- 1e-4

if(verbose) print(paste("Simulated catchability is ", round(catchability,2), "x 10^-4"))

yearly.effort <- runif(nb.of.cohort, min = effort.range[1], max = effort.range[2])
#yearly.effort <- rep(1e3, nb.of.cohort)
effort.faced.by.each.cohort <- matrix(nrow = nb.of.cohort, ncol = max.age)
for(i in 1:nb.of.cohort) effort.faced.by.each.cohort[i,] <- yearly.effort[seq(i, i + max.age - 1)]

n <- max.age-1
#selectivity.at.age <- c(0,0,seq(1/(max.age - 3), (max.age - 4)/(max.age - 3), length = max.age - 4), 1,1)
#selectivity.at.age <- c(0,0,seq(1/(max.age - 5), (max.age - 6)/(max.age - 5), length = max.age - 8), rep(1,6))
log.para <- runif(1, min = log.para.range[1], max=log.para.range[2])
if(verbose) print(paste("Logistic parameter a is", log.para))
log.parb <- runif(1, min = log.parb.range[1], max=log.parb.range[2])
if(verbose) print(paste("Logistic parameter b is", log.parb))

selectivity.at.age <- logistic(log.para,log.parb,seq(1,max.age))

F <- catchability * catchability.mf * effort.faced.by.each.cohort * outer(rep(1, nb.of.cohort), selectivity.at.age)

M <- runif(1, min = nat.mort.range[1], max = nat.mort.range[2])
#M <- 0.2
if(verbose) print(paste("Simulated natural mortality is", round(M,3)))

# Recruitment: N(0)
Recruitment <- runif(nb.of.cohort, min = recruitment.range[1], max = recruitment.range[2])

# Nb at age in cohorts
cohort <- matrix(nrow = nb.of.cohort, ncol = max.age + 1)
cohort[,1] <- Recruitment
for(i in 1:nrow(cohort)) cohort[i,] <- cohort[i,1] * exp(-c(0, cumsum(M + F[i,])))

# Number that died
cohort.nb.dead <- cohort[,1:max.age] - cohort[,seq(2, max.age+1)]

# Catch
cohort.nb.caught <- F / (M+F) * cohort.nb.dead

#
#nb.at.age <- cbind(age, Recruitment * exp(-c(0, cumsum(M + F))))

# suppose there are 1000 recruits, 50% survive per year
#vec.survival = rep(0.5, nb.of.cohort) #

# suppose there are 1000 recruits, survival increase with cohort number
#vec.survival = seq(0.2, 0.8, length = nb.of.cohort)

# suppose there are 1000 recruits, survival is random
# vec.survival <- runif(nb.of.cohort, min = 0.1, max = 0.9)

### Calculate the number alive at age
# using constant recruitment
#cohort <- data.frame(1000 * vec.survival ^ age)
# using variable recruitment
#cohort <- runif(nb.of.cohort, min = 1e2, max = 1e4) * vec.survival ^ age

# Name nicely you data with arbitrary cohort birth-date
dimnames(cohort)[[1]] = seq( from  = 1988, by = 1, length = nb.of.cohort)
dimnames(cohort)[[2]] = paste("age-group", seq(0,max.age), sep="")  # give age names


# Re-format data into catch at age matrix
catch.at.age <- matrix(ncol = max.age, nrow = nb.of.cohort)
effort.at.age <- matrix(ncol = max.age, nrow = nb.of.cohort)

for(i in seq(1,nb.of.cohort - 1)) {

  diag(catch.at.age[seq(i, nb.of.cohort),seq(1, min(nb.of.cohort - i +1, max.age))]) <-
    as.numeric(cohort.nb.caught[i,seq(1, min(nb.of.cohort - i +1, max.age))])

  diag(effort.at.age[seq(i, nb.of.cohort),seq(1, min(nb.of.cohort - i +1, max.age))]) <-
    as.numeric(effort.faced.by.each.cohort[i,seq(1, min(nb.of.cohort - i +1, max.age))])
  
}
catch.at.age[nb.of.cohort,1] = cohort.nb.caught[nb.of.cohort,1] # diag deals on with matrix
effort.at.age[nb.of.cohort,1] = effort.faced.by.each.cohort[nb.of.cohort,1] # diag deals on with matrix

# Get a complete dataset
c.catch.at.age = catch.at.age[complete.cases(catch.at.age),]
effort.at.age = effort.at.age[complete.cases(effort.at.age),]

return(list(catch = c.catch.at.age, effort = effort.at.age, catchability = catchability * catchability.mf, Nat.Mort = M, logistic.par = c(log.para, log.parb), Recruitment = Recruitment, M = M, F = F))
}


    
# A function that identifies the cohort in a matrix of year x age
# ASSUMES age-group from 0 to n
which.cohort <- function(mat){
n <- nrow(mat)
p <- ncol(mat)

new.mat <- matrix(nrow = n, ncol = p)

#counter <- 1
#for(j in p:1){
#        new.mat[,j] <- seq(counter, by = 1, length = n)
#        counter = counter+1
#    }

for(i in 1:n){
    for(j in 1:p){
        new.mat[i,j] <- i - j + p
    }
}

return(new.mat)
}

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

### Draw n sample in any year from the population

# ARGUMENTS pop.matrix: a matrix (nxp) of number at age (p columns) in years (n rows)
#           sample.size: the size of the desired sample

draw.sample2 <- function(pop.matrix, sample.size.each.year = 10){

    # get the dimensions
    n <- nrow(pop.matrix)
    p <- ncol(pop.matrix)

    result <- matrix(NA, nrow = n, ncol = p)
    
    # Loop over rows
    for(i in 1:n){
        
    # take a sample of the indices of the matrix using number at age as a weight
    my.sample <- sample(1:p, size = sample.size.each.year, replace = TRUE, prob = pop.matrix[i,])

    # build a vector of frequencies of indices
    freq <- table(factor(my.sample, levels = 1:p))

    result[i,] <- freq
}    

    return(result)

}

### Draw different n sample in any year from the population

# ARGUMENTS pop.matrix: a matrix (nxp) of number at age (p columns) in years (n rows)
#           sample.size: the size of the desired sample

draw.sample3 <- function(pop.matrix, vec){

    # get the dimensions
    n <- nrow(pop.matrix)
    p <- ncol(pop.matrix)

    if(length(vec) != n){stop(paste("vector number of sample should be of length", n))}
       
    result <- matrix(NA, nrow = n, ncol = p)
    
    # Loop over rows
    for(i in 1:n){
        
    # take a sample of the indices of the matrix using number at age as a weight
    my.sample <- sample(1:p, size = vec[i], replace = TRUE, prob = pop.matrix[i,])

    # build a vector of frequencies of indices
    freq <- table(factor(my.sample, levels = 1:p))

    result[i,] <- freq
}    

    return(result)

}
