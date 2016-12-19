# CREATED  29 August 2014
# MODIFIED 29 August 2014

# PURPOSE estimating fishing mortality using surviva

# Some parameters
max.age = 5; nb.of.cohort = 20 # be aware that nb.of.cohort / max.age >= 3
                               # for some algorithm below to work
age = matrix(seq(0, max.age), nrow = nb.of.cohort, ncol = max.age+1, byrow=T)

# suppose there are 1000 recruits, survival is random
# vec.survival <- runif(nb.of.cohort, min = 0.1, max = 0.9)

### Calculate the number alive at age
# using constant recruitment
#cohort <- data.frame(1000 * vec.survival ^ age)
# using variable recruitment
m<-0.2
f<-runif(1, min = 0.1, max = 0.3) * seq(1, by=1, length=(max.age))
#cohort <- runif(nb.of.cohort, min = 1e2, max = 1e4) * f/(m+f) * (exp(-(m+f)) ^ age - exp(-(m+f)) ^ (age+1))
#f <- outer(rep(1,9), seq(0.1, 0.4, 0.1)); m <- matrix(0.2, nrow=9, ncol=4);
cohort <- runif(nb.of.cohort, min = 1e2, max = 1e4) * outer(rep(1,nb.of.cohort), f)/(matrix(0.2, nrow=nb.of.cohort, ncol = max.age)+outer(rep(1,nb.of.cohort), f)) * (outer(rep(1, nb.of.cohort), exp(-(m+f))) ^ age - outer(rep(1, nb.of.cohort), exp(-(m+f))) ^ (age+1))

# Name nicely you data with arbitrary cohort birth-date
dimnames(cohort)[[1]] = seq( from  = 1988, by = 1, length = nb.of.cohort)
dimnames(cohort)[[2]] = paste("age-group", seq(0,max.age), sep="")  # give age names

# Re-format data into catch at age matrix
nb.at.age = matrix(ncol = max.age + 1, nrow = nb.of.cohort)

for(i in seq(1,nb.of.cohort - 1)) {

  diag(nb.at.age[seq(i, nb.of.cohort),seq(1, min(nb.of.cohort - i +1, max.age + 1))]) <-
    as.numeric(cohort[i,seq(1, min(nb.of.cohort - i +1, max.age + 1))])

}
nb.at.age[nb.of.cohort,1] = cohort[nb.of.cohort,1] # diag deals on with matrix

# Get a complete dataset
c.catch.at.age = nb.at.age[complete.cases(nb.at.age),]


#

# Given a matrix of catch at age and their associated probability, we want
# a function that sums them along the cohorts
normalizing.matrix <- function(x){
n.row <- dim(x)[1]
n.col <- dim(x)[2]
res <- x

  # Fill the upper right corner
  for(i in seq(1, n.col-1)){
    diag(res[seq(1,i+1), seq(n.col-i,n.col) ]) = sum(diag(x[seq(1,i+1), seq(n.col-i,n.col) ]))
  }

  # Fill the lower left corner (note the overlap)
  for(i in seq(1, n.row - 1)){
    indices <- seq(i, min(i + n.col - 1, n.row))
    diag(res[indices, seq(1, length(indices))]) <-
  sum(diag(x[indices, seq(1, length(indices))])) 
}

return(res)
} # End of the function


lilylik <- function(F){
M <- matrix(0.2, nrow = nb.of.cohort - max.age, ncol = max.age+1)
F <- outer(rep(F, nb.of.cohort - max.age), seq(1, by=1, length=max.age+1))
#F <- outer(seq(0.1, by = 0.1, length = 10), seq(0.2, by = 0.2, length=4))
#F <- outer(rep(0.35, 10), rep(1,4))

Age <- matrix(seq(0,max.age), nrow = nb.of.cohort - max.age, ncol = max.age+1, byrow=T)

prob1 <- 1 - exp(- (M+F) * Age)
prob2 <- 1 - exp(- (M+F) * (Age+1))

P <- prob2 - prob1

       -sum(c.catch.at.age * log( (F/(M+F)) * (prob1 - prob2) / normalizing.matrix( (F/(M+F)) * (prob1-prob2)) ))

#print(P)
#Catch <- 1e3 * F/(M+F) * P
}

# Determine the number of cohorts in the catch at age matrix
# which will give you the number of parameter to estimate
#co.nb = dim(c.catch.at.age)[1] + dim(c.catch.at.age)[2] - 1

# Estimate cohort-specific mortality rates
#result1 <- optim(par = rep(0.2, co.nb), fn = lilylik, method = c("L-BFGS-B"),
#      lower = rep(1e-5, co.nb), upper = rep(5, co.nb), hessian = TRUE)
result1 <- optim(par = 0.2, fn = lilylik, method = c("L-BFGS-B"),
                 lower = 1e-2, upper = 1, hessian = TRUE)

print(paste("Simulated F is ", round(f,2)))
print(paste("Estimated F is ", round(result1$par,2)))
