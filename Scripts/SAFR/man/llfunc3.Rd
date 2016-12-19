\name{llfunc3}
\alias{llfunc3}
\title{log-likelihood function of catch at age matrix to estimate
  catchability, selectivity and natural mortality from a matrix of catch at age}
\description{
  This function calculate the log-likelihood of the survival model
  assuming fishing mortality the outer product of catchability times
  effort and selectivity and natural mortality is constant.}
\usage{llfunc3(par, catch, effort, selectivity.at.age, catchability.scaling.factor,plus.group)}
\arguments{
  \item{par}{a vector of two parameters: catchability and natural mortality}
  \item{catch}{a matrix containing number at age in the catch}
  \item{effort}{a matrix of effort}
  \item{selectivity.at.age}{a vector of selectivity at age bound between
  0 and 1}
  \item{catchability.scaling.factor}{a factor to scale the parameters}
  \item{plus.group}{a boolean indicating whether the data for the last age-group is the sum of observation for this age-group and all olders one or not}
  }
\references{ NONE }
\examples{

# First example, estimate mortality rates assuming selectivity known exactly
set.seed(3)
max.age <- 9
nb.of.cohort <- 30
population <- GenerateData2(max.age = max.age, nb.of.cohort = nb.of.cohort) # Generate catch using gear selectivity

# sample a fix number of fish each years
n.sample.per.year <- 1e3
nb.at.age.sample <- draw.sample(population$catch, sample.size = n.sample.per.year * (nb.of.cohort + 1 - max.age))


# Estimate assuming you know selectivity
result <- optim(par = c(0.2,1), fn = llfunc3, catch = nb.at.age.sample, effort = population$effort, catchability.scaling.factor = 1e-4, selectivity.at.age =  c(0,0,seq(1/(max.age - 3), (max.age - 4)/(max.age - 3), length = max.age - 4), 1,1), hessian = TRUE)

errors <- sqrt(diag(solve(result$hessian)))

print(paste("Estimated catchability is", round(result$par[1],3), "+-", round(errors[1],3), " x 10^-4"))
print(paste("Estimated M is", round(result$par[2],3), "+-", round(errors[2],3)))


# Second example to show how to use a +group
set.seed(12)
max.age <- 25
nb.of.cohort <- 75
population <- GenerateData2(max.age = max.age, nb.of.cohort = nb.of.cohort) # Generate catch using gear selectivity

# sample a fix number of fish each years
n.sample.per.year <- 1e3
nb.at.age.sample <- draw.sample(population$catch, sample.size = n.sample.per.year * (nb.of.cohort + 1 - max.age))

# Estimate assuming you know selectivity
result <- optim(par = c(0.2,1), fn = llfunc3, catch = nb.at.age.sample, effort = population$effort, catchability.scaling.factor = 1e-4, selectivity.at.age =  c(0,0,seq(1/(max.age - 3), (max.age - 4)/(max.age - 3), length = max.age - 4), 1,1), hessian = TRUE)

errors <- sqrt(diag(solve(result$hessian)))

print(paste("Estimated catchability is", round(result$par[1],3), "+-", round(errors[1],3), " x 10^-4"))
print(paste("Estimated M is", round(result$par[2],3), "+-", round(errors[2],3)))

# Using a +group
ap <- 20
nb.at.age.sample2 <- nb.at.age.sample[,1:ap]; nb.at.age.sample2[,ap] <- rowSums(nb.at.age.sample[,ap:max.age])
effort2 <- population$effort[,1:ap];

result2 <- optim(par = c(0.2,1), fn = llfunc3, catch = nb.at.age.sample2, effort = effort2, catchability.scaling.factor = 1e-4, selectivity.at.age =  c(0,0,seq(1/(max.age - 3), (max.age - 4)/(max.age - 3), length = max.age - 4), 1,1)[1:ap], plus.group = TRUE, hessian = TRUE)

errors2 <- sqrt(diag(solve(result2$hessian)))

print(paste("Estimated catchability is", round(result2$par[1],3), "+-", round(errors2[1],3), " x 10^-4"))
print(paste("Estimated M is", round(result2$par[2],3), "+-", round(errors2[2],3)))

}
\keyword{misc}
