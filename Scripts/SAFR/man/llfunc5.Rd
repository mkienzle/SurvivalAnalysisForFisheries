\name{llfunc5}
\alias{llfunc5}
\title{log-likelihood function of catch at age matrix to estimate
  catchability, selectivity and natural mortality}
\description{
  This function calculate the log-likelihood of the survival model
  assuming fishing mortality the outer product of catchability times
  effort and selectivity and natural mortality is constant.}
\usage{llfunc5(par, catch, effort, catchability.scaling.factor)}
\arguments{
  \item{par}{a vector of two parameters: catchability and natural mortality}
  \item{catch}{a matrix containing number at age in the catch}
  \item{effort}{a matrix of effort}
  \item{catchability.scaling.factor}{a factor to scale the parameters}
  }
\references{ NONE }
\examples{
# This likelihood function constrains the selectivity on the last 2 age-groups to 1
# it doesn't help much to estimate parameters
# Simulate data
set.seed(3)
max.age <- 9
nb.of.cohort <- 50
population <- GenerateData2(max.age = max.age, nb.of.cohort = nb.of.cohort) # Generate catch using gear selectivity

# sample a fix number of fish each years
n.sample.per.year <- 2e3
nb.at.age.sample <- draw.sample(population$catch, sample.size = n.sample.per.year * (nb.of.cohort + 1 - max.age))

# Estimate parameters, fixing selectivity for the last 2 age-groups to 1
result3 <- optim(par = c(0.2,1, rep(1e-12,max.age-2)), fn = llfunc5, catch = nb.at.age.sample, effort = population$effort, catchability.scaling.factor = 1e-4, method = c("L-BFGS-B"),
       lower = c(5e-2,5e-2, rep(1e-12, max.age-2)), upper = c(10,0.5, rep(1, max.age-2)), hessian = TRUE, control = list(maxit = 1e3))
errors3 <- sqrt(abs(diag(solve(result3$hessian))))

print(cbind("Estimate" = result3$par, "Error" = errors3))

}
\keyword{misc}
