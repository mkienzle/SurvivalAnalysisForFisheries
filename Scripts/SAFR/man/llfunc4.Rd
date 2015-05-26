\name{llfunc4}
\alias{llfunc4}
\title{log-likelihood function of catch at age matrix to estimate
  catchability, selectivity and natural mortality}
\description{
  This function calculate the log-likelihood of the survival model
  assuming fishing mortality the outer product of catchability times
  effort and selectivity and natural mortality is constant.}
\usage{llfunc4(par, catch, effort, catchability.scaling.factor)}
\arguments{
  \item{par}{a vector of parameters: catchability, natural mortality and 1 gear selectivity for each age-group}
  \item{catch}{a matrix containing number at age in the catch}
  \item{effort}{a matrix of effort}
  \item{catchability.scaling.factor}{a factor to scale the parameters}
  }
\references{ NONE }
\examples{
# without a function for gear selectivity, it is very difficult (impossible) to estimate parameters of interest
# we need substantially more data
# Simulate data
set.seed(3)
max.age <- 9
nb.of.cohort <- 50
population <- GenerateData2(max.age = max.age, nb.of.cohort = nb.of.cohort) # Generate catch using gear selectivity

# sample a fix number of fish each years
n.sample.per.year <- 2e3
nb.at.age.sample <- draw.sample(population$catch, sample.size = n.sample.per.year * (nb.of.cohort + 1 - max.age))

# Estimate parameters
result2 <- optim(par = c(0.2,1, c(rep(1e-12,5), rep(1, max.age-5))), fn = llfunc4, catch = nb.at.age.sample, effort = population$effort, catchability.scaling.factor = 1e-4, method = c("L-BFGS-B"),
       lower = c(5e-2,5e-2, rep(1e-12, max.age)), upper = c(10,0.5, rep(1, max.age)), hessian = TRUE, control = list(maxit = 1e3))
errors2 <- sqrt(abs(diag(solve(result2$hessian))))

print(cbind("Estimate" = result2$par, "Error" = errors2))
}
\keyword{misc}
