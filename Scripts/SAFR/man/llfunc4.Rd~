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
  \item{par}{a vector of two parameters: catchability and natural mortality}
  \item{catch}{a vector of catch}
  \item{effort}{a vector of effort}
  \item{catchability.scaling.factor}{a factor to scale the parameters}
  }
\references{ NONE }
\examples{
# Simulate data
set.seed(3)
max.age <- 9
sim <- GenerateData2(max.age = max.age, nb.of.cohort = 30) # Generate catch using gear selectivity

# Estimate parameters
result2 <- optim(par = c(0.2,1, c(rep(1e-12,5), rep(1, max.age-5))), fn = llfunc4, catch = sim$catch, effort = sim$effort, catchability.scaling.factor = 1e-4, method = c("L-BFGS-B"),
       lower = c(5e-2,5e-2, rep(1e-12, max.age)), upper = c(10,0.5, rep(1, max.age)), hessian = TRUE, control = list(maxit = 1e3))
errors2 <- sqrt(abs(diag(solve(result2$hessian))))

print(cbind("Estimate" = result2$par, "Error" = errors2))
}
\keyword{misc}
