\name{EstimateMandQ}
\alias{EstimateMandQ}
\title{Estimate total mortality (Z) using catch at age from a single cohort}
\description{
  This function provide an estimate of total mortality (Z) by maximum
  likelihood using a vector of catch at age from a cohort for each age
  group from 0 to max.age}
\usage{EstimateMandQ(catch, effort, catchability.scaling.factor)}
\arguments{
  \item{catch}{is a vector of numerics describing the number of fish
    caught in each yearly age-group. The first value reprensent fishes
    caught with age between 0 and 1 year old}
    \item{effort}{is a vector of numerics describing fishing effort
      in any specific year}
    \item{catchability.scaling.factor}{A factor to scale the parameters
      during the optimization}
}
\references{
  Quinn and Deriso (1999) - Quantitative Fish Dynamics
}
\examples{
# Suppose age varies between 0 and 10
age <- seq(0,10)

# Generate a random natural mortality
M <- runif(1, min = 1e-2, max = 0.3)

effort <- runif(length(age)-1, min = 1e3, max = 2e3)
catchability <- runif(1, min = 1/3e3, max = 1/2e3)

# Catchability scaling factor
csf <- 1e-4
F <-  catchability * effort

print(paste("Simulated q is", round(catchability / csf,3), as.character(csf)))
print(paste("Simulated M is ", round(M,3)))

N0 <- runif(1, min = 4e3, max = 1e4)
print(paste("Simulated recruitment is", round(N0)))

# Calculate number at age using a simple exponential model ( see Quinn and Deriso, 1999)
nb.at.age <- cbind(age, N0 * exp(-c(0, cumsum(M + F))))

# Calculate the total number of individual dying at age
total.death <- N0 * (exp(-c(0,cumsum(M+F)[-length(effort)])) - exp(-cumsum(M+F)))

# Number of fish dying from fishing is a fraction of total mortality
catch <- F/(M+F) * total.death

# Estimate q and M
best.qM.est <- optim(par = c(10,1), fn = llfunc2, catch = catch, effort = effort, catchability.scaling.factor = catchability.scaling.factor
      , hessian = TRUE)
= csf)

errors <- sqrt(diag(solve(best.qM.est$hessian)))

print(" ##### ")
print(paste("Estimated catchability is", round(best.qM.est$par[1],3), "+-", round(errors[1],3), as.character(csf)))
print(paste("Estimated M is", round(best.qM.est$par[2],3), "+-", round(errors[2],3)))

}
\keyword{misc}
