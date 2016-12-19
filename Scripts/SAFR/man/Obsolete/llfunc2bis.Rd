\name{llfunc2bis}
\alias{llfunc2bis}
\title{log-likelihood function of catch at age to estimate catchability
  and natural mortality}
\description{
  This function calculate the log-likelihood of the survival model
  assuming fishing mortality is a linear function of effort and natural
  mortality is constant.

  NOTE that the optimization does not constrains both q and M to be > 0 because we
  noticed problems of convergence of the example below on 32-bit systems
  when using method = "L-BFGS-B".

}
\usage{llfunc2bis(par, catch, effort, catchability.scaling.factor)}
\arguments{
  \item{par}{a vector of two parameters: catchability and natural mortality}
  \item{catch}{a vector of catch}
  \item{effort}{a vector of effort}
  \item{catchability.scaling.factor}{a factor to scale the parameters}
  }
\references{
  Cox (1984) - Analysis of survival data
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
lower.bound <- c(0.1,1e-2,sum(catch))
upper.bound <- c(1e2, 1, 2 * sum(catch))
best.qM.est <- optim(par = c(5, 0.5, 2*sum(catch)), lower = lower.bound, upper = upper.bound, fn = llfunc2bis, catch = catch, effort
= effort, catchability.scaling.factor = csf, method = c("L-BFGS-B"), hessian = TRUE)

errors <- sqrt(diag(solve(best.qM.est$hessian)))

### Estimate recruitment
# According to Dupont (1983) Biometrics vol. 39 No 4 pp. 1021-1033

est.rec <- sum(catch)/sum(prob.for.llfunc2bis(best.qM.est$par, effort, csf))

## And not finding a better way to calculate the uncertainty
#est.rec.limits <- c("Lower" = sum(catch)/sum(prob.for.llfunc2bis(best.qM.est$par - errors, effort, csf)), "Upper" = sum(catch)/sum(prob.for.llfunc2bis(best.qM.est$par + errors, effort, csf)))

print(" ##### ")
print(paste("Estimated catchability is", round(best.qM.est$par[1],3), "+-", round(errors[1],3), as.character(csf)))
print(paste("Estimated M is", round(best.qM.est$par[2],3), "+-", round(errors[2],3)))
#print(paste("Estimated recruitment is", est.rec, " ranging from ", est.rec.limits[1], " to ", est.rec.limits[2]))

}
\keyword{misc}
