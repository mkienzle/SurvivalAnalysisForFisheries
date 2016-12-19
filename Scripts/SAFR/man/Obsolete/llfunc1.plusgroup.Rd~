\name{llfunc1}
\alias{llfunc1}
\title{log-likelihood function of catch at age and total mortality (Z)
  written to Z}
\description{
  This function calculate the log-likelihood of the survival model
  assuming constant mortality rate (Z) given catch at age.

  NOTE that the optimization does not constrains Z > 0 because we
  noticed problems of convergence of the example below on 32-bit systems
  when using method = "L-BFGS-B".

}
\usage{llfunc1(Z, catch)}
\arguments{
  \item{Z}{a positive or null scalar giving the constant mortality rate
    (units 1/year)}
  \item{catch}{a vector of number of individual caught at age. The first
    value represent a number of animal between age 0 and 1, the second
    between 1 and 2, etc...}
  }
\references{
  Cox (1984) - Analysis of survival data
}
\examples{
# Suppose age varies between 0 and 10
age <- seq(0,10)

# Suppose you have a M=0.105, F and N0 are arbitrary (randomly generated)
M <- 0.105
F <- runif(1, min = 0.1, max = 3)
print(paste("Simulated Z is", round(M+F,3)))

# Generate a random recruitment
N0 <- runif(1, min = 1e3, max = 1e4)
print(paste("Simulated recruitment", round(N0)))

# Calculate number at age using a simple exponential model ( see Quinn and Deriso, 1999)
nb.at.age <- cbind(age, N0 * exp(-(M+ F)) ^ age)

# Calculate the total number of individual dying at age
total.death <- N0 * (exp(-(M+F) * age) - exp(-(M+F) * (age+1)))

# And the fraction dying from fishing 
catch <- F/(M+F) * total.death

# Estimate Z
result <- optim(par = c(0.1), fn = llfunc1, catch = catch, method = c("L-BFGS-B"),
      lower = c(1e-2), upper = c(3), hessian = TRUE)
print(paste("Estimated Z is", round(result$par,3), "+-", round(sqrt(diag(solve(result$hessian))),3)))


}
\keyword{misc}
