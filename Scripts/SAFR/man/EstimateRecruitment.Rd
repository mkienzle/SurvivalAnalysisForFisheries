\name{EstimateRecruitment}
\alias{EstimateRecruitment}
\title{Estimate recruitment using total mortality (Z) and catch at age from a single cohort}
\description{
  This function provide an estimate of recruitment using total mortality
  (Z) and catch at age.
  The idea is that each catch at age figure provides an estimate of
  recruitment which first and second moment are calculated}
\usage{EstimateRecruitment(Z)}
\arguments{
  \item{Z}{ a numerica scalar >= 0}
}
\references{
  no reference
}
\examples{
# Suppose age varies between 0 and 10
age = seq(0,10)

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
#best.Z.estimate <- EstimateZ(catch)
#best.Rec.estimate <- EstimateRecruitment(Z=best.Z.estimate$par)
}
\keyword{misc}
