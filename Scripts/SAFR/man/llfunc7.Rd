\name{llfunc7}
\alias{llfunc7}
\title{log-likelihood function of catch at age matrix to estimate
  catchability, selectivity [assumed logistic] and natural mortality}
\description{
  This function calculate the log-likelihood of the survival model
  assuming fishing mortality the outer product of catchability times
  effort and selectivity and natural mortality is constant.}
\usage{llfunc7(par, catch, effort, catchability.scaling.factor)}
\arguments{
  \item{par}{a vector of four parameters: catchability, natural mortality and 2 parameters of the logistic function for gear selectivity}
  \item{catch}{a matrix containing number at age in the catch}
  \item{effort}{a matrix of effort}
  \item{catchability.scaling.factor}{a factor to scale the parameters}
  }
\references{ NONE }
\examples{
max.age <- 9
nb.of.cohort <- 30
 
population <- GenerateData3(max.age = max.age, nb.of.cohort = nb.of.cohort, verbose = TRUE)

#############################################################################
# Simulate sampling
#############################################################################

# sample a fix number of fish each years
n.sample.per.year <- 2e3
nb.at.age.sample <- draw.sample(population$catch, sample.size = n.sample.per.year * (nb.of.cohort + 1 - max.age))
# Estimate assuming you know selectivity
lower.bound <- c(5e-2,1e-2,1,1);upper.bound <- c(15,1,20,20)

csf <- 1e-4 # catchability scaling factor

result <- optim(par = c(0.2,0.5, 10, 2), fn = llfunc7, catch = nb.at.age.sample, effort = population$effort, catchability.scaling.factor = csf, method = c("L-BFGS-B"),
       lower = lower.bound, upper = upper.bound, hessian = TRUE)
errors <- sqrt(diag(solve(result$hessian)))

res <- cbind("Estimate" = result$par, "Errors" = errors);
dimnames(res)[[1]] <- c("Est. catchability", "Est. natural mort.", "Est. logistic par a", "Est. logistic par b")
print(res)

# Calculate probability of being caught
p <- prob.for.llfunc7(result$par, population$catch, population$effort, catchability.scaling.factor = csf)

# An estimate of recruitment
rec <- rowSums(Caaa2Coaa(population$catch), na.rm = TRUE) / rowSums(p, na.rm = TRUE)
ind.rec <- Caaa2Coaa(population$catch) / p
var.ind.rec <- 1/ncol(ind.rec) * rowSums((ind.rec - outer(rec, rep(1, ncol(ind.rec))))^2, na.rm = TRUE)

par(mfrow=c(1,2))
plot(population$Rec[9:30], rec[9:30]); abline(0,1)
segments(population$Rec[9:30], rec[9:30] + sqrt(var.ind.rec[9:30]), population$Rec[9:30], rec[9:30] - sqrt(var.ind.rec[9:30]))

plot(1:22, population$Rec[9:30], type = "b", ylim = c(0.9 * min(c(rec[9:30], population$Rec[9:30])), 1.1 * max(c(rec[9:30], population$Rec[9:30]))))
points(1:22, rec[9:30], type = "b", pch = 19)

}
\keyword{misc}
