pkgname <- "SAFR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('SAFR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Caaa2Coaa")
### * Caaa2Coaa

flush(stderr()); flush(stdout())

### Name: Caaa2Coaa
### Title: Convert catch-at-age to cohort-at-age
### Aliases: Caaa2Coaa
### Keywords: misc

### ** Examples

nb.at.age <- matrix(sample(1:10, 40, replace = TRUE), nrow = 10, ncol = 4)
Caaa2Coaa(nb.at.age)



cleanEx()
nameEx("Coaa2Caaa")
### * Coaa2Caaa

flush(stderr()); flush(stdout())

### Name: Coaa2Caaa
### Title: Convert cohort-at-age to catch-at-age - the opposite of
###   Caaa2Coaa
### Aliases: Coaa2Caaa
### Keywords: misc

### ** Examples

(nb.at.age <- matrix(sample(1:10, 40, replace = TRUE), nrow = 10, ncol = 4))
tmp <- Caaa2Coaa(nb.at.age)
Coaa2Caaa(tmp)



cleanEx()
nameEx("EstimateMandQ")
### * EstimateMandQ

flush(stderr()); flush(stdout())

### Name: EstimateMandQ
### Title: Estimate total mortality (Z) using catch at age from a single
###   cohort
### Aliases: EstimateMandQ
### Keywords: misc

### ** Examples

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
best.qM.est <- EstimateMandQ(catch, effort, catchability.scaling.factor
= csf)

errors <- sqrt(diag(solve(best.qM.est$hessian)))

print(" ##### ")
print(paste("Estimated catchability is", round(best.qM.est$par[1],3), "+-", round(errors[1],3), as.character(csf)))
print(paste("Estimated M is", round(best.qM.est$par[2],3), "+-", round(errors[2],3)))




cleanEx()
nameEx("EstimateRecruitment")
### * EstimateRecruitment

flush(stderr()); flush(stdout())

### Name: EstimateRecruitment
### Title: Estimate recruitment using total mortality (Z) and catch at age
###   from a single cohort
### Aliases: EstimateRecruitment
### Keywords: misc

### ** Examples

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



cleanEx()
nameEx("EstimateZ")
### * EstimateZ

flush(stderr()); flush(stdout())

### Name: EstimateZ
### Title: Estimate total mortality (Z) using catch at age from a single
###   cohort
### Aliases: EstimateZ
### Keywords: misc

### ** Examples

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
best.Z.est <- EstimateZ(catch)



cleanEx()
nameEx("llfunc1")
### * llfunc1

flush(stderr()); flush(stdout())

### Name: llfunc1
### Title: log-likelihood function of catch at age and total mortality (Z)
###   written to Z
### Aliases: llfunc1
### Keywords: misc

### ** Examples

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





cleanEx()
nameEx("llfunc2")
### * llfunc2

flush(stderr()); flush(stdout())

### Name: llfunc2
### Title: log-likelihood function of catch at age to estimate catchability
###   and natural mortality
### Aliases: llfunc2
### Keywords: misc

### ** Examples

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
best.qM.est <- optim(par = c(10,1), fn = llfunc2, catch = catch, effort
= effort, catchability.scaling.factor = csf, hessian = TRUE)

errors <- sqrt(diag(solve(best.qM.est$hessian)))

print(" ##### ")
print(paste("Estimated catchability is", round(best.qM.est$par[1],3), "+-", round(errors[1],3), as.character(csf)))
print(paste("Estimated M is", round(best.qM.est$par[2],3), "+-", round(errors[2],3)))




cleanEx()
nameEx("llfunc3")
### * llfunc3

flush(stderr()); flush(stdout())

### Name: llfunc3
### Title: log-likelihood function of catch at age matrix to estimate
###   catchability, selectivity and natural mortality
### Aliases: llfunc3
### Keywords: misc

### ** Examples

# Simulate data
set.seed(3)
max.age <- 9
sim <- GenerateData2(max.age = max.age, nb.of.cohort = 30) # Generate catch using gear selectivity

# Estimate assuming you know selectivity
result <- optim(par = c(0.2,1), fn = llfunc3, catch = sim$catch, effort = sim$effort, catchability.scaling.factor = 1e-4, selectivity.at.age =  c(0,0,seq(1/(max.age - 3), (max.age - 4)/(max.age - 3), length = max.age - 4), 1,1), hessian = TRUE)
 errors <- sqrt(diag(solve(result$hessian)))

print(paste("Estimated catchability is", round(result$par[1],3), "+-", round(errors[1],3), " x 10^-4"))
print(paste("Estimated M is", round(result$par[2],3), "+-", round(errors[2],3)))



cleanEx()
nameEx("llfunc4")
### * llfunc4

flush(stderr()); flush(stdout())

### Name: llfunc4
### Title: log-likelihood function of catch at age matrix to estimate
###   catchability, selectivity and natural mortality
### Aliases: llfunc4
### Keywords: misc

### ** Examples

# Simulate data
set.seed(3)
max.age <- 9
sim <- GenerateData2(max.age = max.age, nb.of.cohort = 30) # Generate catch using gear selectivity

# Estimate parameters
result2 <- optim(par = c(0.2,1, c(rep(1e-12,5), rep(1, max.age-5))), fn = llfunc4, catch = sim$catch, effort = sim$effort, catchability.scaling.factor = 1e-4, method = c("L-BFGS-B"),
       lower = c(5e-2,5e-2, rep(1e-12, max.age)), upper = c(10,0.5, rep(1, max.age)), hessian = TRUE, control = list(maxit = 1e3))
errors2 <- sqrt(abs(diag(solve(result2$hessian))))

print(cbind("Estimate" = result2$par, "Error" = errors2))



cleanEx()
nameEx("llfunc5")
### * llfunc5

flush(stderr()); flush(stdout())

### Name: llfunc5
### Title: log-likelihood function of catch at age matrix to estimate
###   catchability, selectivity and natural mortality
### Aliases: llfunc5
### Keywords: misc

### ** Examples

# Simulate data
set.seed(3)
max.age <- 9
sim <- GenerateData2(max.age = max.age, nb.of.cohort = 30) # Generate catch using gear selectivity

# Estimate parameters, fixing selectivity for the last 2 age-groups to 1
result3 <- optim(par = c(0.2,1, rep(1e-12,max.age-2)), fn = llfunc5, catch = sim$catch, effort = sim$effort, catchability.scaling.factor = 1e-4, method = c("L-BFGS-B"),
       lower = c(5e-2,5e-2, rep(1e-12, max.age-2)), upper = c(10,0.5, rep(1, max.age-2)), hessian = TRUE, control = list(maxit = 1e3))
errors3 <- sqrt(abs(diag(solve(result3$hessian))))

print(cbind("Estimate" = result3$par, "Error" = errors3))




cleanEx()
nameEx("which.cohort")
### * which.cohort

flush(stderr()); flush(stdout())

### Name: which.cohort
### Title: A function that counts and numbers cohorts given a catch at age
###   matrix
### Aliases: which.cohort
### Keywords: misc

### ** Examples

mat <- matrix(NA, nrow = 5, ncol = 7)
which.cohort(mat)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
