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

### Estimate of Z using a plus group
ap <- ifelse( (0.0001 * sum(catch)) < catch[11], 11, min(which(catch < (0.0001 * sum(catch)) )))
catch1 <- catch[1:ap]; catch1[ap] <- sum(catch[ap:11]) # create the +group

# if the number of observation is large enough so that there is no need to create a +group
# then do not use the +group option
ifelse( length(catch) == length(catch1), print("+group option not used"),
{ 
result1 <- optim(par = c(0.1), fn = llfunc1, catch = catch1, plus.group = TRUE, method = c("L-BFGS-B"),
      lower = c(1e-2), upper = c(3), hessian = TRUE)
print(paste("Estimated Z is", round(result1$par,3), "+-", round(sqrt(diag(solve(result1$hessian))),3)))
})

print("# An estimate of recruitment")
# According to Dupont (1983) and Chiang (1968) cohort abundance at t=0 can be estimated by the ratio of total number of individual dying to total probability of dying

#rec.est <- catch / ((result$par[1] - M)/result$par[1]) / (exp(-result$par[1] * #age) - exp(-result$par[1] * (age + 1)) )
#print(mean(rec.est))
rec.est <- sum(catch) / ((result$par[1] - M)/result$par[1]) / sum( (exp(-result$par[1] * age) - exp(-result$par[1] * (age + 1)) ))
print(rec.est)
print(paste("Compared to N0=", round(N0,3)))



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

### Estimate recruitment
# According to Dupont (1983) Biometrics vol. 39 No 4 pp. 1021-1033

est.rec <- sum(catch)/sum(prob.for.llfunc2(best.qM.est$par, effort, csf))

## And not finding a better way to calculate the uncertainty
ind.rec <- catch / prob.for.llfunc2(best.qM.est$par, effort, csf)
#est.rec.limits <- c("Lower" = sum(catch)/sum(prob.for.llfunc2(best.qM.est$par - errors, effort, csf)), "Upper" = sum(catch)/sum(prob.for.llf#unc2(best.qM.est$par + errors, effort, csf)))

print(" ##### ")
print(paste("Estimated catchability is", round(best.qM.est$par[1],3), "+-", round(errors[1],3), as.character(csf)))
print(paste("Estimated M is", round(best.qM.est$par[2],3), "+-", round(errors[2],3)))
print(paste("Estimated recruitment is", round(est.rec,0), " ranging from ", round(min(ind.rec),0), " to ", round(max(ind.rec),0)))




cleanEx()
nameEx("llfunc3")
### * llfunc3

flush(stderr()); flush(stdout())

### Name: llfunc3
### Title: log-likelihood function of catch at age matrix to estimate
###   catchability, selectivity and natural mortality from a matrix of
###   catch at age
### Aliases: llfunc3
### Keywords: misc

### ** Examples


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




cleanEx()
nameEx("llfunc7")
### * llfunc7

flush(stderr()); flush(stdout())

### Name: llfunc7
### Title: log-likelihood function of catch at age matrix to estimate
###   catchability, selectivity [assumed logistic] and natural mortality
### Aliases: llfunc7
### Keywords: misc

### ** Examples

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




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
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
