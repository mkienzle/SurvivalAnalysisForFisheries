# CREATED  13 November 2014
# MODIFIED 10 December 2014

# PURPOSE test the estimators by simulation

# Useful functions
source("SimulationFunctions.R")

# Keep all simulations
all.simulations <- list()
counter <- 1

# Combination of paramaters
#combination.of.par <- expand.grid(max.age = c(8,12,16), nb.of.cohort = seq(20,40,10), n.sample.per.year = rep(seq(1e3,1e4,1e3), each = 1e2))
combination.of.par <- expand.grid(max.age = c(8,12,16), nb.of.cohort = seq(25,45,10), n.sample.per.year = rep(125 * 2 ^ seq(0,4), each = 1e2))

# How many simulations ?
n.simulations <- nrow(combination.of.par)

# track simulation using
rand.seeds <- runif(n.simulations, min = 1, max = 1e6)

# Table to compare the results
compare.estimates <- data.frame(
    max.age = NA,
    nb.of.cohort = NA,
    n.sample.per.year = NA,
    random.seed = NA,
    sampling.type = NA,
    Optimization.status = NA,
    NegLL = NA,
    Nat.Mort.sim = NA,
    Nat.Mort.est = NA,
    Nat.Mort.error = NA,
    Catchability.sim = NA,
    Catchability.est = NA,
    Catchability.error =NA,
    Logistic.par.alpha.sim = NA,
    Logistic.par.alpha.est = NA,
    Logistic.par.alpha.error = NA,
    Logistic.par.beta.sim = NA,
    Logistic.par.beta.est = NA,
    Logistic.par.beta.error = NA,
    FourAndArchiNLL = NA)
    

for( i in 1:n.simulations){
#for( i in 1:1){ # For tests

print(paste("Simulation ", i,"of", dim(combination.of.par)[1]))

###########################################
# Strategy 1: irrealistic strategy used as a reference
###########################################

set.seed(rand.seeds[i])
tmp0 <- SamplingStrategy1(max.age = combination.of.par$max.age[i], nb.of.cohort = combination.of.par$nb.of.cohort[i], n.sample.per.year=combination.of.par$n.sample.per.year[i], verbose = FALSE)

all.simulations[[counter]] <- list(random.seed = rand.seeds[i], sampling.type = "Strategy 1", outcome = tmp0)
counter <- counter + 1

# calculate best case scenario Fournier & Archibald (1982) negative log-likelihood
tmp.catch <- tmp0$population$catch
prop.catch <- tmp.catch / outer(rowSums(tmp.catch), rep(1, ncol(tmp.catch)))
FourAndArchiNLL <- -sum(tmp0$nb.at.age.sample * log(prop.catch))
                                
compare.estimates <- rbind(compare.estimates, data.frame(
    max.age = combination.of.par$max.age[i],
    nb.of.cohort = combination.of.par$nb.of.cohort[i],
    n.sample.per.year = combination.of.par$n.sample.per.year[i],
    random.seed = rand.seeds[i],
    sampling.type = "Strategy 1",
    Optimization.status = tmp0$optimization.status,
    NegLL = tmp0$optimization$value,
    Nat.Mort.sim = tmp0$population$Nat.Mort,
    Nat.Mort.est = tmp0$optimization$par[2],
    Nat.Mort.error = tmp0$uncertainties[2],
    Catchability.sim = tmp0$population$catchability,
    Catchability.est = tmp0$optimization$par[1] * tmp0$optimization$catchability.scaling.factor,
    Catchability.error = tmp0$uncertainties[1] * tmp0$optimization$catchability.scaling.factor,
    Logistic.par.alpha.sim = tmp0$population$logistic.par[1],
    Logistic.par.alpha.est = tmp0$optimization$par[3],
    Logistic.par.alpha.error = tmp0$uncertainties[3],
    Logistic.par.beta.sim = tmp0$population$logistic.par[2],
    Logistic.par.beta.est = tmp0$optimization$par[4],
    Logistic.par.beta.error = tmp0$uncertainties[4],
    FourAndArchiNLL = FourAndArchiNLL))

print(" ################################################################### ")

###########################################
# Strategy 2
###########################################

set.seed(rand.seeds[i])
tmp1 <- SamplingStrategy2(max.age = combination.of.par$max.age[i], nb.of.cohort = combination.of.par$nb.of.cohort[i], n.sample.per.year=combination.of.par$n.sample.per.year[i], verbose = FALSE)

all.simulations[[counter]] <- list(random.seed = rand.seeds[i], sampling.type = "Strategy 2", outcome = tmp1)
counter <- counter + 1

# calculate best case scenario Fournier & Archibald (1982) negative log-likelihood
tmp1.catch <- tmp1$population$catch
prop1.catch <- tmp1.catch / outer(rowSums(tmp1.catch), rep(1, ncol(tmp1.catch)))
FourAndArchiNLL1 <- -sum(tmp1$nb.at.age.sample * log(prop1.catch))

compare.estimates <- rbind(compare.estimates, data.frame(
    max.age = combination.of.par$max.age[i],
    nb.of.cohort = combination.of.par$nb.of.cohort[i],
    n.sample.per.year = combination.of.par$n.sample.per.year[i],
    random.seed = rand.seeds[i],
    sampling.type = "Strategy 2",
    Optimization.status = tmp1$optimization.status,
    NegLL = tmp1$optimization$value,
    Nat.Mort.sim = tmp1$population$Nat.Mort,
    Nat.Mort.est = tmp1$optimization$par[2],
    Nat.Mort.error = tmp1$uncertainties[2],
    Catchability.sim = tmp1$population$catchability,
    Catchability.est = tmp1$optimization$par[1] * tmp1$optimization$catchability.scaling.factor,
    Catchability.error = tmp1$uncertainties[1] * tmp1$optimization$catchability.scaling.factor,
    Logistic.par.alpha.sim = tmp1$population$logistic.par[1],
    Logistic.par.alpha.est = tmp1$optimization$par[3],
    Logistic.par.alpha.error = tmp1$uncertainties[3],
    Logistic.par.beta.sim = tmp1$population$logistic.par[2],
    Logistic.par.beta.est = tmp1$optimization$par[4],
    Logistic.par.beta.error = tmp1$uncertainties[4],
    FourAndArchiNLL = FourAndArchiNLL1))

print(" ################################################################### ")
###########################################
# Strategy 2 with samples weighted by total catch
###########################################
set.seed(rand.seeds[i])
tmp2 <- SamplingStrategy2withCorrection(max.age = combination.of.par$max.age[i], nb.of.cohort = combination.of.par$nb.of.cohort[i], n.sample.per.year=combination.of.par$n.sample.per.year[i], verbose = FALSE)

all.simulations[[counter]] <- list(random.seed = rand.seeds[i], sampling.type = "Strategy 2 - weighted sample", outcome = tmp2)
counter <- counter + 1

# calculate best case scenario Fournier & Archibald (1982) negative log-likelihood
tmp2.catch <- tmp2$population$catch
prop2.catch <- tmp2.catch / outer(rowSums(tmp2.catch), rep(1, ncol(tmp2.catch)))

### Analysis with correction for catch
# proportion at age in sample
prop.at.age.sample <- tmp2$nb.at.age.sample / outer(rowSums(tmp2$nb.at.age.sample), rep(1,ncol(tmp2$nb.at.age.sample)))

# estimated total number in the population
raised.number <- outer(rowSums(tmp2$population$catch), rep(1, ncol(tmp2$nb.at.age.sample))) * prop.at.age.sample

# reduce back to sampling number
corrected.nb.at.age.sample <- raised.number * sum(tmp2$nb.at.age.sample) / sum(raised.number)

FourAndArchiNLL2 <- -sum(corrected.nb.at.age.sample * log(prop2.catch))

compare.estimates <- rbind(compare.estimates, data.frame(
    max.age = combination.of.par$max.age[i],
    nb.of.cohort = combination.of.par$nb.of.cohort[i],
    n.sample.per.year = combination.of.par$n.sample.per.year[i],
    random.seed = rand.seeds[i],
    sampling.type = "Strategy 2 - weighted sample",
    Optimization.status = tmp2$optimization.status,
    NegLL = tmp2$optimization$value,
    Nat.Mort.sim = tmp2$population$Nat.Mort,
    Nat.Mort.est = tmp2$optimization$par[2],
    Nat.Mort.error = tmp2$uncertainties[2],
    Catchability.sim = tmp2$population$catchability,
    Catchability.est = tmp2$optimization$par[1] * tmp2$optimization$catchability.scaling.factor,
    Catchability.error = tmp2$uncertainties[1] * tmp2$optimization$catchability.scaling.factor,
    Logistic.par.alpha.sim = tmp2$population$logistic.par[1],
    Logistic.par.alpha.est = tmp2$optimization$par[3],
    Logistic.par.alpha.error = tmp2$uncertainties[3],
    Logistic.par.beta.sim = tmp2$population$logistic.par[2],
    Logistic.par.beta.est = tmp2$optimization$par[4],
    Logistic.par.beta.error = tmp2$uncertainties[4],
    FourAndArchiNLL = FourAndArchiNLL2))

}

# Keep results
write.csv(file = "../Results/Simulations/CompareEstimates.csv", compare.estimates[-1,])
write.csv(file = paste("../Results/Simulations/Archive/CompareEstimates-", format(Sys.time(), "%a-%b-%d-%H:%M:%S-%Y"), ".csv", sep=""), compare.estimates[-1,])
save(all.simulations, file = "../Results/Simulations/all.simulations.R")
save(all.simulations, file = paste("../Results/Simulations/Archive/all.simulations-", format(Sys.time(), "%a-%b-%d-%H:%M:%S-%Y"),".R", sep=""))
