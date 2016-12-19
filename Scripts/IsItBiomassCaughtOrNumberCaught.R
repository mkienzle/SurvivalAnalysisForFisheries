# 10 March 2016


source("SimulationFunctions.R")

combination.of.par <- expand.grid(max.age = c(8,12,16), nb.of.cohort = seq(25,45,10), n.sample.per.year = rep(125 * 2 ^ seq(0,4), each = 1e2))


i <- 4485
set.seed(1)
tmp2 <- SamplingStrategy2withCorrection(max.age = combination.of.par$max.age[i], nb.of.cohort = combination.of.par$nb.of.cohort[i], n.sample.per.year=combination.of.par$n.sample.per.year[i], verbose = FALSE)

