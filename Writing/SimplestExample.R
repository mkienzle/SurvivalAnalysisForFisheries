# Simple illustration of using survival analysis concepts
# to estimate mortality rates

N0 <- 1e3
F <- 0.4 # per year
M <- 0.2 # per year
time <- seq(0,20)
nb.alive.at.time <- N0 * exp(- (M+F) * time)
nb.dying.in.interval <- nb.alive.at.time[-length(time)] - nb.alive.at.time[-1]

llfunc <- function(Z){
    -sum( nb.dying.in.interval * log(exp(-Z * time[-length(time)]) - exp(-Z*time[-1])))
}

result <- optim(par = 0.5, llfunc, method = "L-BFGS-B")
