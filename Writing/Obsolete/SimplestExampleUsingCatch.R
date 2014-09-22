# Simple illustration of using survival analysis concepts
# to estimate mortality rates

N0 <- 1e3
F <- runif(1, min = 0, max = 2) # per year
M <- runif(1, min = 0.1, max = 0.5) # per year
time <- seq(0,20)
nb.alive.at.time <- N0 * exp(- (M+F) * time)
nb.dying.in.interval <- nb.alive.at.time[-length(time)] - nb.alive.at.time[-1]
catch <- F/(F+M) * nb.dying.in.interval

llfunc <- function(f){
    # Here you assume that you know M or you could just estimate total mortality
    Z <- f+M
    P <- f/(Z) * (exp(-Z * time[-length(time)]) - exp(-Z*time[-1]))
    -sum( catch * log(P/sum(P)))
}

result <- optim(par = 0.5, llfunc, method = "L-BFGS-B")
print(paste("Simulated F is", round(F,2)))
print(paste("Estimated F is", round(result$par,2)))
