# survival analysis to estimate mortality rates
# using truncated distributions

N0 <- 1e3
F <- 0.4 # per year
M <- 0.2 # per year
time <- seq(0,20)
nb.alive.at.time <- N0 * exp(- (M+F) * time)
nb.dying.in.interval <- nb.alive.at.time[-length(time)] - nb.alive.at.time[-1]

# Suppose you have only the first 3 age-groups
x <- 1:3

llfunc <- function(Z){
    
    P <- (exp(-Z * time[-length(time)]) - exp(-Z*time[-1]))[x]
    -sum( nb.dying.in.interval[x] * log(P/sum(P)))
}

result <- optim(par = 0.5, llfunc, method = "L-BFGS-B")

# or only 3 age-groups in the middle of the distribution
x <- 4:7
result2 <- optim(par = 0.5, llfunc, method = "L-BFGS-B")
