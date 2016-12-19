# CREATED  2 Sept 2015
# MODIFIED 2 Sept 2015

# Using the results of simulations
load(file = "../Results/Simulations/Archive/all.simulations-Tue-Nov-25-10:28:46-2014.R")

# And a case of strategy 2 with weights, the hessian is

hess <- solve(all.simulations[[26997]]$outcome$optimization$hessian)
my.sd <- sqrt(diag(hess))
var.cov <- hess / (my.sd %*% t(my.sd))
