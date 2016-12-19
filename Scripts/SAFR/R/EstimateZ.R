# Estimate total mortality (Z) using catch at age data from a single cohort
EstimateZ <- function(catch){

    # Estimate cohort-specific mortality rates

result <- optim(par = c(0.1), fn = llfunc1, catch = catch, method = c("L-BFGS-B"),
      lower = c(1e-2), upper = c(3), hessian = TRUE)
print(paste("Estimated Z is", round(result$par,3), "+-", round(sqrt(diag(solve(result$hessian))),3)))

return(result)
}
