EstimateMandQ <- function(catch, effort, catchability.scaling.factor){

result <- optim(par = c(10,1), fn = llfunc2, catch = catch, effort = effort, catchability.scaling.factor = catchability.scaling.factor
      , hessian = TRUE)

ifelse(result$convergence == 0, return(result), {print("Convergence failed"); return(1)})
}
