EstimateMandQ <- function(catch, effort, catchability.scaling.factor){

result <- optim(par = c(10,1), fn = llfunc2, catch = catch, effort = effort, catchability.scaling.factor = catchability.scaling.factor, method = c("L-BFGS-B"),
      lower = c(1e-2,1e-2), upper = c(1e2,1e2), hessian = TRUE)

return(result)
}
