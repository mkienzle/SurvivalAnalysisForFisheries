my.log.lik <- function(n){

-(sum(log(seq(1,n))) - sum(log(seq(1, n -x))) - sum(log(seq(1,x))) + x * log(p) + (n-x) * log(1-p))
}

x <- sum(catch)
p <- sum(prob.for.llfunc2(best.qM.est$par, effort, csf))
prob <- prob.for.llfunc2(best.qM.est$par, effort, csf)
n <- 1e4


multinom.lik <- function(n){
tmp <- 0

tmp <- tmp + sum(log(seq(1,n)))

tmp <- tmp - sum(log(seq(1, n - sum(catch))))

for(i in 1:length(catch))
      tmp <- tmp - sum(log(seq(1,catch[i])))

for(i in 1:length(catch))
      tmp <- tmp + catch[i] * log(prob[i])

tmp <- tmp + (n- sum(catch)) * log(1-sum(prob))

return(-tmp)
}