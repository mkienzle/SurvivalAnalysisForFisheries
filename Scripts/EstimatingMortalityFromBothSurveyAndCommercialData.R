# CREATION DATE     11 Nov. 2008
# MODIFICATION DATA 12 Nov. 2008

# AUTHOR marco.kienzle@gmail.com
# PURPOSE express a likelihood function to estimate cohort-specific mortality
#         and age-specific selectivity in commercial catch
#         using catch formatted as the typical catch at age matrix (year x age)
#         from Torres Strait Survey data

# METHOD information is limited to 2 age group

# DATA
# In survey data
#        age-group 1 individuals are thought to be between 1 and 1.5 years old
#        age-group 2 individuals are thougth to be between 2 and 2.5 years old

# In commercial catch data
#        age-group 1 individuals are thought to be between 1.75 and 2 years old
#        age-group 2 individuals are thougth to be between 2 and 2.75 years old

# ASSUMPTION constant natural mortality, cohort-specific fishing mortality
#            selectivity for commercial age-group 1 between [0;1] and fixed to 1 for age-group 2 assumed


# Load lobster commercial data
  l.com.catch = read.table("/home/kie029/Fishery/Torres_Straight_Lobster/Data/CommercialCatch.txt")

### Load lobster survey data

# Note that this raising factor influence the results
l.surv.catch = 210 * read.table("/home/kie029/Fishery/Torres_Straight_Lobster/Data/SurveyIndex.txt")

#### Define the log likelihood function to estimate mortality


llfunc.both.dataset <- function(x) { # express as a function 

# NOTE the vector of parameter x contains M in the first position
#      and dim(l.surv.catch)[1] + 1 cohort-specific fishing mortality
#      and 2 selectivity parameter for age group 1: 1st from 1989 to 2002, 2nd since 2003  
# Specify the natural mortality
  M = x[1]

# Specify matrix where fishing mortality parameter are
# associated with each cohort
  F = matrix(NA, ncol = dim(l.surv.catch)[2], nrow = dim(l.surv.catch)[1])
  n = dim(F)[1]; p = dim(F)[2]
  F[1, p] = x[2]

  # Fill the upper right corner
  for(i in seq(1, p-1)){
    diag(F[seq(1,i+1), seq(p-i,p) ]) = x[i+1 + 1]
  }
  # Fill the lower left corner (note the overlap)
  for(i in seq(1, n - 1)){
diag(F[seq(i, min(i + p - 1, n)), seq(1, length(seq(i, min(i + p - 1, n))))]) = x[p + i - 1 + 1]
}
# And the last cohort
F[n,1] = x[length(x) - 2]

### And selectivity for commercial catch of age-group 1
  q1 = x[length(x) - 1]
  q2 = x[length(x)]
  # Specify the selectivity
sel.mat = matrix(c(q1,1), nrow = 14, ncol = dim(l.com.catch)[2], byrow=T)
sel.mat = rbind(sel.mat, matrix(c(q2,1), nrow = dim(l.com.catch)[1] - 14, ncol = dim(l.com.catch)[2], byrow=T))

# Specify the age matrix
surv.age.mat = matrix(c(1,2), nrow = dim(l.surv.catch)[1], ncol = dim(l.surv.catch)[2] , byrow=T)
com.age.mat1 = matrix(c(1.75,2), nrow = dim(l.surv.catch)[1], ncol = dim(l.surv.catch)[2] , byrow=T)
com.age.mat2 = com.age.mat1 + matrix(c(0.25,.75), nrow = dim(l.surv.catch)[1], ncol = dim(l.surv.catch)[2] , byrow=T)
  
# Calculate the probability associated with each observation
       surv.prob1 = exp(- (M) * surv.age.mat)
       surv.prob2 = exp(- (M+F) * (surv.age.mat + 0.5))

       com.prob1 = exp(- (M + 0.25 * F) * com.age.mat1)
       com.prob2 = exp(- (M+F) * com.age.mat2)

#TEST print((prob1 - prob2))
#TEST print(normalizing.matrix(prob1-prob2))
  
# Express the likelihood
       - sum(l.surv.catch * log( (surv.prob1 - surv.prob2) / normalizing.matrix(surv.prob1 - surv.prob2) )) -
         sum(l.com.catch * log( sel.mat * (com.prob1 - com.prob2) / normalizing.matrix(sel.mat * (com.prob1 - com.prob2)) ))
     }

# Determine the number of cohorts in the catch at age matrix
# which will give you the number of parameter to estimate
co.nb = sum(dim(l.surv.catch)) - 1

# Estimate cohort-specific mortality rates
estimated = optim(par = c(0.1,rep(0.5, co.nb), 0.1, 0.1), fn = llfunc.both.dataset, method = c("L-BFGS-B"),
      lower = c(1e-5, rep(1e-5, co.nb), 1e-5, 1e-5), upper = c(5, rep(10, co.nb),1,1), hessian = FALSE)

# Plot the estimated total mortality
plot(1988:2006, estimated$par[1] + estimated$par[2:20], xlab = "Cohort birth year", ylab = "Total mortality", type = "l", las=1)
