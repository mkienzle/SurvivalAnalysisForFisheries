# Generate data
GenerateData2 <- function(max.age = 10, nb.of.cohort = 20, ...){

# Fishing mortality as the product of catchability and yearly effort
catchability <- runif(1, min = 1, max = 10)
catchability.mf <- 1e-4 # multiplying factor

print(paste("Simulated catchability is ", round(catchability,2), "x 10^-4"))

yearly.effort <- runif(nb.of.cohort, min = 1e4, max = 1.75e4)

effort.faced.by.each.cohort <- matrix(nrow = nb.of.cohort, ncol = max.age)
for(i in 1:nb.of.cohort) effort.faced.by.each.cohort[i,] <- yearly.effort[seq(i, i + max.age - 1)]

n <- max.age-1
selectivity.at.age <- c(0,0,seq(1/(max.age - 3), (max.age - 4)/(max.age - 3), length = max.age - 4), 1,1)
F <- catchability * catchability.mf * effort.faced.by.each.cohort * outer(rep(1, nb.of.cohort), selectivity.at.age)

M <- runif(1, min = 0.05, max = 0.5)
print(paste("Simulated natural mortality is", round(M,3)))

# Recruitment: N(0)
Recruitment <- runif(nb.of.cohort, min = 1e6, max = 5e6)

# Nb at age in cohorts
cohort <- matrix(nrow = nb.of.cohort, ncol = max.age + 1)
cohort[,1] <- Recruitment
for(i in 1:nrow(cohort)) cohort[i,] <- cohort[i,1] * exp(-c(0, cumsum(M + F[i,])))

# Number that died
cohort.nb.dead <- cohort[,1:max.age] - cohort[,seq(2, max.age+1)]

# Catch
cohort.nb.caught <- F / (M+F) * cohort.nb.dead

### Calculate the number alive at age

# Name your dataframe with arbitrary cohort birth-date
dimnames(cohort)[[1]] = seq( from  = 1988, by = 1, length = nb.of.cohort)
dimnames(cohort)[[2]] = paste("age-group", seq(0,max.age), sep="")  # give age names

# Re-format data into catch at age matrix
catch.at.age <- matrix(ncol = max.age, nrow = nb.of.cohort)
effort.at.age <- matrix(ncol = max.age, nrow = nb.of.cohort)

for(i in seq(1,nb.of.cohort - 1)) {

  diag(catch.at.age[seq(i, nb.of.cohort),seq(1, min(nb.of.cohort - i +1, max.age))]) <-
    as.numeric(cohort.nb.caught[i,seq(1, min(nb.of.cohort - i +1, max.age))])

  diag(effort.at.age[seq(i, nb.of.cohort),seq(1, min(nb.of.cohort - i +1, max.age))]) <-
    as.numeric(effort.faced.by.each.cohort[i,seq(1, min(nb.of.cohort - i +1, max.age))])
  
}
catch.at.age[nb.of.cohort,1] = cohort.nb.caught[nb.of.cohort,1] # diag deals on with matrix
effort.at.age[nb.of.cohort,1] = effort.faced.by.each.cohort[nb.of.cohort,1] # diag deals on with matrix

# Get a complete dataset
c.catch.at.age = catch.at.age[complete.cases(catch.at.age),]
effort.at.age = effort.at.age[complete.cases(effort.at.age),]

return(list(catch = c.catch.at.age, effort = effort.at.age))
}

