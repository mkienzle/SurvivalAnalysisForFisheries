# CREATED   27 October 2014
# MODIFIED  27 October 2014

library(SAFR)
example(EstimateZ)

# Plot the data
par(mfrow=c(2,2))
plot(age, nb.at.age[,2], main = "Surviving both nature and fishing", las = 1)
plot(age, catch, main = "Catch", type = "h", las=1)

