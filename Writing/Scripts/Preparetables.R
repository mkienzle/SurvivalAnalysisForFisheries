
# Load catch
tmp.catch <- read.csv("/home/mkienzle/mystuff/Work/DEEDI/Biometry services/Fisheries/Mullet/Data/Mullet-CatchBySector.csv")
catch.est <- tmp.catch$Estuarine; names(catch.est) <- tmp.catch$Year
catch.ocean <- tmp.catch$Ocean.Beach; names(catch.ocean) <- tmp.catch$Year

# Load the otolith measurements
tmp.nb.at.age.ocean <- read.csv("/home/mkienzle/mystuff/Work/DEEDI/Biometry services/Fisheries/Mullet/Data/Mullet-NumberAged-Ocean.csv")
nb.at.age.ocean <- with(tmp.nb.at.age.ocean, tapply(Numbers.of.fish, list(Year, Age.Group), I))
#nb.at.age.ocean <- replace(nb.at.age.ocean, is.na(nb.at.age.ocean), 0)

tmp.nb.at.age.estuarine <- read.csv("/home/mkienzle/mystuff/Work/DEEDI/Biometry services/Fisheries/Mullet/Data/Mullet-NumberAged-Estuarine.csv")
nb.at.age.est <- with(tmp.nb.at.age.estuarine, tapply(Numbers.of.fish, list(Year, Age.Group), I))
#nb.at.age.est <- replace(nb.at.age.est, is.na(nb.at.age.est), 0)

# data contain same age-group and year ?
all.col <- sort(as.numeric(union(dimnames(nb.at.age.est)[[2]], dimnames(nb.at.age.ocean)[[2]])))
all.row <- sort(as.numeric(union(dimnames(nb.at.age.est)[[1]], dimnames(nb.at.age.ocean)[[1]])))

new.nb.at.age.est <- matrix(NA, nrow = length(all.row), ncol = length(all.col))
dimnames(new.nb.at.age.est) <- list(all.row, all.col)

new.nb.at.age.ocean <- matrix(NA, nrow = length(all.row), ncol = length(all.col))
dimnames(new.nb.at.age.ocean) <- list(paste(all.row, ".ocean", sep=""), all.col)

for(j in dimnames(nb.at.age.est)[[2]]) new.nb.at.age.est[,j] <- nb.at.age.est[,j]
for(j in dimnames(nb.at.age.ocean)[[2]]) new.nb.at.age.ocean[,j] <- nb.at.age.ocean[,j]

tmp.effort.est <- read.csv("/home/mkienzle/mystuff/Work/DEEDI/Biometry services/Fisheries/Mullet/Data/Mullet-Effort-Estuarine.csv")
effort.est <- with(tmp.effort.est, outer(Number.of.Days, rep(1, dim(nb.at.age)[2])))
dimnames(effort.est)[[1]] <- tmp.effort.est$Year
dimnames(effort.est)[[2]] <- dimnames(nb.at.age)[[2]]

tmp.effort.ocean <- read.csv("/home/mkienzle/mystuff/Work/DEEDI/Biometry services/Fisheries/Mullet/Data/Mullet-Effort-Ocean.csv")
effort.ocean <- with(tmp.effort.ocean, outer(Number.of.Days, rep(1, dim(nb.at.age)[2])))
dimnames(effort.ocean)[[1]] <- tmp.effort.ocean$Year
dimnames(effort.ocean)[[2]] <- dimnames(nb.at.age)[[2]]

# # Prepare number at age table
# tmp.nb.at.age <- read.csv("/home/mkienzle/mystuff/Work/DEEDI/Biometry services/Fisheries/Mullet/Data/Mullet-NumberAged.csv")
# nb.at.age <- with(tmp.nb.at.age, tapply(Numbers.Aged, list(Year, AgeGroup), I))

as.numeric(dimnames(new.nb.at.age.est)[[2]]) -> x
dimnames(new.nb.at.age.est)[[2]] <- paste(x-1,x,sep="--")

as.numeric(dimnames(new.nb.at.age.ocean)[[2]]) -> x
dimnames(new.nb.at.age.ocean)[[2]] <- paste(x-1,x,sep="--")

# tmp.effort <- read.csv("/home/mkienzle/mystuff/Work/DEEDI/Biometry services/Fisheries/Mullet/Data/Mullet-Effort2.csv")
# effort <- with(tmp.effort, outer(Days.fished, rep(1, ncol(nb.at.age))))


library(xtable)
my.xtable <- xtable(cbind(rbind(new.nb.at.age.est, new.nb.at.age.ocean), Catch = c(catch.est, catch.ocean), Effort = c(tmp.effort.est$Number.of.Days, tmp.effort.ocean$Number.of.Days)), digits = 0, caption = "Distribution of yearly samples (in rows) of sea mullet into age-groups of width 1 year (in columns); catch in tonnes and effort in number of days.", label = "tab:Mullet-NbAtAge", align = paste(paste("|l",paste(rep("|c", ncol(new.nb.at.age.est)),sep = "", collapse=""), sep = ""), "||c||c|", sep = "", collapse=""))
print(my.xtable, file = "../Tables/Mullet-NbAtAge.tex", floating.environment = 'sidewaystable')
