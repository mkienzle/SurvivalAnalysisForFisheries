

# Prepare number at age table
tmp.nb.at.age <- read.csv("/home/mkienzle/mystuff/Work/DEEDI/Biometry services/Fisheries/Mullet/Data/Mullet-NumberAged.csv")
nb.at.age <- with(tmp.nb.at.age, tapply(Numbers.Aged, list(Year, AgeGroup), I))

as.numeric(dimnames(nb.at.age)[[2]]) -> x
dimnames(nb.at.age)[[2]] <- paste(x-1,x,sep="--")

tmp.effort <- read.csv("/home/mkienzle/mystuff/Work/DEEDI/Biometry services/Fisheries/Mullet/Data/Mullet-Effort2.csv")
effort <- with(tmp.effort, outer(Days.fished, rep(1, ncol(nb.at.age))))


library(xtable)
my.xtable <- xtable(cbind(nb.at.age, Catch = tmp.effort$Catch..t., Effort = tmp.effort$Days.fished), caption = "Distribution of yearly samples (in rows) of sea mullet into age-groups of width 1 year (in columns); catch in tonnes and effort in boat-days.", label = "tab:Mullet-NbAtAge", align = paste(paste("|l",paste(rep("|c", ncol(nb.at.age)),sep = "", collapse=""), sep = ""), "||c||c|", sep = "", collapse=""))
print(my.xtable, file = "../Tables/Mullet-NbAtAge.tex", floating.environment = 'sidewaystable')
