# based on the idea that each catch figure provides an estimate of recruitment
EstimateRecruitment <- function(Z){

    
    estimates <- get("catch", envir=.GlobalEnv) / ((Z-get("M", envir=.GlobalEnv)) * ((exp(-Z*get("age", envir=.GlobalEnv)) - exp(-Z*(get("age", envir=.GlobalEnv)+1)))/Z) )

print(paste("Estimated recruitment:", paste(c(round(mean(estimates),2), round(sd(estimates),2)), collapse = " +- ")))

# Use mean and sd of estimates to provide the best estimate
    return(c(mean(estimates), sd(estimates)))
}

