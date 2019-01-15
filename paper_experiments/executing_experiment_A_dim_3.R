
library(parallel)
library(foreach)
cl <- parallel::makeCluster(5,outfile="")
seed.vec <- 1:10 + 1234 # We fix this seed
source.local <- TRUE

rho.vec <- seq(0,0.98,length.out=50)[c(seq(1,50,by=2),seq(2,50,by=2))]

bb = foreach(rho = 1:10, .errorhandling = 'pass') %dopar% {
    rho = rho^2
    source("paper_experiments/test2.R")
    this
}

primary = "hpc04"





f <- function(rho){
    print(rho)


    rho
}



aa=mclapply(1:5,f)




stopCluster(cl)
