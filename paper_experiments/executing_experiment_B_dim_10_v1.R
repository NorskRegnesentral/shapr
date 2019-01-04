
##############
library(doSNOW)
library(foreach)
cl <- makeCluster(5,outfile="")
registerDoSNOW(cl)
seed.vec <- 1:5 + 1234 # We fix this seed
source.local <- TRUE


rho.vec <- c(0,0.1)#0.2,0.4,0.6,0.8)

#seq(0,0.95,length.out=20)[c(seq(1,20,by=2),seq(2,20,by=2))]
 progress <- function(n) cat(sprintf("task %d is complete\n", n))
 opts <- list(progress=progress)

 bb = foreach(this.seed = seed.vec,.options.snow = opts, .errorhandling = 'pass') %dopar% {
     for (rho in rho.vec){
         source("paper_experiments/experiment_B_dim_10_PiecewiseConstant_XGBoost_Gaussian.R",local = source.local)
     }
    paste0("Just finished computation for rho = ",rho," with seed ",this.seed)
}




