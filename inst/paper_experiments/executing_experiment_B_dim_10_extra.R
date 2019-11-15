
 rm(list=ls())
 library(parallel)
 library(foreach)
 library(doParallel)
 cl <- parallel::makeCluster(5,outfile="")
 registerDoParallel(cl)
 seed.vec <- 1:10 + 1234 # We fix this seed
 source.local <- TRUE

 rho.vec <- c(0,0.1,0.2,0.3,0.4)#c(0.5,0.6,0.7,0.8,0.9)#seq(0,0.95,length.out=20)[c(seq(1,20,by=2),seq(2,20,by=2))]

 bb = foreach(rho = rho.vec, .errorhandling = 'pass') %dopar% {
     for (this.seed in seed.vec){
         source("paper_experiments/experiment_B_dim_10_PiecewiseConstant_XGBoost_Gaussian.R",local = source.local)
     }
     print(paste0("Just finished computation for rho = ",rho," with seed ",this.seed))
 }


