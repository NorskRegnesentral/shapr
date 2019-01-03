
##############
library(doSNOW)
library(foreach)
cl <- makeCluster(5,outfile="")
registerDoSNOW(cl)
seed.vec <- 1:5 + 1234 # We fix this seed
local <- TRUE


rho.vec <- seq(0,0.95,length.out=20)[c(seq(1,20,by=2),seq(2,20,by=2))]
 progress <- function(n) cat(sprintf("task %d is complete\n", n))
 opts <- list(progress=progress)


bb = foreach(rho = rho.vec, .options.snow = opts, .errorhandling = 'pass') %dopar% {
    for (this.seed in seed.vec){
        source("paper_experiments/experiment_A_dim_10_Linear_Linear_Gaussian.R",local = local)
        }
    paste0("Just finished computation for rho = ",rho," with seed ",this.seed)
}


    #





#
#
# #################
#
# library(doParallel)
# library(foreach)
# cl <- parallel::makeCluster(5,type="FORK")
# registerDoParallel(cl)
# seed.vec <- 1:5 + 1234 # We fix this seed
#
# rho.vec <- seq(0,0.95,length.out=20)[c(seq(1,20,by=2),seq(2,20,by=2))]
#
# log.socket <- make.socket(port=4000)
#
# Log <- function(text, ...) {
#     msg <- sprintf(paste0(as.character(Sys.time()), ": ", text, "\n"), ...)
#     cat(msg)
#     write.socket(log.socket, msg)
# }
#
#
# bb=foreach(rho=rho.vec) %dopar%{
#     this = rho
#     for (this.seed in seed.vec){
#         Log("Just STARTED computation for rho = %f with seed %f", this, 1)
#     }
#     paste0("Just finished computation for rho = ",rho," with seed ",1)
#
# }
#
#
#
# progress <- function(n) cat(sprintf("task %d is complete\n", n))
# opts <- list(progress=progress)
#
# output_par =
#     foreach(i = 1:5, .options.snow = opts, .errorhandling = 'pass') %dopar%
#     # the default .combine = list
#     {
#         this = rho
#         for (this.seed in seed.vec){
#         }
#         paste0("Just finished computation for rho = ",rho," with seed ",1)
#     }
#
#
#
#     for (this.seed in seed.vec){
#
#
# #        source("paper_experiments/experiment_A_dim_10_Linear_Linear_Gaussian.R")
#
#         Log("Just FINISHED computation for rho = %d with seed %d", this, 1)
#
# #        print(paste0("Just finished computation for rho = ",rho," with seed ",this.seed))
#     }
# }
#
# stopCluster(cl)
#
#
#
#
