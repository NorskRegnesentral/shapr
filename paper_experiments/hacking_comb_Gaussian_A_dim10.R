



rm(list=ls())

library(data.table)

folder = "paper_experiments/res/single_res/"


filelist <- list.files(folder)


Adim10_filelist <- filelist[grepl("current_results_experiment_A_dim_10_Linear",filelist)]

Adim10_filelist_RData <- Adim10_filelist[grepl(".RData",Adim10_filelist)]

Adim10_filelist_RData_seedlist <- list()
for(i in 1:10){
    seed = 1234 + i
    Adim10_filelist_RData_seedlist[[i]] <- Adim10_filelist_RData[grepl(paste0("this.seed_",seed),Adim10_filelist_RData)]
}

fileDT <- NULL
for (i in 1:10){
    for(j in 1:length(Adim10_filelist_RData_seedlist[[i]])){
        pathRData <- file.path(folder,Adim10_filelist_RData_seedlist[[i]][j])
        pathcsv <- paste0(substr(pathRData,start=1,stop=nchar(pathRData)-5),"csv")
#        load(pathRData)
        dat <- fread(pathcsv)

        fileDT <- rbind(fileDT,dat[1,.(i,j,rho,run_date_time,this.seed)])
    }

}

#setkey(fileDT,rho,run_date_time)
setorderv(x=fileDT,cols=c("rho","this.seed","run_date_time"),order=c(1,1,-1))

isdup <- duplicated(fileDT,by=c("this.seed","rho"))

fileDT[,isdup := isdup]

fileDT <- fileDT[isdup==FALSE,]
fileDT[,.N,by=.(rho)]

setkey(fileDT,i,j)

source.local <- T

for (k in 1:nrow(fileDT)){
    i <- fileDT[k,i]
    j <- fileDT[k,j]
    rho <- fileDT[k,rho]
    this.seed <- fileDT[k,this.seed]

    source("paper_experiments/source_getting_l_for_hacking__A_dim_10.R",local = source.local)

    pathRData <- file.path(folder,Adim10_filelist_RData_seedlist[[i]][j])
    pathcsv <- paste0(substr(pathRData,start=1,stop=nchar(pathRData)-5),"csv")
    load(pathRData)
    dat <- fread(pathcsv)


    ### Running equality test first
    DT <- Shapley.approx$comb_AIC_each_k$other_objects$DT

    Kshap <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
    for (i in l$Xtest[, .I]) {
        Kshap[i, ] = l$W %*% DT[id == i, k]
    }

    print(all.equal(Shapley.approx$comb_AIC_each_k$Kshap,Kshap)) # TRUE if OK

    ####

    Shapley.approx$comb_Gaussian_AIC_each_k <- Shapley.approx$comb_AIC_each_k
    Shapley.approx$comb_Gaussian_AIC_each_k$other_objects$ll <- NULL
    Shapley.approx$comb_Gaussian_AIC_each_k$other_objects$DT[wcomb %in% 177:1024] <- Shapley.approx$Gaussian$other_objects$DT[wcomb %in% 177:1024]

    DT <- Shapley.approx$comb_Gaussian_AIC_each_k$other_objects$DT

    Kshap <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
    for (i in l$Xtest[, .I]) {
        Kshap[i, ] = l$W %*% DT[id == i, k]
    }

    Shapley.approx$comb_Gaussian_AIC_each_k$Kshap <- Kshap

    source("paper_experiments/source_getting_compute_results_for_hacking_A_dim_10.R",local = source.local) # Creating the res.DT object

    newdat <- data.table(res,keep.rownames = T)

    newdat <- cbind(newdat,rbind(dat[,221:237],dat[4,221:237]))



    #### Write results to csv files ------------
    fwrite(x = newdat,file = paste0("paper_experiments/res/","all_results_experiment_A_dim_10_Linear_Linear_Gaussian_hacked.csv"),append = T)

    newpathcsv <- paste0(substring(pathcsv,1,nchar(pathcsv)-4),"_hacked.csv")
    newpathRData <- paste0(substring(pathRData,1,nchar(pathRData)-6),"_hacked.RData")

    fwrite(x = newdat,file = newpathcsv)
    save(Shapley.approx,Shapley.true,file=newpathRData)

    print(k)

}



