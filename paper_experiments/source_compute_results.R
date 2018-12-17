colSds <- function(x){
    apply(X=x,MARGIN = 2,FUN=sd)
}


absmeans <- matrix(NA,nrow=length(Shapley.approx), ncol=ncol(Xtrain))
rownames(absmeans) <- names(Shapley.approx)
colnam <- paste0("absmean_X",1:ncol(Xtrain))
colnames(absmeans) <- colnam

for (i in 1:length(Shapley.approx)){
    absmeans[i,] <- colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx[[i]]$Kshap[,-1]))
}
absmeans <- cbind(absmeans,absmean_total=rowMeans(absmeans))

abssds <- matrix(NA,nrow=length(Shapley.approx), ncol=ncol(Xtrain))
rownames(abssds) <- names(Shapley.approx)
colnam <- paste0("abssd_X",1:ncol(Xtrain))
colnames(abssds) <- colnam

abssd_total <- numeric()
for (i in 1:length(Shapley.approx)){
    abssds[i,] <- colSds(abs(Shapley.true$exactShap[,-1]-Shapley.approx[[i]]$Kshap[,-1]))
    abssd_total[i] <- sd(abs(Shapley.true$exactShap[,-1]-Shapley.approx[[i]]$Kshap[,-1]))
}
abssds <- cbind(abssds,abssd_total=abssd_total)


### Storing the optimal bandwidths (h)
h_optims <- matrix(NA,nrow=length(Shapley.approx), ncol=2^ncol(Xtrain))
for (i in 1:length(Shapley.approx)){
    for (j in 1:(2^ncol(Xtrain))){
        h_optims[i,j] <- mean(Shapley.approx[[i]]$other_objects$h_optim_mat[j,])
        #h_optims[i,j] <-Shapley.approx[[i]]$other_objects$h_optim_mat[j,1]
    }
}

colnam_h <- character()
for (j in 1:(2^ncol(Xtrain))){
    colnam_h[j] <- paste0("h_X_",paste0(l$X$features[[j]],collapse = "_"))
}

rownames(h_optims) <- names(Shapley.approx)
colnames(h_optims) <- colnam_h
h_optims[which(rownames(h_optims)=="empirical_independence"),] <- Inf

comp_time <- rep(NA,length(Shapley.approx))
for (i in 1:length(Shapley.approx)){
    comp_time[i] <- Shapley.approx[[i]]$other_objects$comp_time[3]
}

res <- cbind(absmeans,abssds,h_optims[,-c(1,ncol(h_optims))])
res.DT <- data.table(res,keep.rownames = T)

res.DT[,comp_time:=comp_time]
res.DT[,true_model:=true_model]
res.DT[,fitted_model:=fitted_model]
res.DT[,variables:=variables]
res.DT[,notes:=notes]
res.DT[,rho:=rho]
res.DT[,pi.G:=pi.G]
res.DT[,sd_noise:=sd_noise]
res.DT[,nTrain:=nTrain]
res.DT[,nTest:=nTest]
res.DT[,w_threshold:=w_threshold]
res.DT[,n_threshold:=n_threshold]
res.DT[,this.seed:=this.seed]
