colSds <- function(x){
    apply(X=x,MARGIN = 2,FUN=sd)
}


absmeans <- matrix(NA,nrow=length(Shapley.approx), ncol=ncol(Shapley.approx[[1]][[1]])-1)
rownames(absmeans) <- names(Shapley.approx)
colnam <- paste0("absmean_X",1:(ncol(Shapley.approx[[1]][[1]])-1))
colnames(absmeans) <- colnam

for (i in 1:length(Shapley.approx)){
    absmeans[i,] <- colMeans(abs(Shapley.true$exactShap[,-1]-Shapley.approx[[i]]$Kshap[,-1]))
}
absmeans <- cbind(absmeans,absmean_total=rowMeans(absmeans))

absrelmeans <- matrix(NA,nrow=length(Shapley.approx), ncol=ncol(Shapley.approx[[1]][[1]])-1)
rownames(absrelmeans) <- names(Shapley.approx)
colnam <- paste0("absrelmean_X",1:(ncol(Shapley.approx[[1]][[1]])-1))
colnames(absrelmeans) <- colnam

for (i in 1:length(Shapley.approx)){
    absrelmeans[i,] <- colMeans(abs((Shapley.true$exactShap[,-1]-Shapley.approx[[i]]$Kshap[,-1])/Shapley.true$exactShap[,-1]))

}
absrelmeans <- cbind(absrelmeans,absrelmean_total=rowMeans(absrelmeans))

skillscoremeans <- matrix(NA,nrow=length(Shapley.approx), ncol=ncol(Shapley.approx[[1]][[1]])-1)
rownames(skillscoremeans) <- names(Shapley.approx)
colnam <- paste0("skillscoremean_X",1:(ncol(Shapley.approx[[1]][[1]])-1))
colnames(skillscoremeans) <- colnam

absmeans_ref <- absmeans[rownames(absmeans)=="empirical_independence",]
last <- length(absmeans_ref)

for (i in 1:length(Shapley.approx)){
    skillscoremeans[i,] <- (absmeans[i,-last] - absmeans_ref[-last])/(0-absmeans_ref[-last])
}

skillscoremeans <- cbind(skillscoremeans,skillscoremean_total=rowMeans(skillscoremeans))



abssds <- matrix(NA,nrow=length(Shapley.approx), ncol=ncol(Shapley.approx[[1]][[1]])-1)
rownames(abssds) <- names(Shapley.approx)
colnam <- paste0("abssd_X",1:(ncol(Shapley.approx[[1]][[1]])-1))
colnames(abssds) <- colnam

abssd_total <- numeric()
for (i in 1:length(Shapley.approx)){
    abssds[i,] <- colSds(abs(Shapley.true$exactShap[,-1]-Shapley.approx[[i]]$Kshap[,-1]))
    abssd_total[i] <- sd(abs(Shapley.true$exactShap[,-1]-Shapley.approx[[i]]$Kshap[,-1]))
}
abssds <- cbind(abssds,abssd_total=abssd_total)


### Storing the optimal bandwidths (h)
if(X_dim==3){
    these_in_h_optims <- 2:8
} else {
    these_in_h_optims <- 2:176
}

h_optims <- matrix(NA,nrow=length(Shapley.approx), ncol=length(these_in_h_optims))
for (i in 1:length(Shapley.approx)){
    if(!is.null(Shapley.approx[[i]]$other_objects$h_optim_DT)){
        h_optims[i,] <- rowMeans(Shapley.approx[[i]]$other_objects$h_optim_DT[match(these_in_h_optims,varcomb),][,-1])
        #h_optims[i,j] <-Shapley.approx[[i]]$other_objects$h_optim_mat[j,1]
    }
}

colnam_h <- character()
for (j in 1:length(these_in_h_optims)){
    colnam_h[j] <- paste0("h_X_",paste0(l$X$features[[these_in_h_optims[j]]],collapse = "_"))
}

rownames(h_optims) <- names(Shapley.approx)
colnames(h_optims) <- colnam_h
h_optims[grep("independence",rownames(h_optims)),] <- Inf

comp_time <- rep(NA,length(Shapley.approx))
for (i in 1:length(Shapley.approx)){
    comp_time[i] <- Shapley.approx[[i]]$other_objects$comp_time[3]
}

res <- cbind(skillscoremeans,absrelmeans,absmeans,abssds,h_optims)
res.DT <- data.table(res,keep.rownames = T)

res.DT[,comp_time:=comp_time]
res.DT[,true_model:=true_model]
res.DT[,fitted_model:=fitted_model]
res.DT[,variables:=variables]
res.DT[,notes:=notes]
res.DT[,X_dim:=X_dim]
res.DT[,X_GenHyp:=X_GenHyp]

if(variables=="GenHyp"){
    res.DT[,beta.scale:=beta.scale]
    res.DT[,rho:=rho]
    res.DT[,lambda:=lambda]
    res.DT[,omega:=omega]
}
if (variables=="Gaussianmix"){
    res.DT[,mu.scale:=mu.scale]
    res.DT[,rho:=rho]
    res.DT[,pi.G:=pi.G]
}
if (variables=="Gaussian"){
    res.DT[,rho:=rho]
    res.DT[,pi.G:=pi.G]
}


res.DT[,sd_noise:=sd_noise]
res.DT[,nTrain:=nTrain]
res.DT[,nTest:=nTest]
res.DT[,w_threshold:=w_threshold]
res.DT[,n_threshold:=n_threshold]
res.DT[,this.seed:=this.seed]
res.DT[,run_indicator:=run_indicator]
res.DT[,run_date_time:=run_date_time]
