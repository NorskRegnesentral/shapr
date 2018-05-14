
#### DOCUMENT THESE FUNCTION AND PUT THEM IN THE R FOLDER LATER  ####



dens.X.func <- function(X,pi.G,mu.list,Sigma.list){
    no.mix <- length(pi.G)
    dens <- 0
    for (i in 1:no.mix){
        dens <- dens + pi.G[i]*dmvnorm(x = X, mean = mu.list[[i]], sigma = Sigma.list[[i]])
    }
    return(dens)
}

# Computes the conditional density of X given certain dimensions of that matrix
# X should contain all dimensions. Those who are given.ind are not used however. Just fill inn zeros or so.
# X.given, however, just contains the variables that is conditioned upon, in the provided by given.ind
cond.dens.X.func <- function(X,given.ind,X.given,pi.G,mu.list,Sigma.list){

    no.mix <- length(pi.G)

    mu.list.given <- list()
    Sigma.list.given <- list()
    for (i in 1:no.mix){
        mu.list.given[[i]] <- mu.list[[i]][given.ind,drop=FALSE]
        Sigma.list.given[[i]] <- Sigma.list[[i]][given.ind,given.ind,drop=FALSE]
    }

    p <- ncol(X)
    dependent.ind <- which(!(1:p %in% given.ind))
    X.dependent <- X[,dependent.ind,drop=FALSE]

    X.dependent.and.given <- X
    for (i in 1:length(given.ind)){
        X.dependent.and.given[,given.ind[i]] <- X.given[i]
    }

    X.given.mat <- matrix(X.given,nrow=1) # Making sure it is a matrix

    dens.joint <-dens.X.func(X.dependent.and.given,pi.G,mu.list,Sigma.list)
    dens.marginal <-dens.X.func(X.given.mat,pi.G,mu.list.given,Sigma.list.given)

    dens.cond <- dens.joint/dens.marginal

    return(dens.cond)
}

joint.samp.func <- function(n,pi.G,mu.list,Sigma.list){
    no.mix <- length(pi.G)
    G <- as.factor(sample(x=1:no.mix,size = n,replace = T,prob = pi.G))
    X <- matrix(NA,ncol=length(mu.list[[1]]),nrow = n)
    for (i in 1:no.mix){
        these <- (G==i)
        X[these,] <- rmvnorm(n = sum(these),mean = mu.list[[i]],sigma=Sigma.list[[i]])
    }
    dat <- data.frame(X)

    return(dat)
}

