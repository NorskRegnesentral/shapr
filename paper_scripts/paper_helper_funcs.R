
#### Should probably document these helper functions  ####

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

integrand.func <- function(model,X.grid,given.inds,X.given,pi.G,mu.list,Sigma.list){

    gx = pred_vector(model,data=as.data.frame(X.grid))


    xDens <- cond.dens.X.func(X=X.grid,
                              given.ind=given.inds,
                              X.given=X.given,
                              pi.G=pi.G,
                              mu.list =mu.list,
                              Sigma.list=Sigma.list)
    gx*xDens
}

integrator.1D.func <- function(h,integrate.inds,given.inds,Xtest,model,X.grid,pi.G,mu.list,Sigma.list){
    nTest <- nrow(Xtest)
    intval <- rep(NA,nTest)
    for (i in 1:nTest){

        x.given <- as.numeric(Xtest[i,given.inds])
        for (j in 1:length(given.inds)){
            X.grid[,given.inds[j]] <- x.given[j]
        }

        integrand.vec <- integrand.func(model,X.grid,given.inds,x.given,pi.G,mu.list,Sigma.list)
        #intval[i] <- h*(integrand.vec[1]/2 + sum(integrand.vec[2:n]) + integrand.vec[n+1]/2)
        intval[i] <- h * sum(integrand.vec) #h*(integrand.vec[1]/2 + sum(integrand.vec[2:n]) + integrand.vec[n+1]/2)

    }
    print(paste0("Integration over ",integrate.inds," finished."))

    return(intval)
}

integrator.2D.func <- function(h,integrate.inds,given.inds,Xtest,model,X.grid,pi.G,mu.list,Sigma.list){
    nTest <- nrow(Xtest)
    intval <- rep(NA,nTest)

    X.new.sub <- matrix(NA,ncol=3,nrow=nrow(X.grid)^2)
    X.new.sub[,integrate.inds] <- as.matrix(expand.grid(as.data.frame(X.grid[,integrate.inds])))

    colnames(X.new.sub) = colnames(Xtest)
    for (i in 1:nTest){
        X.given <- as.numeric(Xtest[i,given.inds])
        for (j in 1:length(given.inds)){
            X.new.sub[,given.inds[j]] <- X.given[j]
        }

        #    integrand.vec <- (X.new.sub[,2]>0)*(X.new.sub[,2]<=1)*(X.new.sub[,3]>0)*(X.new.sub[,3]<=1)*1
        integrand.vec <- integrand.func(model=model,
                                        X.grid=X.new.sub,
                                        given.inds=given.inds,
                                        X.given=X.given,
                                        pi.G=pi.G,
                                        mu.list=mu.list,
                                        Sigma.list=Sigma.list)
        #intval[i] <- h*(integrand.vec[1]/2 + sum(integrand.vec[2:n]) + integrand.vec[n+1]/2)
        intval[i] <- h^2 * sum(integrand.vec) #h*(integrand.vec[1]/2 + sum(integrand.vec[2:n]) + integrand.vec[n+1]/2)
    }
    print(paste0("Integration over (",paste0(integrate.inds,collapse=", "),") finished."))
    return(intval)
}

Shapley_true = function(model,Xtrain,Xtest,pi.G,mu.list,Sigma.list,int.samp=500,l,pred_zero){
    p = ncol(Xtest)
    nTest <- nrow(Xtest)
    Xtraintest = rbind(Xtrain,Xtest)
    Xrange = range(Xtraintest)
    Xsd = mean(diag(var(Xtraintest)))

    a <- Xrange[1]-0.5*Xsd
    b <- Xrange[2]+0.5*Xsd
    h <- (b-a)/int.samp
    X.grid <-seq(a+h/2, b-h/2, by=h) %x%t(rep(1,p))
    colnames(X.grid) = colnames(Xtrain)

    trueValues.mat = matrix(NA,ncol=2^p,nrow=nrow(Xtest))
    for (i in (2:(2^p-1))){
        given.inds = which(l$S[i,]==1)
        integrate.inds = which(l$S[i,]==0)
        if(length(integrate.inds)==1){
            trueValues.mat[,i]=integrator.1D.func(h = h,
                                                      integrate.inds=integrate.inds,
                                                      given.inds=given.inds,
                                                      Xtest= as.matrix(Xtest),
                                                      model = model,
                                                      X.grid = X.grid,
                                                      pi.G = pi.G,
                                                      mu.list = mu.list,
                                                      Sigma.list = Sigma.list)
        } else {
            trueValues.mat[,i]=integrator.2D.func(h = h,
                                                  integrate.inds=integrate.inds,
                                                  given.inds=given.inds,
                                                  Xtest= as.matrix(Xtest),
                                                  model = model,
                                                  X.grid = X.grid,
                                                  pi.G = pi.G,
                                                  mu.list = mu.list,
                                                  Sigma.list = Sigma.list)

        }
    }
    trueValues.mat[,1] = pred_zero
    trueValues.mat[,2^p] = pred_vector(model,data=as.data.frame(Xtest))

    #Eksakt Shapley values
    exactShap = matrix(NA,nrow(Xtest),p+1)
    for(i in 1:nTest){
        exactShap[i,] <- c(l$W %*% trueValues.mat[i,])
    }

    return(exactShap)
}

