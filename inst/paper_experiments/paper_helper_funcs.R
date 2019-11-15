
stepwiseConstant_fun1 = function(x){
    sign(x)*1
}

stepwiseConstant_fun2 = function(x){
    floor(x/2)*2
}

stepwiseConstant_fun3 = function(x){
    tmp.class = floor(x/2)
    up = (abs(tmp.class) %% 2 == 0)*2-1
    return(up)
}


#x = seq(-10,10,0.1)
#plot(x,stepwiseConstant_fun1(x*0.5),type="l",ylim=c(-3,3),lwd=3)
#lines(x,stepwiseConstant_fun2(x*0.5),col=2)
#lines(x,stepwiseConstant_fun3(x*0.5),col=3)


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

    dens.cond <- exp(log(dens.joint)-log(dens.marginal))

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

        X.given <- as.numeric(Xtest[i,given.inds])
        for (j in 1:length(given.inds)){
            X.grid[,given.inds[j]] <- X.given[j]
        }
        #unique(X.grid)

        integrand.vec <- integrand.func(model = model,
                                        X.grid = X.grid,
                                        given.inds = given.inds,
                                        X.given = X.given,
                                        pi.G = pi.G,
                                        mu.list = mu.list,
                                        Sigma.list = Sigma.list)
        intval[i] <- h * sum(integrand.vec)
    }
    print(paste0("Integration over dim ",integrate.inds," finished."))

    return(intval)
}


integrator.2D.func <- function(h,integrate.inds,given.inds,Xtest,model,X.grid,pi.G,mu.list,Sigma.list){
    nTest <- nrow(Xtest)
    p <- ncol(Xtest)
    intval <- rep(NA,nTest)

    X.new.grid <- matrix(NA,ncol=p,nrow=nrow(X.grid)^2)
    X.new.grid[,integrate.inds] <- as.matrix(expand.grid(as.data.frame(X.grid[,integrate.inds])))

    colnames(X.new.grid) = colnames(Xtest)
    for (i in 1:nTest){
        X.given <- as.numeric(Xtest[i,given.inds])
        for (j in 1:length(given.inds)){
            X.new.grid[,given.inds[j]] <- X.given[j]
        }

        integrand.vec <- integrand.func(model=model,
                                        X.grid=X.new.grid,
                                        given.inds=given.inds,
                                        X.given=X.given,
                                        pi.G=pi.G,
                                        mu.list=mu.list,
                                        Sigma.list=Sigma.list)
        intval[i] <- h^2 * sum(integrand.vec)
    }
    print(paste0("Integration over dim (",paste0(integrate.inds,collapse=", "),") finished."))
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
    # Handles the zero and full models separatedly
    trueValues.mat[,1] = pred_zero
    trueValues.mat[,2^p] = pred_vector(model,data=as.data.frame(Xtest))

    #Exact Shapley values
    exactShap = matrix(NA,nrow(Xtest),p+1)
    for(i in 1:nTest){
        exactShap[i,] <- c(l$W %*% trueValues.mat[i,])
    }

    ret <- list(exactShap=exactShap,trueValue.mat = trueValues.mat)

    return(ret)
}


#### Helper functions for the generlizewd hyperbolic distribution ####

simulateGenHyperbolic <- function(nSim, Sigma, beta,omega,lambda,mu)
{
    dim <- nrow(Sigma)
    W <- rgig(nSim, lambda,omega, omega)
    Z <- matrix(rnorm(nSim * dim), ncol = dim)
    A <- chol(Sigma, pivot = FALSE)
    U <- (Z %*% A)
    M <- matrix(rep(mu,nSim), ncol = dim, byrow = TRUE)
    X <- M+outer(W, beta)+sqrt(W)*U

    X
}


#########################################################################################################################################################

simulateCondDistHyperbolic <- function(nSim, Sigma,lambda,omega,beta,mu,dep.ind,given.ind,x0)
{
    d1 <- length(given.ind)
    beta1 <- beta[given.ind]
    beta2 <- beta[dep.ind]
    Sigma11 <- as.matrix(Sigma[given.ind,given.ind])
    Sigma2 <- as.matrix(Sigma[dep.ind,dep.ind])
    Sigma12 <- matrix(Sigma[given.ind,dep.ind],ncol=length(dep.ind),byrow=T)
    x1 <- x0[given.ind]
    mu1 <- mu[given.ind]
    mu2 <- mu[dep.ind]

    lambda21  <- lambda - d1/2
    psi21     <- omega  +       t(beta1)%*%solve(Sigma11)%*%beta1
    Sigma21   <- Sigma2 -t(Sigma12)%*%solve(Sigma11)%*%Sigma12
    chi21     <- omega  + t(x1-mu1)%*%solve(Sigma11)%*%(x1-mu1)
    mu21      <- mu2    +    t(Sigma12)%*%solve(Sigma11)%*%(x1-mu1)
    beta21    <- beta2  -   t(Sigma12)%*%solve(Sigma11)%*%beta1

    if(length(dep.ind)==1)
    {
        univ.ghyp <- ghyp(lambda=as.numeric(lambda21), chi=as.numeric(chi21), psi=as.numeric(psi21), mu=as.numeric(mu21), sigma=as.numeric(Sigma21), gamma=as.numeric(beta21))
        simData <- rghyp(n = nSim, univ.ghyp)
    }
    else
    {
        multiv.ghyp <- ghyp(lambda=lambda21, chi=as.numeric(chi21), psi=as.numeric(psi21), mu=mu21, sigma=Sigma21, gamma=beta21)
        simData <- rghyp(n = nSim, multiv.ghyp)
    }
    simData
}

simulateCondDistHyperbolic_new <- function(given.ind, n_threshold, Sigma,lambda,omega,beta,mu,p,Xtest)
{
    if (length(given.ind) %in% c(0, p)) {
        ret <- matrix(Xtest, ncol = p, nrow = 1)
    } else {
        # Transforming some variables for below
        nSim <- n_threshold
        x0 <- Xtest
        dep.ind <- (1:length(mu))[-given.ind]
        X_given <- Xtest[given.ind]


        ## Core function of "simulateCondDistHyperbolic"
        d1 <- length(given.ind)
        beta1 <- beta[given.ind]
        beta2 <- beta[dep.ind]
        Sigma11 <- Sigma[given.ind,given.ind,drop=FALSE]
        Sigma2 <- Sigma[dep.ind,dep.ind,drop=FALSE]
        Sigma12 <- Sigma[given.ind,dep.ind,drop=FALSE]
        x1 <- x0[given.ind]
        mu1 <- mu[given.ind]
        mu2 <- mu[dep.ind]

        lambda21  <- lambda - d1/2
        psi21     <- omega  +       t(beta1)%*%solve(Sigma11)%*%beta1
        Sigma21   <- Sigma2 -t(Sigma12)%*%solve(Sigma11)%*%Sigma12
        chi21     <- omega  + t(x1-mu1)%*%solve(Sigma11)%*%(x1-mu1)
        mu21      <- mu2    +    t(Sigma12)%*%solve(Sigma11)%*%(x1-mu1)
        beta21    <- beta2  -   t(Sigma12)%*%solve(Sigma11)%*%beta1

        if(length(dep.ind)==1){
            univ.ghyp <- ghyp(lambda=as.numeric(lambda21), chi=as.numeric(chi21), psi=as.numeric(psi21), mu=as.numeric(mu21), sigma=as.numeric(Sigma21), gamma=as.numeric(beta21))
            simData <- rghyp(n = nSim, univ.ghyp)
        }
        else{
            multiv.ghyp <- ghyp(lambda=lambda21, chi=as.numeric(chi21), psi=as.numeric(psi21), mu=mu21, sigma=Sigma21, gamma=beta21)
            simData <- rghyp(n = nSim, multiv.ghyp)
        }

        ret <- matrix(NA, ncol = p, nrow = n_threshold)
        ret[, given.ind] <- rep(X_given,each=n_threshold)
        ret[, dep.ind] <- simData
    }
    colnames(ret) <- colnames(Xtest)
    return(as.data.table(ret))
}



