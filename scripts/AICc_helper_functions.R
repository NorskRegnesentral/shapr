library(mvtnorm)

#k.func.Euclidean <- function(x){
#    dnorm(x = x,mean = 0,sd = 1)
#}
#K.func.Euclidean <- function(x,h.vec,Sigma){
#    prod(k.func.Euclidean((x)/h.vec)/h.vec)
#}
K.func.Mahalanobis.all <- function(X,h.vec,Sigma,S_scale_dist){
    exp(-gen_Mahlanobis_dist_cpp(featureList = list(1,1:ncol(X)),Xtrain = X,Xtest = X,mcov = Sigma,S_scale_dist = S_scale_dist)[,,2]/(2*h.vec^2))
    }


# h.vec is vector of length q=ncol(X)
H.func <- function(h.vec,X,kernel = "Euclidean",scale_var=T,S_scale_dist){ #### SPEEDUP: Make an Rcpp function doing all of this with Mahlanobis only.
    n <- nrow(X)

    H <- matrix(NA,ncol=n,nrow=n)
    Sigma <- cov(X)   #### SPEEDUP: May move this outside both the H.func and the AICc-function.

    if (kernel=="Euclidean"){
        ### OLD VERSION
        # K.func <- K.func.Euclidean
        # for (i in 1:n){
        #     for (j in 1:n){
        #         H[i,j] <- K.func(x = X[i,]-X[j,],h.vec=h.vec,Sigma=Sigma)
        #     }
        #     H[i,] <- H[i,]/sum(H[i,])
        # }
        Sigma.var <- 0*Sigma
        if(scale_var){
            diag(Sigma.var) <- diag(Sigma)
        } else {
            diag(Sigma.var) <- 1
        }
        H <- K.func.Mahalanobis.all(X=X,h.vec=h.vec,Sigma=Sigma.var,S_scale_dist = F) # So including the variance in this measure, not to have to scale for different variability in different dimensions.
        for (i in 1:n){
          H[i,] <- H[i,]/sum(H[i,])
      }


    }

    if (kernel=="Mahalanobis"){
        H <- K.func.Mahalanobis.all(X=X,h.vec=h.vec,Sigma=Sigma,S_scale_dist = S_scale_dist)
        H <- H/rowSums(H)

        # OLD CODE
        #        for (i in 1:n){
        #            H[i,] <- H[i,]/sum(H[i,])
        #        }



    }

    return(H)
}

######
# h.vec is vector of length q=ncol(X)
# Requires X to include a .id column as the first column in the X matrix, identifying how the H-matrix should be
# diagonalized

H.func.new <- function(h.vec,X,kernel = "Euclidean",scale_var=T,S_scale_dist){ #### SPEEDUP: Make an Rcpp function doing all of this witha loop of uniqe ids, and where X is only the matrix part, and
                                                                               #### the .idcol is supplied as an integer vector to the function. Include only Mahlanobis only, Sigma supplied directly and with scaling of Mahlanobis, not the variables themselves
    H.list <- list()
    for(i in unique(X$.id)){
        H.list[[i]] <- H.func(h.vec=h.vec,
                              X = as.matrix(X[.id == i,-1]),
                              kernel = kernel,
                              scale_var = scale_var,
                              S_scale_dist = S_scale_dist)
    }
    H <- as.matrix(Matrix::bdiag(H.list)) # Inefficient, but will be coding this up in Rcpp anyway
    return(H)
}



sigma.hat.sq.func <- function(y,H){
    n <- length(y)

    sigma.hat.sq <- as.numeric(t(y)%*%t(diag(n)-H)%*%(diag(n)-H)%*%y)/n

    return(sigma.hat.sq)
}

AICc.func <- function(h.vec,y,X,negative = FALSE,kernel = "Euclidean",scale_var = T,S_scale_dist = F){
    n <- length(y)
    q <- ncol(X)

    if (length(h.vec)==1){
        h.vec <- rep(h.vec,q)
    }

    H <- H.func(h.vec = h.vec,X = X,kernel = kernel, scale_var = scale_var, S_scale_dist = S_scale_dist)

    sigma.hat.sq <- sigma.hat.sq.func(y=y,
                                      H = H)

    tr.H <- sum(diag(H))
    correction.term <- (1+tr.H/n)/(1-(tr.H+2)/n)

    AICc <- log(sigma.hat.sq) + correction.term
    if(negative){
        AICc <- -AICc
    }
    return(AICc)
}

# Requires X to include a .id column as the first column in X, which now is a data.table object, identifying how the H-matrix should be
# diagonalized

AICc.func.new <- function(h.vec,y,X,negative = FALSE,kernel = "Euclidean",scale_var = T,S_scale_dist = F,idcol = T){
    n <- length(y)
    q <- ncol(X) - idcol

    if (length(h.vec)==1){
        h.vec <- rep(h.vec,q)
    }

    H <- H.func.new(h.vec = h.vec,X = X,kernel = kernel, scale_var = scale_var, S_scale_dist = S_scale_dist) #### SPEEDUP: Rcpp this

    sigma.hat.sq <- sigma.hat.sq.func(y=y,    #### SPEEDUP: Rcpp this with log taken automatically
                                      H = H)

    tr.H <- sum(diag(H))
    correction.term <- (1+tr.H/n)/(1-(tr.H+2)/n) #### SPEEDUP: Rcpp this with log taken automatically

    AICc <- log(sigma.hat.sq) + correction.term
    if(negative){
        AICc <- -AICc
    }

    print(sigma.hat.sq)
    print(correction.term)
    print(tr.H)

    return(AICc)
}



samp_train_test_comb <- function(nTrain,nTest,nosamp){

    sampinds <- 1:(nTrain*nTest)
    if (nosamp < max(sampinds)){
        input_samp <- sample(x = sampinds,
                             size = nosamp,
                             replace = F)
    } else {
        input_samp <- sampinds
    }

    #               Test using input_samp=c(1,2,3, 1999, 2000 ,2001 ,2002)
    samp_train <- (input_samp-1) %% nTrain + 1
    samp_test <- (input_samp-1) %/% nTrain + 1

    ret <- data.frame(samp_train = samp_train, samp_test = samp_test)
    return(ret)
}



g.hat.func <- function(x,h.vec,y,XMAT,kernel = "Euclidean"){

    n <- nrow(XMAT)
    q <- ncol(XMAT)

    if (length(h.vec)==1){
        h.vec <- rep(h.vec,q)
    }

    if (kernel=="Euclidean"){
        K.func <- K.func.Euclidean
    }
    if (kernel=="Mahalanobis"){
        K.func <- K.func.Mahalanobis
    }

    K <- rep(NA,n)
    for (i in 1:n){
        K[i] <- K.func(x=XAT[i,]-x,h.vec=h.vec,Sigma=cov(XMAT))
    }
    g.hat <- sum(y*K)/sum(K)

    return(g.hat)
}

g.hat.func.vec <- Vectorize(g.hat.func,vectorize.args = "x")
