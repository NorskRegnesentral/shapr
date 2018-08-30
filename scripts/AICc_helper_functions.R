k.func <- function(x){
    dnorm(x = x,mean = 0,sd = 1)
}
# h.vec is vector of length q=ncol(X)
H.func <- function(h.vec,X){
    n <- nrow(X)

    H <- matrix(NA,ncol=n,nrow=n)

    for (i in 1:n){
        for (j in 1:n){
            H[i,j] <- prod(k.func((X[i,]-X[j,])/h.vec)/h.vec)
        }
        H[i,] <- H[i,]/sum(H[i,])
    }
    return(H)
}

sigma.hat.sq.func <- function(y,H){
    n <- length(y)

    sigma.hat.sq <- as.numeric(t(y)%*%t(diag(n)-H)%*%(diag(n)-H)%*%y)/n

    return(sigma.hat.sq)
}

AICc.func <- function(h.vec,y,X,negative = FALSE){
    n <- length(y)
    q <- ncol(X)

    if (length(h.vec)==1){
        h.vec <- rep(h.vec,q)
    }

    H <- H.func(h.vec = h.vec,X = X)

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
g.hat.func <- function(x,h.vec,y,XMAT){

    n <- nrow(XMAT)
    q <- ncol(XMAT)

    if (length(h.vec)==1){
        h.vec <- rep(h.vec,q)
    }
    K <- rep(NA,n)
    for (i in 1:n){
        K[i] <- prod(k.func((XMAT[i,]-x)/h.vec))
    }
    g.hat <- sum(y*K)/sum(K)

    return(g.hat)
}

g.hat.func.vec <- Vectorize(g.hat.func,vectorize.args = "x")
