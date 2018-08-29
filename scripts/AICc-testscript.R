
### Trying out methods to choose the bandwidth parameter (sigma).

### Stage our problem as a local nonlinear regression problem. The formula we use
## is equicalent to local constatn regression (nadaray-watson).



### Trying the bandwith selector for this problem implemented in the np package

# Sample some data. The response y here, would correspond to the prediction s(y,x)
rm(list=ls())
set.seed(12345)

n <- 500
x1 <- rnorm(n)
x2 <- runif(n,-2,2)
x3 <- rnorm(n)
X <- cbind(x1,x2)
pred <- x1 + x2 +x3 + rnorm(n)


library(np)
bw <- npregbw(formula=pred~x1+x2+x3,regtype="lc",bwmethod="cv.aic")

### It is probably working, but it is quite slow only with n=500.


#### Implementing the AICc myself based on the formula in section 2.3.3. (page 72) of the Li and Racine book:

X <- cbind(x1,x2,x3)


k.func <- function(x){
    dnorm(x = x,mean = 0,sd = 1)
}

h.vec <- c(1,2,4)

Kh.ij.func <- function(i,j,h.vec,X){ # h.vec defined globally
    prod(k.func((X[i,]-X[j,])/h.vec)/h.vec)
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

sigma.hat.sq.func <- function(pred,H){
    n <- length(pred)

    sigma.hat.sq <- as.numeric(t(pred)%*%t(diag(n)-H)%*%(diag(n)-H)%*%pred)/n

    return(sigma.hat.sq)
}

AICc.func <- function(h.vec,pred,X,negative = FALSE){
    n <- length(pred)

    H <- H.func(h.vec = h.vec,X = X)

    sigma.hat.sq <- sigma.hat.sq.func(pred=pred,
                                      H = H)

    tr.H <- sum(diag(H))
    correction.term <- (1+tr.H/n)/(1-(tr.H+2)/n)

    AICc <- log(sigma.hat.sq) + correction.term
    if(negative){
        AICc <- -AICc
    }
    return(AICc)
}

AICc.func(c(1,2,3),pred,X)

#nlminb(start = c(1,1,1),objective = AICc.func,pred=pred,X=X,negative=T)


AICc.single.h.func <- function(h,pred,X,negative = FALSE){
    n <- length(pred)
    q <- ncol(X)

    H <- H.func(h.vec = rep(h,q),X = X)

    sigma.hat.sq <- sigma.hat.sq.func(pred=pred,
                                      H = H)

    tr.H <- sum(diag(H))
    correction.term <- (1+tr.H/n)/(1-(tr.H+2)/n)

    AICc <- log(sigma.hat.sq) + correction.term
    if(negative){
        AICc <- -AICc
    }
    return(AICc)
}

AICc.single.h.func(1,pred,X)

h.val <- seq(0.2,3,0.2)
h.val <- seq(0.02,0.4,0.02)

AICc.vals <- rep(NA,length(h.val))
for (i in 1:length(h.val)){
    (AICc.vals[i] <- AICc.single.h.func(h=h.val[i],pred=pred,X=X,negative=F))
    print(AICc.vals)
}




nlminb(start = 1,objective = AICc.func,pred=pred,X=X,negative=T)


# ## Trying to vectorize...
# vec.Kh.ij.func <- Vectorize(Kh.ij.func,vectorize.args = c("i","j"))
#
# mat.k.func <- function(MAT){
#     apply(X = MAT,MARGIN = c(1,2),FUN = k.func)
# }
# Kh.ij.func.2 <- function(i,j,h.vec,X){ # h.vec defined globally
#     prod(mat.k.func((X[i,]-X[j,])/h.vec)/h.vec)
# }
#
# outer(1:n,1:n,FUN=vec.Kh.ij.func,h.vec=h.vec,X=X)



