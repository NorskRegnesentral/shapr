
### Trying out methods to choose the bandwidth parameter (sigma).

### Stage our problem as a local nonlinear regression problem. The formula we use
## is equicalent to local constatn regression (nadaray-watson).



### Trying the bandwith selector for this problem implemented in the np package

# Sample some data. The response y here, would correspond to the yiction s(y,x)
rm(list=ls())
set.seed(12345)

n <- 500
x1 <- rnorm(n)
x2 <- runif(n,-2,2)
x3 <- rnorm(n)
X <- cbind(x1,x2,x3)
y <- x1 + x2 +x3 + rnorm(n)


library(np)
bw <- npregbw(formula=y~x1+x2+x3,regtype="lc",bwmethod="cv.aic")

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

AICc.func(c(1,2,3),y,X)

# Should give the same answer -- and it does
AICc.func(c(1,1,1),y,X)
AICc.func(1,y,X)


#nlminb(start = c(1,1,1),objective = AICc.func,y=y,X=X,negative=T)

# h.val <- seq(0.2,3,0.2)
# h.val <- seq(0.02,0.4,0.02)
#
# AICc.vals <- rep(NA,length(h.val))
# for (i in 1:length(h.val)){
#     (AICc.vals[i] <- AICc.single.h.func(h=h.val[i],y=y,X=X,negative=F))
#     print(AICc.vals)
# }




#nlminb(start = 1,objective = AICc.func,y=y,X=X,negative=T)


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


################## Trying with only one variable #############

set.seed(12345)

n <- 500
x1 <- rnorm(n)
y <- x1 + rnorm(n)
X <- cbind(x1)


AICc.single.h.func(1,y,X)

# h.val <- seq(0.02,2,0.02)
#
# AICc.vals <- rep(NA,length(h.val))
# for (i in 1:length(h.val)){
#     (AICc.vals[i] <- AICc.single.h.func(h=h.val[i],y=y,X=X,negative=F))
#     print(AICc.vals[i])
# }


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

g.hat.func(1,1,y,X)

g.hat.func.vec <- Vectorize(g.hat.func,vectorize.args = "x")

x.val <- seq(-3,3,0.01)
true.y <- x.val

ksmooth.val <- ksmooth(x1, y, kernel = "normal", bandwidth = 0.3,
                       range.x = range(x.val),
                       n.points = length(x.val))

library(KernSmooth)
locpoly.val <- locpoly(x1, y, degree=0, kernel="normal", bandwidth=.3,
                       range.x = range(x.val),
                       gridsize = length(x.val))

plot(locpoly.val,type="l")
lines(ksmooth.val,col=2)
lines(x.val,g.hat.func.vec(x.val,0.3,y,X),col=3)

h.val <- seq(0.1,1,0.04)

### sample some test data#
#
#n <- 10000
#x1.test <- rnorm(n)
#y.test <- x1.test + rnorm(n)


par(mfrow=c(2,2))
MSE.vec <- rep(NA,length(h.val))
plot(x1,y,asp = 1,xlim=range(x.val))
lines(x.val,true.y)
for (i in 1:length(h.val)){
#    pred.y <- g.hat.func.vec(x.val,h.val[i],y,X)
    pred.y <-locpoly(x1, y, degree=0, kernel="normal", bandwidth=h.val[i],
                     range.x = range(x.val),
                     gridsize = length(x.val))$y # Same results, just faster
    MSE.vec[i] <- mean((pred.y-true.y)^2)
    lines(x.val,pred.y,col=2)
}

lines(x.val,locpoly(x1, y, degree=0, kernel="normal", bandwidth=0.215,
                    range.x = range(x.val),
                    gridsize = length(x.val))$y,col=4,lwd=3)
lines(x.val,locpoly(x1, y, degree=0, kernel="normal", bandwidth=0.3,
                    range.x = range(x.val),
                    gridsize = length(x.val))$y,col=3,lwd=3)

plot(h.val,MSE.vec,type='l')


AICc.vec <- rep(NA,length(h.val))
for (i in 1:length(h.val)){
    AICc.vec[i] <- AICc.single.h.func(h.val[i],y,X)
    print(AICc.vec[i])
}

plot(h.val,AICc.vec,type='l')
bw <- npregbw(formula=y~x1,regtype="lc",bwmethod="cv.aic")

###########################333


n <- 500
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
X <- cbind(x1,x2,x3)
y <- x1 + x2 +x3 + rnorm(n,sd=0.1)
model <- lm(y~x1+x2+x3)

y.pred.12 <- x1+x2 # conditioning on x3=0
X[,3] <- 0

### sample some test data#

n.test <- 1000
x1.test <- rnorm(n.test)
x2.test <- rnorm(n)
x3.test <- rnorm(n.test)
X.test <- cbind(x1.test,x2.test,x3.test)
y.test <- x1.test + x2.test +x3.test + rnorm(n.test,sd=0.1)


MSE.testvec <- rep(NA,length(h.val))
for (i in 1:length(h.val)){
    pred.y <- apply(X = X.test, MARGIN = 1, FUN = g.hat.func, h.vec = h.val[i],y=y,XMAT=X)
    #pred.y <-locpoly(x1, y, degree=0, kernel="normal", bandwidth=h.val[i],
    #                 range.x = range(x.val),
    #                 gridsize = length(x.val))$y # Same results, just faster
    MSE.testvec[i] <- mean((pred.y-y.test)^2)
    print(c(h.val[i],MSE.testvec[i]))
}

AICc.vec <- rep(NA,length(h.val))
for (i in 1:length(h.val)){
    AICc.vec[i] <- AICc.func(h.val[i],y.pred.12,X)
    print(AICc.vec[i])
}


par(mfrow=c(2,1))
plot(h.val,MSE.testvec,type='l')
plot(h.val,AICc.vec+5,type='l',log="y")


#### How well does the conditioned version work? ####

MSE.testvec <- rep(NA,length(h.val))
for (i in 1:length(h.val)){

    n <- nrow(X)
    q <- ncol(X)
    h.vec <- rep(h.val,q)

    K <- rep(NA,n)
    for (i in 1:n){
        K[i] <- prod(k.func((X[i,]-x)/h.vec))
    }


    pred.y <- apply(X = X, MARGIN = 1, FUN = g.hat.func, h.vec = h.val[i],y=y.pred.12,XMAT=X)
    #pred.y <-locpoly(x1, y, degree=0, kernel="normal", bandwidth=h.val[i],
    #                 range.x = range(x.val),
    #                 gridsize = length(x.val))$y # Same results, just faster
    mean(pred.y)
    MSE.testvec[i] <- mean((pred.y-y.test)^2)
    print(c(h.val[i],MSE.testvec[i]))
}




