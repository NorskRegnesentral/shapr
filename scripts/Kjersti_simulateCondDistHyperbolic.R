library(condMVNorm)
library(ghyp)
library(Matrix)


####################################################################################################################
#10-dimensjonalt eksempel
nSim <- 1000000
dim <- 10

#Parametere som er felles i de to eksemplene
lambda <- 1
mu     <- c(1:3,1:3,1:3,3)

####################################################################################################################
#### Simulerer forst fra en GHYP som er veldig naer en normalfordeling ###################################################
####################################################################################################################
Sigma <- matrix(0,10,10)
Sigma[1:5,1:5]   <- 0.9
Sigma[6:10,6:10] <- 0.5
Sigma[1:5,6:10]  <- 0.2
Sigma[6:10,1:5]  <- 0.2
diag(Sigma)      <- 1
beta <- rep(0,10)
omega <- 100
X <- simulateGenHyperbolic(nSim, Sigma, beta,omega,lambda,mu)
pairs(X[1:1000,])
par(mfrow=c(3,3))
for(i in 1:9)
hist(X[,i])
round(cor(X),2)
Y <- 0.5*X[,2]  +  (X[,1]<0)*1 + (X[,3]<2) + (X[,4]>4)*1 + (X[,5]<6)*1 + (X[,6]<0)*1 + (X[,7]>-2)*(X[,8]<4)*1+ X[,9] + X[,10] + rnorm(nSim,0,0.1)

muNorm <- apply(X,2,mean)
covMat <- var(X)

dep.ind   <- 2:10
given.ind <- 1
x0 <- rep(2,10)
simData <- simulateCondDistHyperbolic(nSim=10000, Sigma,lambda,omega,beta,mu,dep.ind,given.ind,x0)
normData <- simulateCondDistGaussian(nSim=10000, muNorm,covMat,dep.ind,given.ind,x0)
plotDistributions(simData,normData,dep.ind,given.ind)


####################################################################################################################
#### Simulerer saa fra en GHYP som er langt unna en normalfordeling ###################################################
####################################################################################################################
Sigma <- diag(c(1:3,1:3,1:3,3))
beta <- c(rep(1,5),rep(0.5,5))
omega <- 0.5
X <- simulateGenHyperbolic(nSim, Sigma, beta,omega,lambda,mu)
pairs(X[1:1000,])
par(mfrow=c(3,3))
for(i in 1:9)
hist(X[,i])
round(cor(X),2)
Y <- 0.5*X[,2]  +  (X[,1]<0)*1 + (X[,3]<2) + (X[,4]>4)*1 + (X[,5]<6)*1 + (X[,6]<0)*1 + (X[,7]>-2)*(X[,8]<4)*1+ X[,9] + X[,10] + rnorm(nSim,0,0.1)

muNorm <- apply(X,2,mean)
covMat <- var(X)

dep.ind   <- 2:10
given.ind <- 1
x0 <- rep(2,10)
simData <- simulateCondDistHyperbolic(nSim=10000,Sigma,lambda,omega,beta,mu,dep.ind,given.ind,x0)
normData <- simulateCondDistGaussian(nSim=10000, muNorm,covMat,dep.ind,given.ind,x0)
plotDistributions(simData,normData,dep.ind,given.ind)


###########################################################################################################################################################

simulateGenHyperbolic <- function(nSim, Sigma, beta,omega,lambda,mu)
{
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
###############################################################################################################################################################
simulateCondDistGaussian <- function(nSim,muNorm,covMat,dep.ind,given.ind,x0)
{
 x1 <- x0[given.ind]

 ret <- condMVN(X.given = x1, mean = muNorm, sigma = covMat, dependent.ind = dep.ind, given.ind = given.ind)
 sigma = as.matrix(nearPD(ret$condVar, corr = FALSE, keepDiag = FALSE)$mat)
 if(length(dep.ind)==1)
 normData <- rnorm(nSim, mean = ret$condMean, sd = sigma)
 else
 normData <- rmvnorm(nSim, mean = ret$condMean, sigma = sigma)

 normData
}

#########################################################################################################################################################
plotDistributions <- function(simData,normData,dep.ind,given.ind)
{
  if(length(dep.ind)==1)
  {
   par(mfrow=c(1,1))
   qqplot(simData,normData)    
   lines(simData,simData)
   print(c(sqrt(var(simData)),sqrt(var(normData))))
   print(summary(normData))
   print(summary(simData))
  }
  else
  {
   par(mfrow=c(3,3))
   for(i in 1:dim(simData)[2])
    {
     qqplot(simData[,i],normData[,i])    
     lines(simData[,i],simData[,i])
    }
   print(summary(normData))
   print(summary(simData))
   print(apply(normData,2,var))
   print(apply(simData,2,var))
  } 
}