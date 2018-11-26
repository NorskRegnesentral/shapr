#### Implementing AICC in Rcpp


library(data.table)
library(inline)
library(shapr)
library(Rcpp)

#### Starting with the H function (the regular one, taking X as an input)

h.vec = rep(0.2,3)
h = 0.2

set.seed(123)

X = matrix(rnorm(50*3),ncol=3)
#X = data.table(.id=1,X)


Sigma <- cov(X)   #### SPEEDUP: May move this outside both the H.func and the AICc-function.

### For R function

#### First H_cpp version done. Working well and giveing correct answer

kernel="Mahalanobis"
scale_var=F
S_scale_dist = T

#sourceCpp("src/AICc.cpp")
#

aa_new = H_cpp(X,mcov = Sigma,S_scale_dist = S_scale_dist,h = h)

aa_old = H.func(h,X=X,kernel = kernel,scale_var=scale_var,S_scale_dist=S_scale_dist)

#aa_old2 = K.func.Mahalanobis.all(X=X,h.vec=h,Sigma=Sigma,S_scale_dist = S_scale_dist)

dim(aa_new)

all.equal(aa_new,aa_old)

### FORTSETT HER !!!!

### lag Rcpp funksjon ac sigma.hat.sq.func også, samt correction term, og til slutt en hovedfunksjon som
# tar inn Rcpp:List med alt som trengs, og så looper gjennom hver del og til slutt summerer alt til AICc-formelen


#sourceCpp("src/AICc.cpp")

X = matrix(rnorm(5*3),ncol=3)
Sigma <- cov(X)+0.1   #### SPEEDUP: May move this outside both the H.func and the AICc-function.

H = H_cpp(X,mcov = Sigma,S_scale_dist = S_scale_dist,h = 0.1)
y = rnorm(nrow(H))

sighat_new = sigma_hat_sq_cpp(H,y,ret_log=F)

sighat_old = sigma.hat.sq.func(y,H)

all.equal(sighat_new,sighat_old)


#sourceCpp("src/AICc.cpp")

correction_term_cpp(H)
tr.H <- sum(diag(H))
n = nrow(H)
(correction.term <- (1+tr.H/n)/(1-(tr.H+2)/n))


####### NOW we implement the single AICc function

sourceCpp("src/AICc.cpp")

h = 0.2
set.seed(123)
n <- 10000
p = 10

X = matrix(rnorm(n*p),ncol=p)
mcov <- cov(X)   #### SPEEDUP: May move this outside both the H.func and the AICc-function.
y = rnorm(nrow(X))

kernel="Mahalanobis"
scale_var=F
S_scale_dist = T
negative = F

X.dt = data.table(.id=1,X)

tt <- proc.time()
AICc_single_cpp(X = X,
                mcov = mcov,
                S_scale_dist = S_scale_dist,
                h = h,
                y=y,
                negative = negative)
print(proc.time()-tt)
#  9.804   0.747   9.230

tt <- proc.time()
AICc.func.new(h.vec =h,
              y = y,
              X = X.dt,
              negative = negative,
              kernel = kernel,
              scale_var = scale_var,
              S_scale_dist = S_scale_dist,
              idcol = T)
print(proc.time()-tt)
#  12.440   2.578  15.293


########### Now trying the complete thing for mulitple X's

sourceCpp("src/AICc.cpp")

h = 0.2
set.seed(123)
n <- 100
p = 10
K = 2
kernel="Mahalanobis"
scale_var=F
S_scale_dist = T
negative = F

X_list <- list()
mcov_list <- list()
y_list <- list()

X_list_dt <- list()
for (k in 1:K){
    X_list[[k]] = matrix(rnorm(n*p),ncol=p)
    mcov_list[[k]] <- cov(X_list[[k]])   #### SPEEDUP: May move this outside both the H.func and the AICc-function.
    y_list[[k]] = rnorm(nrow(X_list[[k]]))

    X_list_dt[[k]] = data.table(X_list[[k]])
}



X_for_R = rbindlist(X_list_dt,idcol = T)
y_for_R = unlist(y_list)

AICc_R = AICc.func.new(h.vec = h,
                       y = y_for_R,
                       X = X_for_R,
                       negative = negative,
                       kernel = kernel,
                       scale_var = scale_var,
                       S_scale_dist = S_scale_dist,
                       idcol = T)

AICc_cpp = AICc_full_cpp(X_list = X_list,
                         mcov_list = mcov_list,
                         S_scale_dist = S_scale_dist,
                         h = h,
                         y_list = y_list,
                         negative = negative)

all.equal(AICc_R,AICc_cpp)

#### testing

aa=AICc_full_intermediate_cpp(X_list[[1]], mcov_list[[1]], S_scale_dist, h,  y_list[[1]])+AICc_full_intermediate_cpp(X_list[[2]], mcov_list[[2]], S_scale_dist, h,  y_list[[2]])

log(aa[1])+correction_term_trace_input_cpp(aa[2],aa[3])

log(0.6546109) + 1.2144005

AICc.func(h.vec = h,y = y_list[[2]],X = X_list[[2]],negative = negative,kernel = kernel,scale_var = scale_var,S_scale_dist = S_scale_dist)


################# OLD

##### NOw we continue with H_func.new which needs to build a block diagnoal matrix of the H-functions.

### testing

library(Matrix)

n = 40
H = bdiag(aa_new,aa_new)
y = rnorm(40)

sigma.hat.sq <- as.numeric(t(y)%*%t(diag(n)-H)%*%(diag(n)-H)%*%y)/n
as.numeric(t(y[1:20])%*%t(diag(20)-H[1:20,1:20])%*%(diag(20)-H[1:20,1:20])%*%y[1:20])/n+as.numeric(t(y[21:40])%*%t(diag(20)-H[21:40,21:40])%*%(diag(20)-H[21:40,21:40])%*%y[21:40])/n



aa_new = H_cpp(X,mcov = Sigma,S_scale_dist = S_scale_dist,h = 0.2)


#aa_new2 = H_new_cpp(X,mcov = Sigma,S_scale_dist = S_scale_dist,h = 0.2)

#all.equal(aa_new,aa_new2)





############

## OLD





# Gives the same, so can put the H matrices in a list, and just loop over the list elements when doing the various computations


# Now building example with two H's and X's

X1 = matrix(rnorm(20*3),ncol=3)

Sigma1 <- cov(X1)   #### SPEEDUP: May move this outside both the H.func and the AICc-function.

X2 = matrix(rnorm(30*3),ncol=3)

Sigma2 <- cov(X2)   #### SPEEDUP: May move this outside both the H.func and the AICc-function.

X.list = list(X1,X2)

Sigma.list = list(Sigma1,Sigma2)

sourceCpp("src/AICc.cpp")

test_old = H_cpp(X1,mcov = Sigma1,S_scale_dist = S_scale_dist,h = 0.2)


test_new = H_new_cpp(X.list,mcov = Sigma.list,S_scale_dist = S_scale_dist,h = 0.2)

all.equal(test_old,test_new)

# Så ideen er å loope over antall liste-elementer, kjøre H_cpp på alle dem og lagre resultatene i en
# std::vector<arma::mat>
#std::vector<arma::mat>




#cppFunction("arma::mat H_cpp_old(arma::mat x) {
##            std::normal_distribution<double> distribution(5.0,2.0);##
#
#            return(x*x.t()); }",
#            depends="RcppArmadillo")


cppFunction('arma::mat H_cpp(arma::mat X, arma::mat mcov, bool S_scale_dist, double h) {

            using namespace arma;

            // Define variables
            int nrows = X.n_rows;
            int m = X.n_cols;

            arma::mat cholDec;
            arma::mat mu;

            arma::mat out(nrows,nrows,arma::fill::zeros);

            // Declaring some private variables

            double acc;
            uint32_t icol, irow, ii;
            double S_scale;
            double rowsum;

            uint32_t d = m;
            vec tmp(d);
            cholDec = trimatl(chol(mcov).t());
            vec D = cholDec.diag();
            if(S_scale_dist){
            S_scale = 0.5/pow(m*h,2);
            } else {
            S_scale = 0.5/pow(h,2);
            }

            for (int j = 0; j < nrows; ++j) {
            mu = X.row(j);

            // For each of the "n" random vectors, forwardsolve the corresponding linear system.
            // Forwardsolve because Im using the lower triangle Cholesky.
            for(icol = 0; icol < nrows; icol++)
            {

            for(irow = 0; irow < d; irow++)
            {
            acc = 0.0;

            for(ii = 0; ii < irow; ii++) acc += tmp.at(ii) * cholDec.at(irow, ii);

            tmp.at(irow) = ( X.at(icol, irow) - mu.at(irow) - acc ) / D.at(irow);
            }

            out.at(icol,j) = sum(square(tmp));

            }

            }
            out *= S_scale;

            out = exp(-out);

            return out;
            }

            ',
            depends="RcppArmadillo")



H_cpp(X)



H_R = H_Rfunc(X = X,
              Sigma = Sigma,
              h = h)



H_Rfunc = function(X,Sigma,h,S_scale_dist = T){
    n <- nrow(X)

    H <- K.func.Mahalanobis.all(X=X,h.vec=h,Sigma=Sigma,S_scale_dist = S_scale_dist)
    H <- H/rowSums(H)

    return(H)
}

K.func.Mahalanobis.all <- function(X,h.vec,Sigma,S_scale_dist){
    exp(-gen_Mahlanobis_dist_cpp(featureList = list(1,1:ncol(X)),Xtrain = X,Xtest = X,mcov = Sigma,S_scale_dist = S_scale_dist)[,,2]/(2*h.vec^2))
}






############



K.func.Mahalanobis.all <- function(X,h.vec,Sigma,S_scale_dist){
    exp(-gen_Mahlanobis_dist_cpp(featureList = list(1,1:ncol(X)),Xtrain = X,Xtest = X,mcov = Sigma,S_scale_dist = S_scale_dist)[,,2]/(2*h.vec^2))
}




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



H = H.func(h.vec=h.vec,
           X = X,
           kernel = kernel,
           scale_var = scale_var,
           S_scale_dist = S_scale_dist)






dim(H)





arma::cube gen_Mahlanobis_dist_cpp(Rcpp::List featureList,arma::mat Xtrain, arma::mat Xtest, arma::mat mcov, bool S_scale_dist) {

    using namespace arma;

    // Define variables
    int ntrain = Xtrain.n_rows;
    int ntest = Xtest.n_rows;
    int m = Xtrain.n_cols;
    int p = featureList.size();

    arma::mat mcov0;
    arma::mat cholDec;
    arma::mat mu0;
    arma::mat mu;
    arma::mat X;

    arma::cube out(ntrain,ntest,p,arma::fill::zeros);

    // Declaring some private variables

    double acc;
    uint32_t icol, irow, ii;
    double S_scale;
    double rowsum;


    for (int k = 1; k < p; ++k){ // Ignoring the first List element (assuming it contains en empty vector)

        arma::uvec theseFeatures = featureList[k];
        theseFeatures = theseFeatures-1;

        mcov0 = mcov.submat(theseFeatures,theseFeatures);
        X = Xtrain.cols(theseFeatures);
        mu0 = Xtest.cols(theseFeatures);

        uint32_t d = X.n_cols;
        vec tmp(d);
        cholDec = trimatl(chol(mcov0).t());
        vec D = cholDec.diag();
        if(S_scale_dist){
            S_scale = 1.0/pow(theseFeatures.n_elem,2);
        } else {
            S_scale = 1.0;
        }

        for (int j = 0; j < ntest; ++j) {
            mu = mu0.row(j);

            // For each of the "n" random vectors, forwardsolve the corresponding linear system.
            // Forwardsolve because I'm using the lower triangle Cholesky.
            for(icol = 0; icol < ntrain; icol++)
            {

            for(irow = 0; irow < d; irow++)
            {
            acc = 0.0;

            for(ii = 0; ii < irow; ii++) acc += tmp.at(ii) * cholDec.at(irow, ii);

            tmp.at(irow) = ( X.at(icol, irow) - mu.at(irow) - acc ) / D.at(irow);
            }

            out.at(icol,j,k) = sum(square(tmp));
            }

        }
            out.slice(k) *= S_scale;
    }


            return out;
            }


