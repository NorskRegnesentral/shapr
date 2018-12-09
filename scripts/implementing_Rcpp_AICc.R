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
n <- 5
p = 2
K = 5
kernel="Mahalanobis"
scale_var=F
S_scale_dist = T
negative = F

X_list <- list()
mcov_list <- list()
y_list <- list()

X_list_dt <- list()
for (k in 1:K){
    X_list[[k]] = matrix(rnorm((n+1*k)*p),ncol=p)
    mcov_list[[k]] <- cov(X_list[[k]])   #### SPEEDUP: May move this outside both the H.func and the AICc-function.
    y_list[[k]] = rowSums(X_list[[k]])

    X_list_dt[[k]] = data.table(X_list[[k]])
}



X_for_R = rbindlist(X_list_dt,idcol = T)
y_for_R = unlist(y_list)

tt=proc.time()
AICc_R = AICc.func.new(h.vec = h,
                       y = y_for_R,
                       X = X_for_R,
                       negative = negative,
                       kernel = kernel,
                       scale_var = scale_var,
                       S_scale_dist = S_scale_dist,
                       idcol = T)

print(proc.time()-tt)
#28.624   9.869  38.516

tt=proc.time()
AICc_cpp = AICc_full_cpp(h = h,
                         X_list = X_list,
                         mcov_list = mcov_list,
                         S_scale_dist = S_scale_dist,
                         y_list = y_list,
                         negative = negative)
print(proc.time()-tt)
#12.265   0.517  11.049


all.equal(AICc_R,AICc_cpp)

## temp test

AICc_cpp = AICc_full_cpp_alt(h = h,
                         X_list = X_list,
                         mcov_list = mcov_list,
                         S_scale_dist = S_scale_dist,
                         y_list = y_list,
                         negative = negative)



######## Just doing a test with nlminb as well



tt=proc.time()

nlm.obj_R <- nlminb(start = 0.1,
                    objective = AICc.func.new,
                    y = y_for_R,
                    X = X_for_R,
                    kernel=kernel,
                    scale_var=scale_var,
                    S_scale_dist = S_scale_dist,
                    negative = F,
                    idcol = T,
                    lower = 0,
                    control=list(eval.max=20,trace=1))
print(proc.time()-tt)
#4.378   0.665   5.042

tt=proc.time()

nlm.obj_cpp <- nlminb(start = 0.1,
                    objective = AICc_full_cpp,
                    X_list = X_list,
                    mcov_list = mcov_list,
                    S_scale_dist = S_scale_dist,
                    y_list = y_list,
                    negative = F,
                    lower = 0,
                    control=list(eval.max=20,trace=1))
print(proc.time()-tt)
#1.699   0.001   0.504


all.equal(nlm.obj_R,nlm.obj_cpp)
#[1] TRUE
