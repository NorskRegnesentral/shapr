library(data.table)
library(mvnfast)
library(shapr)

### Defining sampling data and model
dimX <- 5
rho <- 0.5
nTrain <- 500
nTest <- 10

mu <- 1:dimX
Sigma <- matrix(rho,ncol=dimX,nrow=dimX)
diag(sigma) <- 1:dimX


samp_variables <- function(n,mu,sigma){
    data.table(rmvnorm(n = n,
                       mean = mu,
                       sigma = sigma))

}

samp_model <- function(n,x,sd_noise){
    y <- rowSums(x) + rnorm(n = n,mean=0,sd=sd_noise)
}

fit_model_func <- function(XYtrain){
    lm(y~.,data=XYtrain)
}


sd_noise <- 0.5

#### Sampling train and test data

set.seed(123)

XYtrain <- data.table(samp_variables(n = n_train,
                                     mu = mu,
                                     sigma = sigma))

XYtrain[,y:=samp_model(.N,.SD,sd_noise=sd_noise)]
Xtrain <- copy(XYtrain)
Xtrain[,y:=NULL]

# Just features for testing
Xtest <- data.table(samp_variables(n = n_test,
                                   mu = mu,
                                   sigma = sigma))


pred_zero = XYtrain[, mean(y)] # Storing the mean prediction

#### Fitting the model

model <- fit_model_func(XYtrain)


#### Preparing the data for kernelShap

l <- prepare_kshap(
    xtrain = xtrain,
    xtest = xtest)

#### Running a few different versions of kernelShap


Shapley.approx = list()

# Empirical version with sigma set to the default value of 0.1
Shapley.approx$empirical_sigma.01 = compute_kshap(model = model,
                                                       l = l,
                                                       pred_zero=pred_zero)

# Gaussian approach
Shapley.approx$Gaussian = compute_kshap(model = model,
                                             l = l,
                                             cond_approach = "Gaussian",
                                             pred_zero=pred_zero)

# Combined Gaussian and empirical with sigma=0.1
Shapley.approx$comb = compute_kshap(model = model,
                                         l = l,
                                         cond_approach = list(empirical=1:5, Gaussian=6:32),
                                         pred_zero=pred_zero)

# Classical kernelShap assuming independence
Shapley.approx$empirical_independence = compute_kshap(model = model,
                                                      l = l,
                                                      empirical_settings = list(type = "independence",
                                                                                w_threshold = 1),
                                                      pred_zero=pred_zero)


### Just looking at some of the results

head(Shapley.approx$empirical_sigma.01$kshap)

head(Shapley.approx$Gaussian$kshap)

head(Shapley.approx$comb$kshap)

head(Shapley.approx$empirical_independence$kshap)


#> head(Shapley.approx$empirical_sigma.01$kshap)
#[,1]        [,2]       [,3]       [,4]       [,5]        [,6]
#[1,] 15.12185 -0.33101596 -0.1995941 -3.1716767 -1.3690171  5.96095813
#[2,] 15.12185  0.07543789  1.4629576 -0.4498481  1.4533601  0.01711505
#[3,] 15.12185  2.43188435 -1.3705746 -3.4355415  2.2320755 -1.13005862
#[4,] 15.12185  2.12029443  0.1879183  3.3202952  1.0641689 -0.95529675
#[5,] 15.12185  1.22095162 -1.5424712  3.5383849  0.4585373 -4.70531011
#[6,] 15.12185 -1.05841893 -2.9451814 -5.2363138  0.2268107  1.81790819
#>
#    > head(Shapley.approx$Gaussian$kshap)
#[,1]        [,2]       [,3]       [,4]        [,5]       [,6]
#[1,] 15.12185 -0.26997464 -0.5612531 -2.8961716 -1.57141431  6.1884677
#[2,] 15.12185  0.08823734  1.7546374 -0.4306098  1.30675389 -0.1599963
#[3,] 15.12185  2.00064522 -0.8493324 -3.6332692  1.81000702 -0.6002655
#[4,] 15.12186  2.44128000  0.3700763  3.0810003  0.62228148 -0.7772580
#[5,] 15.12185  1.06791081 -0.9292292  3.5612818  0.14014064 -4.8700116
#[6,] 15.12185 -1.17506733 -3.0383310 -4.6305757  0.08754156  1.5612371
#>
#   > head(Shapley.approx$comb$kshap)
#[,1]         [,2]       [,3]       [,4]        [,5]       [,6]
#[1,] 15.12185 -0.365222151 -0.4642992 -2.9715479 -1.51400585  6.2047293
#[2,] 15.12185 -0.004273317  1.7791250 -0.3399272  1.23111803 -0.1070200
#[3,] 15.12185  1.941954689 -0.9865211 -3.3323124  1.82950650 -0.7248426
#[4,] 15.12186  2.372320720  0.1726334  3.2917833  0.63998113 -0.7393384
#[5,] 15.12185  0.969900702 -0.9433975  3.6876274  0.08742696 -4.8314652
#[6,] 15.12185 -1.097507445 -3.4018879 -4.9092677  0.50676064  1.7067072
#
# > head(Shapley.approx$empirical_independence$kshap)
# [,1]       [,2]       [,3]       [,4]       [,5]        [,6]
# [1,] 15.12185 -0.2132539 -0.4355152 -2.4175282 -1.3926871  5.34863846
# [2,] 15.12185  0.1598323  1.2829112 -0.1663144  1.2493053  0.03328819
# [3,] 15.12185  0.9630519 -0.6036891 -2.7352989  1.6173171 -0.51359578
# [4,] 15.12185  1.6306047  0.6584501  2.8261303  0.9675633 -0.34536831
# [5,] 15.12185  0.6452431 -0.4857762  2.8128117  0.2193151 -4.22150128
# [6,] 15.12185 -1.0010532 -2.6095189 -4.0659459 -0.4317870  0.91310953

