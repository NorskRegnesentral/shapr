

#### PAPER EXPERIMENT FRAMEWORK ####
#### Use the current setup for all experiements in the paper to ease reproducablity etc.

#### Example 6 ####
# Random Forest with Gaussian mixture distributed features.

rm(list = ls())

library(shapr)
library(data.table)
library(mvtnorm)
library(condMVNorm)

source("paper_scripts/paper_helper_funcs.R")

mu.list = list(c(0,0,0),c(10,-5,10))
mu.list = list(c(0,0,0))

Sigma.sd <- 0.5

Sigma.list <- list(matrix(c(1,0.7,0.7,
                            0.7,1,0.7,
                            0.7,0.7,1),ncol=3),
                   matrix(c(1,0.7,0.7,
                            0.7,1,0.7,
                            0.7,0.7,1),ncol=3))
Sigma.list <- list(diag(Sigma.sd*c(1,2,4))%*%matrix(c(1,0.7,0.5,
                                                      0.7,1,0.3,
                                                      0.5,0.3,1),ncol=3)%*%diag(Sigma.sd*c(1,2,4)),
                   diag(Sigma.sd*c(1,2,4))%*%matrix(c(1,0.7,0.5,
                                                      0.7,1,0.3,
                                                      0.5,0.3,1),ncol=3)%*%diag(Sigma.sd*c(1,2,4)))

Sigma.list <- list(diag(Sigma.sd*c(1,2,4))%*%matrix(c(1,0.7,0.5,
                                                      0.7,1,0.3,
                                                      0.5,0.3,1),ncol=3)%*%diag(Sigma.sd*c(1,2,4)))

pi.G <- c(0.5,0.5)
pi.G <- 1

sd = 0.1

nTrainMod <- 50000 # Currently not used
nTrain <- 500
nTest <- 1000


#### Defining the true distribution of the variables and the model------

samp_variables <- function(n,pi.G,mu.list,Sigma.list){

    X <- joint.samp.func(n = n,
                         pi.G,
                         mu.list,
                         Sigma.list)
    return(X)
}

samp_model <- function(n,X,sd){
    y <- 0.5*X[,2]  +  (X[,1]<0)*1 + (X[,2]<2) + (X[,2]>4)*1 + (X[,3]<10)*1 + (X[,3]<0)*1 + (X[,1]>-5)*(X[,2]<4)*1+ rnorm(n = n,mean=0,sd=sd)
    return(y)
}




#### Sampling train and test data ---------

set.seed(123)
XYtrain <- data.table(samp_variables(n = nTrain,
                                     pi.G = pi.G,
                                     mu.list = mu.list,
                                     Sigma.list = Sigma.list))
XYtrain[,y:=samp_model(.N,.SD,sd=sd)]
Xtrain <- copy(XYtrain)
Xtrain[,y:=NULL]

XYtest <- data.table(samp_variables(n = nTest,
                                    pi.G = pi.G,
                                    mu.list = mu.list,
                                    Sigma.list = Sigma.list))
XYtest[,y:=samp_model(.N,.SD,sd=sd)]
Xtest <- copy(XYtest)
Xtest[,y:=NULL]

#### Fitting the model ----------

XYtrainMod <- data.table(samp_variables(n = nTrainMod,
                                        pi.G = pi.G,
                                        mu.list = mu.list,
                                        Sigma.list = Sigma.list))
XYtrainMod[,y:=samp_model(.N,.SD,sd=sd)]


model = ranger::ranger(
    formula = y~.,
    data = XYtrain,
    num.trees = 50,
    num.threads = 3,
    verbose = TRUE,
    importance = "impurity",
    mtry=2)



pred_zero = XYtrain[, mean(y)]
m = ncol(Xtrain)


#### Pre computation before kernel shap --------------------------------
l <- prepare_kernelShap(
    m = m,
    Xtrain = Xtrain,
    Xtest = Xtest,
    exact = TRUE,
    nrows = 1e4
)

#### Finding which h is actually the best


w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3


# Not really needed anymore, as we use Shapley.true instead
# Shapley.Gauss = compute_kernelShap(model = model,
#                                    l,
#                                    sigma = 1, # Ignored when Gaussian==T
#                                    w_threshold = w_threshold,
#                                    n_threshold = n_threshold,
#                                    verbose = FALSE,
#                                    cond_approach = "Gaussian",
#                                    pred_zero=pred_zero,
#                                    kernel_metric = "Gaussian")

h.val <- seq(0.1,1,0.1)
#h.val <- seq(0.05,0.5,0.025)
#h.val <- seq(0.1,3,0.2)

Shapley.approx.test <- list()
for (i in 1:length(h.val)){

    Shapley.approx.test[[i]] = compute_kernelShap(model = model,
                                                  l,
                                                  sigma = h.val[i],
                                                  w_threshold = w_threshold,
                                                  n_threshold = n_threshold,
                                                  verbose = FALSE,
                                                  cond_approach = "empirical",
                                                  pred_zero=pred_zero,
                                                  kernel_metric = "Gaussian")
    print(i)

}

#DT.gaussian <- rbindlist(Shapley.Gauss$other_object$ll)
#setnames(DT.gaussian,"k","p.gaussian")


Shapley.true = Shapley_true(model = model,
                            Xtrain = Xtrain,
                            Xtest = Xtest,
                            pi.G = pi.G,
                            mu.list = mu.list,
                            Sigma.list = Sigma.list,
                            int.samp=200,
                            l,
                            pred_zero = pred_zero)

DT.true <- data.table(wcomb = rep(1:2^m,nrow(Shapley.true$trueValue.mat)),
                      p.true = (c(t(Shapley.true$trueValue.mat))),
                      id = rep(1:nrow(Shapley.true$trueValue.mat),each=2^m))
DT.true


DT.list <- list()
for (i in 1:length(h.val)){

    DT.approx <- rbindlist(Shapley.approx.test[[i]]$other_object$ll)
    setnames(DT.approx,"k","p.approx")
    DT.approx[,h:=h.val[i]]

    DT.list[[i]] <- merge(DT.approx,DT.true,by=c("wcomb","id"),all=T)
}
DT <- rbindlist(DT.list)



helper <- copy(l$X)
helper[,wcomb:=ID]
helper[,ID:= NULL]
helper[,features:= NULL]
helper[,N:= NULL]
helper[,weight:= NULL]

DT <- merge(DT,helper,by="wcomb")
DT[,diff:=p.approx-p.true]
DT[,absdiff:=abs(diff)]

print(DT[,mean(absdiff)]) # Mean
DT.summary <- DT[,.(mean=mean(absdiff),sd=sd(absdiff)),by=.(nfeatures,h)] # Summary per nfeatures
DT.summary[nfeatures %in% c(1,2),]
DT.summary[,mean(mean),by=h]




#### Optimum is h=0.20 for nfeatures=1, and h=0.35 for nfeatures=2 with correlation 0.7.
#### Optimum is h=1.05 for nfeatures=1, and h=1.45 for nfeatures=2 with independence.




############# DOING THE AICc-stuff ####

source("scripts/AICc_helper_functions.R")

# Computing for the first test observation only

no.testobs <- 1

# Same optimum is found for every test observation, so no need to do this for more than one test observation.

h.optim.mat <- matrix(NA,nrow=nrow(l$S),ncol=no.testobs)

### If we use Mahalanobis, we might be able to adjust for different correlation between the variables, such that we at least need to do it only once
### per conditioning size (or a few and taking the mean of them) If we are also able to generalize the distance measure to take into account how the optimal bandwidth changes from one size to another,
### we may also get away doing it just once (or a few differnet sets which we take the mean of)


for (j in 1:no.testobs){
    x.star <- Xtest[j,]
    for (i in 2:(nrow(l$S)-1)){ # Finding optimal h for each submodel, except the zero and full model
        S <- l$S[i,]


        S.cols <- paste0("X",which(as.logical(S)))
        Sbar.cols <- paste0("X",which(as.logical(1-S)))

        Xtrain.S <- subset(Xtrain,select=S.cols)
        Xtrain.Sbar <- subset(Xtrain,select=Sbar.cols)
        x.star.S <- subset(x.star,select=S.cols)

        X.pred <- cbind(Xtrain.Sbar,x.star.S)
        X.nms <- colnames(Xtrain)
        setcolorder(X.pred,X.nms)

        pred <- pred_vector(model=model,data=X.pred)

        nlm.obj <- nlminb(start = 1,objective = AICc.func,y = pred,X = as.matrix(Xtrain.S),kernel="Mahalanobis",scale_var=T,lower = 0,control=list(eval.max=20,trace=1))
        h.optim.mat[i,j] <- nlm.obj$par
        # May also use mlrMBO here, something like this maybe: https://mlr-org.github.io/Stepwise-Bayesian-Optimization-with-mlrMBO/, just not stepwise. See other tutorial.
        #    exp(-l$D[,1,i]/2*h)
        print(c(i,j))
    }
}


h.optim.mat

mean(h.optim.mat[2:4])
mean(h.optim.mat[5:7])

DT.summary[(nfeatures %in% c(1,2)),.SD[which.min(mean)] ,by=.(nfeatures)]

aa=DT[,.(mean=mean(absdiff),sd=sd(absdiff)),by=.(nfeatures,h,wcomb)] # Summary per nfeatures

aa[nfeatures%in%c(1,2),.SD[which.min(mean)] ,by=.(wcomb)]
aa[nfeatures%in%c(1,2),.SD,by=.(wcomb)]


#### Results summary #######

# Small samples... not very good, hoping for better results with larger samples
# > h.optim.mat
# [,1]
# [1,]        NA
# [2,] 0.1363913
# [3,] 0.4191277
# [4,] 0.5681838
# [5,] 0.2876686
# [6,] 0.2198464
# [7,] 0.3953498
# [8,]        NA
# >
#     > mean(h.optim.mat[2:4])
# [1] 0.3745676
# > mean(h.optim.mat[5:7])
# [1] 0.3009549
# >
#     > DT.summary[(nfeatures %in% c(1,2)),.SD[which.min(mean)] ,by=.(nfeatures)]
# nfeatures    h       mean         sd
# 1:         1 0.90 0.09398633 0.10674768
# 2:         2 0.65 0.11335388 0.08534083
# >
#     > aa[nfeatures%in%c(1,2),.SD[which.min(mean)] ,by=.(wcomb)]
# wcomb nfeatures    h       mean         sd
# 1:     2         1 0.90 0.15589602 0.14645408
# 2:     3         1 0.80 0.04081149 0.03243543
# 3:     4         1 0.75 0.06049458 0.05317806
# 4:     5         2 0.50 0.08913799 0.05155440
# 5:     6         2 0.70 0.12320015 0.10090477
# 6:     7         2 0.50 0.05703817 0.04924770




# Gaussian mixture thing...
# > h.optim.mat
# [,1]      [,2]      [,3]
# [1,]        NA        NA        NA
# [2,] 0.1334794 0.1334807 0.1354830
# [3,] 0.7747388 0.7939538 0.7567104
# [4,] 0.5790176 0.5742336 0.5684501
# [5,] 0.2847000 0.2867043 0.2848648
# [6,] 0.2169593 0.2162662 0.2167736
# [7,] 0.4183283 0.4113734 0.4201247
# [8,]        NA        NA        NA
# >
#     > mean(h.optim.mat[2:4])
# [1] 0.4957453
# > mean(h.optim.mat[5:7])
# [1] 0.3066625
# >
#     > DT.summary[(nfeatures %in% c(1,2)),.SD[which.min(mean)] ,by=.(nfeatures)]
# nfeatures   h      mean         sd
# 1:         1 0.9 0.1150671 0.11291594
# 2:         2 0.7 0.1245504 0.08785356
# >
#     > aa[nfeatures%in%c(1,2),.SD[which.min(mean)] ,by=.(wcomb)]
# wcomb nfeatures   h       mean         sd
# 1:     2         1 0.9 0.16425531 0.15255834
# 2:     3         1 0.4 0.06083943 0.04150726
# 3:     4         1 0.7 0.07570232 0.09731841
# 4:     5         2 0.3 0.06960619 0.04648308
# 5:     6         2 0.7 0.12033373 0.10007079
# 6:     7         2 0.5 0.08347371 0.06007682
# > aa[nfeatures%in%c(1,2),.SD,by=.(wcomb)]
# wcomb nfeatures   h       mean         sd
# 1:     2         1 0.1 0.94244079 0.73865890
# 2:     2         1 0.2 0.85407885 0.62177014
# 3:     2         1 0.3 0.74746710 0.50950755
# 4:     2         1 0.4 0.63788842 0.40906784
# 5:     2         1 0.5 0.52605648 0.31934632
# 6:     2         1 0.6 0.41096250 0.24034545
# 7:     2         1 0.7 0.29700405 0.17927067
# 8:     2         1 0.8 0.20113445 0.14827701
# 9:     2         1 0.9 0.16425531 0.15255834
# 10:     2         1 1.0 0.21833648 0.16671412
# 11:     3         1 0.1 0.11877443 0.10951482
# 12:     3         1 0.2 0.08670909 0.06937421
# 13:     3         1 0.3 0.06789556 0.04998520
# 14:     3         1 0.4 0.06083943 0.04150726
# 15:     3         1 0.5 0.06140439 0.03600728
# 16:     3         1 0.6 0.06419571 0.03413107
# 17:     3         1 0.7 0.06798135 0.03575691
# 18:     3         1 0.8 0.07266303 0.03933221
# 19:     3         1 0.9 0.07796555 0.04375244
# 20:     3         1 1.0 0.08361600 0.04830380
# 21:     4         1 0.1 0.27682325 0.24364525
# 22:     4         1 0.2 0.19227675 0.18578742
# 23:     4         1 0.3 0.15588334 0.16797121
# 24:     4         1 0.4 0.12346720 0.15199556
# 25:     4         1 0.5 0.09559621 0.13200410
# 26:     4         1 0.6 0.07825433 0.11176168
# 27:     4         1 0.7 0.07570232 0.09731841
# 28:     4         1 0.8 0.08575691 0.09246351
# 29:     4         1 0.9 0.10298044 0.09560959
# 30:     4         1 1.0 0.12330944 0.10267649
# 31:     5         2 0.1 0.13082991 0.11188064
# 32:     5         2 0.2 0.07367605 0.04962167
# 33:     5         2 0.3 0.06960619 0.04648308
# 34:     5         2 0.4 0.07978274 0.05225723
# 35:     5         2 0.5 0.09755967 0.05866896
# 36:     5         2 0.6 0.12694852 0.07062145
# 37:     5         2 0.7 0.16559619 0.08557361
# 38:     5         2 0.8 0.20640228 0.09951437
# 39:     5         2 0.9 0.24398307 0.11158018
# 40:     5         2 1.0 0.27622599 0.12204754
# 41:     6         2 0.1 1.01742725 0.83699286
# 42:     6         2 0.2 0.79828978 0.59424561
# 43:     6         2 0.3 0.62583953 0.40918162
# 44:     6         2 0.4 0.47158462 0.27342648
# 45:     6         2 0.5 0.32190187 0.17824042
# 46:     6         2 0.6 0.18857854 0.12152201
# 47:     6         2 0.7 0.12033373 0.10007079
# 48:     6         2 0.8 0.15036892 0.11862890
# 49:     6         2 0.9 0.22009883 0.14037981
# 50:     6         2 1.0 0.28500785 0.15870437
# 51:     7         2 0.1 0.13871874 0.14851687
# 52:     7         2 0.2 0.10933819 0.10564298
# 53:     7         2 0.3 0.09792011 0.08487756
# 54:     7         2 0.4 0.08874187 0.07030524
# 55:     7         2 0.5 0.08347371 0.06007682
# 56:     7         2 0.6 0.08385922 0.05357733
# 57:     7         2 0.7 0.08772127 0.05264224
# 58:     7         2 0.8 0.09416762 0.05472050
# 59:     7         2 0.9 0.10139454 0.05865095
# 60:     7         2 1.0 0.10859682 0.06301266
# wcomb nfeatures   h       mean         sd


#### WIth regular GAussian data, quite OK
#> h.optim.mat
# [,1]
# [1,]        NA
# [2,] 0.3399994
# [3,] 0.4066318
# [4,] 0.5393687
# [5,] 0.5751572
# [6,] 0.4265953
# [7,] 0.4848465
# [8,]        NA
# >
#     > mean(h.optim.mat[2:4])
# [1] 0.4286666
# > mean(h.optim.mat[5:7])
# [1] 0.495533
# >
#     > DT.summary[(nfeatures %in% c(1,2)),.SD[which.min(mean)] ,by=.(nfeatures)]
# nfeatures   h       mean        sd
# 1:         1 0.3 0.08061748 0.1143813
# 2:         2 0.3 0.09022641 0.1473156
# >
#     > aa[nfeatures%in%c(1,2),.SD[which.min(mean)] ,by=.(wcomb)]
# wcomb nfeatures   h       mean         sd
# 1:     2         1 0.3 0.10917300 0.11791689
# 2:     3         1 0.3 0.03826528 0.03991521
# 3:     4         1 0.5 0.07740759 0.09851135
# 4:     5         2 0.3 0.05290981 0.04640481
# 5:     6         2 0.3 0.18275541 0.22058167
# 6:     7         2 0.3 0.03501400 0.03638534















################ end results summary ################
## REgular Euclidean
#[,1]      [,2]      [,3]
#[1,]        NA        NA        NA
#[2,] 0.2872571 0.2872571 0.2872571
#[3,] 0.2658588 0.2658588 0.2658588
#[4,] 0.2609530 0.2609530 0.2609530
#[5,] 0.4034179 0.4034179 0.4034179
#[6,] 0.4494405 0.4494405 0.4494405
#[7,] 0.3923674 0.3923674 0.3923674
#[8,]        NA        NA        NA

### Varying variance for the differnet variables gives strange results, but just a simple scaling to all, works fine.... Why?


########## THIS IS INCORRECT ####################
#
# no.testobs <- 1
#
# h.optim.mat <- matrix(NA,nrow=nrow(l$S),ncol=no.testobs)
#
# for (j in 1:no.testobs){
#     x.star <- Xtest[j,]
#     for (i in 2:(nrow(l$S)-1)){ # Finding optimal h for each submodel, except the zero and full model
#         S <- l$S[i,]
#
#         S.cols <- paste0("X",which(as.logical(S)))
#         Sbar.cols <- paste0("X",which(as.logical(1-S)))
#
#         Xtrain.Sbar <- subset(Xtrain,select=Sbar.cols)
#         x.star.S <- subset(x.star,select=S.cols)
#
# #        pred <- cbind(Xtrain.Sbar,x.star.S)
#         pred <- copy(XYtrain)
#         X.nms <- colnames(Xtrain)
#         setcolorder(pred,X.nms)
#
#         pred[, p_hat := pred_vector(model = model, data = .SD), .SDcols = X.nms]
#
#          #  h.val <- seq(0.02,2,0.04)
#          # # h.val <- c(0.1,0.123753,0.120703,0.120074,0.120093,0.115)
#          # #
#          #  AICc.vec <- rep(NA,length(h.val))
#          #  for (j in 1:length(h.val)){
#          #      AICc.vec[j] <- AICc.func(h.vec = h.val[j],y = pred$p_hat,X = as.matrix(subset(pred,select = X.nms)))
#          #      print(c(h.val[j],AICc.vec[j]))
#          #  }
#
#         nlm.obj <- nlminb(start = 1,objective = AICc.func,y = pred$p_hat,X = as.matrix(subset(pred,select = X.nms)),lower = 0,control=list(eval.max=20,trace=1))
#         h.optim.mat[i,j] <- nlm.obj$par
#         # May also use mlrMBO here, something like this maybe: https://mlr-org.github.io/Stepwise-Bayesian-Optimization-with-mlrMBO/, just not stepwise. See other tutorial.
#         #    exp(-l$D[,1,i]/2*h)
#         print(c(i,j))
#     }
# }
#
#
# h.optim.mat
############### END INCORRECT ######################

#### Computing the various Shapley approximations --------

w_threshold = 1 # For a fairer comparison, all models use the same number of samples (n_threshold)
n_threshold = 10^3

Shapley.approx = list()

Shapley.approx$sigma.0.005 = compute_kernelShap(model = model,
                                                l,
                                                sigma = 0.01,
                                                w_threshold = w_threshold,
                                                n_threshold = n_threshold,
                                                verbose = FALSE,
                                                cond_approach = "empirical",
                                                pred_zero=pred_zero)


Shapley.approx$sigma.01 = compute_kernelShap(model = model,
                                             l,
                                             sigma = 0.1,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             verbose = FALSE,
                                             cond_approach = "empirical",
                                             pred_zero=pred_zero)

Shapley.approx$sigma.03 = compute_kernelShap(model = model,
                                             l,
                                             sigma = 0.3,
                                             w_threshold = w_threshold,
                                             n_threshold = n_threshold,
                                             verbose = FALSE,
                                             cond_approach = "empirical",
                                             pred_zero=pred_zero)

Shapley.approx$indep = compute_kernelShap(model = model,
                                          l,
                                          sigma = 0.1, # sigma==0 gives the special case of independence (NOTE: NOT the same as setting sigma= 10^10)
                                          w_threshold = w_threshold,
                                          n_threshold = n_threshold,
                                          verbose = FALSE,
                                          cond_approach = "empirical",
                                          pred_zero=pred_zero,
                                          kernel_metric = "independence")

Shapley.approx$Gauss = compute_kernelShap(model = model,
                                          l,
                                          sigma = 0, # Ignored when Gaussian==T
                                          w_threshold = w_threshold,
                                          n_threshold = n_threshold,
                                          verbose = FALSE,
                                          cond_approach = "Gaussian",
                                          pred_zero=pred_zero)

Shapley.true = Shapley_true(model = model,
                            Xtrain = Xtrain,
                            Xtest = Xtest,
                            pi.G = pi.G,
                            mu.list = mu.list,
                            Sigma.list = Sigma.list,
                            int.samp=200,
                            l,
                            pred_zero = pred_zero)

#### Comparing the true and approximate values -------------

# Mean absolute errors per variable (to see if the performance differ between variables)
(absmeans.sigma.01 = colMeans(abs(Shapley.true[,-1]-Shapley.approx$sigma.01$Kshap[,-1])))
(absmeans.sigma.03 = colMeans(abs(Shapley.true[,-1]-Shapley.approx$sigma.03$Kshap[,-1])))
(absmeans.indep = colMeans(abs(Shapley.true[,-1]-Shapley.approx$indep$Kshap[,-1])))
(absmeans.Gauss = colMeans(abs(Shapley.true[,-1]-Shapley.approx$Gauss$Kshap[,-1])))

# Mean of the absolute errors over all variables
res_to_paper <- c(S_KS=mean(absmeans.indep),G_KS = mean(absmeans.Gauss),E_KS_0.1=mean(absmeans.sigma.01),E_KS_0.3=mean(absmeans.sigma.03))
res_to_paper
#S_KS       G_KS   E_KS_0.1   E_KS_0.3
#0.32338379 0.01657350 0.02612238 0.03026733

# Insert ranking based measures etc. here as well.



DT.approx <- rbindlist(Shapley.approx$sigma.0.005$other_object$ll)
DT.gaussian <- rbindlist(Shapley.approx$Gauss$other_object$ll)
DT <- merge(DT.approx,DT.gaussian,by=c("wcomb","id"),all=T)
setnames(DT,"k.x","p.approx")
setnames(DT,"k.y","p.gaussian")

helper <- copy(l$X)
helper[,wcomb:=ID]
helper[,ID:= NULL]
helper[,features:= NULL]
helper[,N:= NULL]
helper[,weight:= NULL]

DT <- merge(DT,helper,by="wcomb")
DT[,diff:=p.approx-p.gaussian]
DT[,absdiff:=abs(diff)]

print(sigma)
print(n_threshold)
print(DT[,mean(absdiff)]) # Mean
DT.summary <- DT[,.(mean=mean(absdiff),sd=sd(absdiff)),by=nfeatures] # Summary per nfeatures
DT.summary[,sigma:=0.1]
DT.summary[,K:=n_threshold]

DT.summary.list[[listnum]] <- copy(DT.summary)



