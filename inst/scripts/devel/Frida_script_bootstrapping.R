# Install shapr from github (do NOT use the CRAN version)
#remotes::install_github("NorskRegnesentral/shapr")

library(data.table)

source("inst/scripts/devel/iterative_kernelshap_sourcefuncs.R")

frida_dt <- data.table::fread("../../Div/shapr_permute_extra_files/til_Martin.csv")
frida_dt[,V1:=NULL]
unique(frida_dt[-1][1:32,])

n_kershap_samps_vec <- seq(32,nrow(frida_dt),by=32)

samp_cov_estimator <- function(frida_dt0,testObs_computed,n_kershap_samps = 32,n_reps = 100,m,shapley_reweighting_strategy = "none",return = "sd_mat",
                               paired_sampling = FALSE,
                               feature_names = NULL,
                               seed = 123){

  set.seed(seed)

  varest_list <- list()

  for (i in seq_len(n_reps)){

    frida_dt_reshuffled <- frida_dt0[-c(1,.N)][sample(.N),]

    feature_sample_1 <- apply(frida_dt_reshuffled[,-5],1,function(x) as.vector(which(x==1)))[seq(1,n_kershap_samps/2)]
    feature_sample_2 <- apply(frida_dt_reshuffled[,-5],1,function(x) as.vector(which(x==1)))[seq(n_kershap_samps/2+1,n_kershap_samps)]

    nonfeature_sample_1 <- apply(frida_dt_reshuffled[,-5],1,function(x) as.vector(which(x==0)))[seq(1,n_kershap_samps/2)]
    nonfeature_sample_2 <- apply(frida_dt_reshuffled[,-5],1,function(x) as.vector(which(x==0)))[seq(n_kershap_samps/2+1,n_kershap_samps)]



    dt_vS0 <- frida_dt_reshuffled[seq(n_kershap_samps)]
    dt_vS0[seq(1,n_kershap_samps/2),S_char:=sapply(feature_sample_1, paste0, collapse = "_")]
    dt_vS0[seq(n_kershap_samps/2+1,n_kershap_samps),S_char:=sapply(feature_sample_2, paste0, collapse = "_")]

    dt_vS0[seq(1,n_kershap_samps/2),Sbar_char:=sapply(nonfeature_sample_1, paste0, collapse = "_")]
    dt_vS0[seq(n_kershap_samps/2+1,n_kershap_samps),Sbar_char:=sapply(nonfeature_sample_2, paste0, collapse = "_")]

    dt_vS00 <- frida_dt0[c(1,.N)]
    dt_vS00[1,S_char:=""]
    dt_vS00[1,Sbar_char:="1_2_3_4"]

    dt_vS00[2,S_char:="1_2_3_4"]
    dt_vS00[2,Sbar_char:=""]


    dt_vS <- rbind(dt_vS00,dt_vS0)
    setorder(dt_vS) # Sorts the dt_vS in correct order

    dt_vS <- dt_vS[,-(1:4)]

    dt_vS <- unique(dt_vS)

    feature_sample_all <- c(feature_sample_1,feature_sample_2)
    halfway <- length(feature_sample_1)


    if(paired_sampling == TRUE){ # Assuming the second half is the paired copy of the first half of the feature samples
      these_ids0 <- sample(seq_len(halfway),replace = TRUE)
      these_ids <- c(these_ids0,these_ids0+halfway)

    } else {
      these_ids <- seq_len(halfway*2)#sample(seq_len(halfway*2),replace = TRUE)
    }

    X_tmp0 <- X_from_feature_set_v3(feature_sample_all[these_ids],m=m,sample_ids=these_ids)[] # sample_ids could be removed from the function -- never used

    X_tmp <- shapley_reweighting(X_tmp0,strategy = shapley_reweighting_strategy)

    # Get weighted matrix ----------------
    X_tmp[,features:=S] # Since shapr:::weight_matrix needs this name
    W_tmp <- shapr:::weight_matrix(
      X = X_tmp,
      normalize_W_weights = TRUE,
      is_groupwise = FALSE
    )

    dt_vS_relevant <- dt_vS[X_tmp[,.(S_char)],, on = "S_char"] # This merges and orderes the dt_vS by the order of the features in X_tmp

    kshap <- t(W_tmp %*% as.matrix(dt_vS_relevant[, -c("S_char","Sbar_char")]))
    dt_kshap <- data.table::as.data.table(kshap)
    colnames(dt_kshap) <- c("none", feature_names)
    dt_kshap[,id:=testObs_computed]

    varest_list[[i]] <- copy(dt_kshap)

  }

  varest_list_dt <- rbindlist(varest_list)


  varest_full_list <- list()
  for(k in seq_along(testObs_computed)){
    varest_full_list[[k]] <- varest_list_dt[id==testObs_computed[k],cov(.SD),.SDcols = -"id"]
  }

  if(return == "sd_mat"){
    ret <- t(sapply(varest_full_list,function(x) sqrt(diag(x))))
    ret_dt <- as.data.table(ret)
    ret_dt[,id:=testObs_computed]
    if(paired_sampling){
      ret_dt[,sd_type:="truth"]
    } else {
      ret_dt[,sd_type:="truth"]
    }
    setcolorder(ret_dt,c("sd_type","id"))
    return(ret_dt)
  } else {
    return(varest_full_list)
  }
}

samp_cov_estimator_new <- function(n_kershap_samps = 32,n_reps = 100,m,shapley_reweighting_strategy = "none",return = "sd_mat",
                               paired_sampling = FALSE,
                               feature_names = NULL,
                               seed = 123){

  set.seed(seed)

  varest_list <- list()

  for (i in seq_len(n_reps)){

    X_tmp0 <- shapr:::feature_not_exact(m,n_kershap_samps,unique_sampling = FALSE)
    X_tmp0[,p:=NULL]
    X_tmp0[,S_char:=sapply(features, paste0, collapse = "_")]

    X_tmp <- shapley_reweighting(X_tmp0,strategy = shapley_reweighting_strategy)

    # Get weighted matrix ----------------
    #X_tmp[,features:=S] # Since shapr:::weight_matrix needs this name
    W_tmp <- shapr:::weight_matrix(
      X = X_tmp,
      normalize_W_weights = TRUE,
      is_groupwise = FALSE
    )

    dt_vS_relevant <- dt_vS_all[X_tmp[,.(S_char)],, on = "S_char"] # This merges and orderes the dt_vS by the order of the features in X_tmp

    kshap <- t(W_tmp %*% as.matrix(dt_vS_relevant[, -c("S_char","Sbar_char")]))
    dt_kshap <- data.table::as.data.table(kshap)
    colnames(dt_kshap) <- c("none", feature_names)
    dt_kshap[,id:=testObs_computed]

    varest_list[[i]] <- copy(dt_kshap)

  }

  varest_list_dt <- rbindlist(varest_list)


  varest_full_list <- list()
  for(k in seq_along(testObs_computed)){
    varest_full_list[[k]] <- varest_list_dt[id==testObs_computed[k],cov(.SD),.SDcols = -"id"]
  }

  if(return == "sd_mat"){
    ret <- t(sapply(varest_full_list,function(x) sqrt(diag(x))))
    ret_dt <- as.data.table(ret)
    ret_dt[,id:=testObs_computed]
    if(paired_sampling){
      ret_dt[,sd_type:="truth"]
    } else {
      ret_dt[,sd_type:="truth"]
    }
    setcolorder(ret_dt,c("sd_type","id"))
    return(ret_dt)
  } else {
    return(varest_full_list)
  }
}

kshap_estimator <- function(feature_sample_1,feature_sample_2,dt_vS,paired_sampling = FALSE,m,
                            shapley_reweighting_strategy,feature_names,testObs_computed){

  feature_sample_all <- c(feature_sample_1,feature_sample_2)
  halfway <- length(feature_sample_1)

  if(paired_sampling == TRUE){ # Assuming the second half is the paired copy of the first half of the feature samples
    these_ids0 <- sample(seq_len(halfway),replace = TRUE)
    these_ids <- c(these_ids0,these_ids0+halfway)

  } else {
    these_ids <- seq_len(halfway*2)#sample(seq_len(halfway*2),replace = TRUE)
  }

  X_tmp0 <- X_from_feature_set_v3(feature_sample_all[these_ids],m=m,sample_ids=these_ids)[] # sample_ids could be removed from the function -- never used

  X_tmp <- shapley_reweighting(X_tmp0,strategy = shapley_reweighting_strategy)

  # Get weighted matrix ----------------
  X_tmp[,features:=S] # Since shapr:::weight_matrix needs this name
  W_tmp <- shapr:::weight_matrix(
    X = X_tmp,
    normalize_W_weights = TRUE,
    is_groupwise = FALSE
  )

  dt_vS_relevant <- dt_vS[X_tmp[,.(S_char)],, on = "S_char"] # This merges and orderes the dt_vS by the order of the features in X_tmp

  kshap <- t(W_tmp %*% as.matrix(dt_vS_relevant[, -c("S_char","Sbar_char")]))
  dt_kshap <- data.table::as.data.table(kshap)
  colnames(dt_kshap) <- c("none", feature_names)
  dt_kshap[,id:=testObs_computed]

  return(dt_kshap[])
}

dt_vS_all <- unique(frida_dt)
feature_sample_all <- apply(dt_vS_all[,-5],1,function(x) as.vector(which(x==1)))
nonfeature_sample_all <- apply(dt_vS_all[,-5],1,function(x) as.vector(which(x==0)))

dt_vS_all[,S_char:=sapply(feature_sample_all, paste0, collapse = "_")]
dt_vS_all[,Sbar_char:=sapply(nonfeature_sample_all, paste0, collapse = "_")]

dt_vS_all <- dt_vS_all[,.(S_char,Sbar_char,vS)]


n_boot_ests <- 1000
current_m <- 4
shapley_reweighting_strategy <- "none"
paired_sampling <- FALSE
current_feature_names <- names(frida_dt)[1:4]
testObs_computed <- 1

kshap_list <- boot_list <- truevar_list <- truevar_list2 <- list()

for (j in seq_along(n_kershap_samps_vec)){
  n_kershap_samps <- n_kershap_samps_vec[j]#32 # must be a multiple of 2

  feature_sample_1 <- apply(frida_dt[,-5],1,function(x) as.vector(which(x==1)))[-1][seq(1,n_kershap_samps/2)]
  feature_sample_2 <- apply(frida_dt[,-5],1,function(x) as.vector(which(x==1)))[-1][seq(n_kershap_samps/2+1,n_kershap_samps)]

  nonfeature_sample_1 <- apply(frida_dt[,-5],1,function(x) as.vector(which(x==0)))[-1][seq(1,n_kershap_samps/2)]
  nonfeature_sample_2 <- apply(frida_dt[,-5],1,function(x) as.vector(which(x==0)))[-1][seq(n_kershap_samps/2+1,n_kershap_samps)]



  dt_vS0 <- frida_dt[1+seq(n_kershap_samps)]
  dt_vS0[seq(1,n_kershap_samps/2),S_char:=sapply(feature_sample_1, paste0, collapse = "_")]
  dt_vS0[seq(n_kershap_samps/2+1,n_kershap_samps),S_char:=sapply(feature_sample_2, paste0, collapse = "_")]

  dt_vS0[seq(1,n_kershap_samps/2),Sbar_char:=sapply(nonfeature_sample_1, paste0, collapse = "_")]
  dt_vS0[seq(n_kershap_samps/2+1,n_kershap_samps),Sbar_char:=sapply(nonfeature_sample_2, paste0, collapse = "_")]

  dt_vS00 <- frida_dt[c(1,.N)]
  dt_vS00[1,S_char:=""]
  dt_vS00[1,Sbar_char:="1_2_3_4"]

  dt_vS00[2,S_char:="1_2_3_4"]
  dt_vS00[2,Sbar_char:=""]


  dt_vS <- rbind(dt_vS00,dt_vS0)
  setorder(dt_vS) # Sorts the dt_vS in correct order

  dt_vS <- dt_vS[,-(1:4)]

  dt_vS <- unique(dt_vS)

  kshap_dt <- kshap_estimator(feature_sample_1,feature_sample_2,dt_vS=dt_vS,paired_sampling = FALSE,m=current_m,
                              shapley_reweighting_strategy = shapley_reweighting_strategy,feature_names = current_feature_names,
                              testObs_computed = testObs_computed)


  kshap_sd_dt <- boot_cov_estimator(feature_sample_1,feature_sample_2,dt_vS=dt_vS,testObs_computed = testObs_computed,n_boot_ests = n_boot_ests,m=current_m,
                                    shapley_reweighting_strategy = shapley_reweighting_strategy,return="sd_mat",paired_sampling = paired_sampling,
                                    feature_names = current_feature_names,seed=1245)



  frida_dt0 <- copy(frida_dt)


  #varest <- samp_cov_estimator(frida_dt0,n_kershap_samps = n_kershap_samps,testObs_computed = testObs_computed,m=current_m,
  #                             feature_names = current_feature_names,n_reps = n_boot_ests)

  varest2=samp_cov_estimator_new(n_kershap_samps = n_kershap_samps,m=current_m,
                                 feature_names = current_feature_names,n_reps = n_boot_ests)


  kshap_list[[j]] <- copy(kshap_dt)
  boot_list[[j]] <- copy(kshap_sd_dt)
  #truevar_list[[j]] <- copy(varest)
  truevar_list2[[j]] <- copy(varest2)

  print(j)
  print(kshap_dt[])
  print(kshap_sd_dt[])
  #print(varest[])
  print(varest2[])

}


boot_dt <- rbindlist(boot_list)
truevar_dt <- rbindlist(truevar_list2)

plot(n_kershap_samps_vec,boot_dt[,Solar.R],type="l")
lines(n_kershap_samps_vec,truevar_dt[,Solar.R],col=2)

plot(n_kershap_samps_vec,boot_dt[,Solar.R]/truevar_dt[,Solar.R],type="l")






kshap_sd_dt_4 <- copy(kshap_sd_dt)
kshap_sd_dt_3 <- copy(kshap_sd_dt)
kshap_sd_dt_2 <- copy(kshap_sd_dt)
kshap_sd_dt_1 <- copy(kshap_sd_dt)


kshap <- rbindlist(list(kshap_sd_dt_4,kshap_sd_dt_3,kshap_sd_dt_2,kshap_sd_dt_1),idcol = "n_boot_ests")
kshap[n_boot_ests==1,n_boot_ests:=10]
kshap[n_boot_ests==2,n_boot_ests:=100]
kshap[n_boot_ests==3,n_boot_ests:=1000]
kshap[n_boot_ests==4,n_boot_ests:=5000]

fwrite(kshap,"../../Div/shapr_permute_extra_files/kshap_boot_sd_dt.csv")

aa <- fread("../../Div/shapr_permute_extra_files/kshap_boot_sd_dt.csv")

m <- 4
n_combinations_sample <- 32

feature_sample_new_list <- feature_set_sample(feature_sample_prev = NULL, m = m,n_combinations_sample = n_combinations_sample,
                                              unique_sampling = FALSE, paired_sampling = FALSE)














library(shapr)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:6
x_train <- data[-ind_x_explain, ..x_var]
y_train <- data[-ind_x_explain, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]

# Looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Specifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "independence", # "ctree", "empirical", "vaeac", "gaussian" etc are other dependence-aware options here (some require additional packages)
  prediction_zero = p0
)

explanation$shapley_values

# Manually producing the same output from objects created by explain()
W <- explanation$internal$objects$W
dt_vS <- explanation$internal$output$dt_vS

kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
dt_kshap <- data.table::as.data.table(kshap)
colnames(dt_kshap) <- c("none", names(x_explain))

identical(dt_kshap,explanation$shapley_values)
# TRUE

# Unused objects which provide additonal information
S <- explanation$internal$objects$S # Indicator matrix showing which features are included in each of the subsets
X <- explanation$internal$objects$X # The same, but in table format with additional information

# To check that you get the same results with you specific v(S)-matrix/vector, replace
# as.matrix(dt_vS[, -"id_combination"])
# with a matrix of the v(S) estimates from your kernelshap implementation (one column per observation to explain)

