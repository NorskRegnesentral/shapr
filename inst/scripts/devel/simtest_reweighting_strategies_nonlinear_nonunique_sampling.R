### Upcoming generalization:

#1. Use non-linear truth (xgboost or so)
#2. Even more features


library(data.table)
library(MASS)
library(Matrix)
library(shapr)
library(future)
library(xgboost)

m <- 12
n_train <- 5000
n_explain <- 5
rho_1 <- 0.9
rho_2 <- 0.6
rho_3 <- 0.3
rho_4 <- 0.1
Sigma_1 <- matrix(rho_1, m/4, m/4) + diag(m/4) * (1 - rho_1)
Sigma_2 <- matrix(rho_2, m/4, m/4) + diag(m/4) * (1 - rho_2)
Sigma_3 <- matrix(rho_3, m/4, m/4) + diag(m/4) * (1 - rho_3)
Sigma_4 <- matrix(rho_4, m/4, m/4) + diag(m/4) * (1 - rho_4)

Sigma <- as.matrix(bdiag(Sigma_1, Sigma_2, Sigma_3, Sigma_4))
mu <- rep(0,m)

set.seed(123)


x_train <- as.data.table(MASS::mvrnorm(n_train,mu,Sigma))
x_explain <- as.data.table(MASS::mvrnorm(n_explain,mu,Sigma))

names(x_train) <- paste0("VV",1:m)
names(x_explain) <- paste0("VV",1:m)


g <- function(a,b){
  a*b+a*b^2+a^2*b
}

beta <- c(0.2, -0.8, 1.0, 0.5, -0.8, rep(0, m - 5))
gamma <- c(0.8,-1)
alpha <- 1
y_train <- alpha +
  as.vector(as.matrix(cos(x_train))%*%beta) +
  unlist(gamma[1]*g(x_train[,1],x_train[,2])) +
  unlist(gamma[1]*g(x_train[,3],x_train[,4])) +
  rnorm(n_train, 0, 1)
y_explain <- alpha +
  as.vector(as.matrix(cos(x_explain))%*%beta) +
  unlist(gamma[1]*g(x_explain[,1],x_explain[,2])) +
  unlist(gamma[1]*g(x_explain[,3],x_explain[,4])) +
  rnorm(n_train, 0, 1)

xy_train <- cbind(y_train, x_train)

set.seed(123)
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 50,
  verbose = FALSE
)

p0 <- mean(y_train)


### First run proper shapr call on this

kernelSHAP_reweighting_strategy = "none"

set.seed(465132)
progressr::handlers(global = TRUE)
expl <- shapr::explain(model = model,
                       x_explain= x_explain,
                       x_train = x_train,
                       approach = "gaussian",
                       n_batches=100,n_samples = 10000,
                       prediction_zero = p0,Sigma=Sigma,mu=mu)

dt_vS_map <- merge(expl$internal$iter_list[[1]]$coalition_map,expl$internal$output$dt_vS,by="id_coalition")[,-"id_coalition"]


kernelSHAP_reweighting_strategy_vec <- c("none","on_N","on_coal_size","on_all","on_all_cond","on_all_cond_paired","comb")

n_coalitions_vec <- c(50,100,200,400,800,1200,1600,2000,2400,2800,3200,3600,4000)

reps <- 200

paired_shap_sampling_vec <- c(FALSE,TRUE)

res_list <- weight_list <- list()

for(ii in seq_along(n_coalitions_vec)){

  for(i0 in seq_along(paired_shap_sampling_vec)){

    for(i in seq_len(reps)){

      this_seed <- 10000+1+i
      this_n_coalitions <- n_coalitions_vec[ii]
      this_paired_shap_sampling <- paired_shap_sampling_vec[i0]

      this <- shapr::explain(model = model,
                             x_explain= x_explain,
                             x_train = x_train,
                             approach = "gaussian",
                             n_samples = 10, # Never used
                             n_batches=10,
                             prediction_zero = p0,
                             Sigma=Sigma,
                             mu=mu,
                             seed = this_seed,
                             max_n_coalitions = this_n_coalitions,
                             kernelSHAP_reweighting = "none",
                             unique_sampling = TRUE,
                             paired_shap_sampling = this_paired_shap_sampling)

      this0_X <- this$internal$objects$X


      exact_dt_vS <- merge(this$internal$iter_list[[1]]$coalition_map,dt_vS_map,by="coalitions_str")
      setorder(exact_dt_vS,id_coalition)


      for(iii in seq_along(kernelSHAP_reweighting_strategy_vec)){
        this_kernelSHAP_reweighting_strategy <- kernelSHAP_reweighting_strategy_vec[iii]

        this_X <- copy(this0_X)

        shapr:::kernelSHAP_reweighting(this_X,reweight=this_kernelSHAP_reweighting_strategy)

        this_W <- weight_matrix(
          X = this_X,
          normalize_W_weights = TRUE
        )

        shap_dt0 <- as.data.table(cbind(seq_len(n_explain),t(this_W%*%as.matrix(exact_dt_vS[,-c("coalitions_str","id_coalition")]))))
        names(shap_dt0) <- names(this$shapley_values_est)

        this_diff <- unlist(shap_dt0[,-c(1,2)]-expl$shapley_values_est[,-c(1,2)])
        this_bias <- mean(this_diff)
        this_var <- var(this_diff)
        this_MAE <- mean(abs(this_diff))
        this_RMSE <- sqrt(mean(this_diff^2))

        res_vec <- data.table(n_coalitions = this_n_coalitions,
                              paired_shap_sampling = this_paired_shap_sampling,
                              kernelSHAP_reweighting_strategy = this_kernelSHAP_reweighting_strategy,
                              seed = this_seed,
                              bias=this_bias,
                              var = this_var,
                              MAE = this_MAE,
                              RMSE = this_RMSE)

        res_list[[length(res_list)+1]] <- copy(res_vec)

        #        weight_dt <- unique(this_X[,.(coalition_size,shapley_weight)][,shapley_weight:=mean(shapley_weight),by=coalition_size][])
        weight_dt <- this_X[,.(coalition_size,shapley_weight)][,head(.SD,1),by=coalition_size]

        weight_dt[,n_coalitions:=this_n_coalitions]
        weight_dt[,paired_shap_sampling:=this_paired_shap_sampling]
        weight_dt[,kernelSHAP_reweighting_strategy:=this_kernelSHAP_reweighting_strategy]
        weight_dt[,seed:=this_seed]

        weight_list[[length(weight_list)+1]] <- copy(weight_dt)


      }

    }

    print(i)

  }

  print(n_coalitions_vec[ii])
}


res_dt <- rbindlist(res_list)

fwrite(res_dt,file = "../../Div/extra_shapr_scripts_etc/res_dt_reweighting_sims_nonlingaus_nonunique_sampling_new.csv")

resres <- res_dt[,lapply(.SD,mean),.SDcols=c("bias","var","MAE","RMSE"),by=.(paired_shap_sampling,n_coalitions,kernelSHAP_reweighting_strategy)]
resres_sd <- res_dt[,lapply(.SD,sd),.SDcols=c("bias","var","MAE","RMSE"),by=.(paired_shap_sampling,n_coalitions,kernelSHAP_reweighting_strategy)]


library(ggplot2)

ggplot(resres,aes(x=n_coalitions,y=MAE,col=kernelSHAP_reweighting_strategy,linetype= paired_shap_sampling))+
         geom_line()

ggplot(resres[paired_shap_sampling==FALSE],aes(x=n_coalitions,y=MAE,col=kernelSHAP_reweighting_strategy,linetype= paired_shap_sampling))+
  geom_line()+scale_y_log10()

ggplot(resres[paired_shap_sampling==TRUE],aes(x=n_coalitions,y=MAE,col=kernelSHAP_reweighting_strategy,linetype= paired_shap_sampling))+
  geom_line()+scale_y_log10()




weight_dt <- rbindlist(weight_list)


weight_dt[!(coalition_size%in%c(0,12)),sum_shapley_weight:=sum(shapley_weight),by=.(seed,paired_shap_sampling,n_coalitions,kernelSHAP_reweighting_strategy)]

weight_dt[!(coalition_size%in%c(0,12)),shapley_weight:=shapley_weight/sum_shapley_weight]
weight_dt[!(coalition_size%in%c(0,12)),mean(shapley_weight),by=.(seed,paired_shap_sampling,n_coalitions,kernelSHAP_reweighting_strategy)]


ww_dt <- weight_dt[!(coalition_size%in%c(0,12)),list(mean_weight=mean(shapley_weight)),by=.(coalition_size,paired_shap_sampling,n_coalitions,kernelSHAP_reweighting_strategy)]

ggplot(ww_dt[paired_shap_sampling==TRUE & kernelSHAP_reweighting_strategy %in% c("none","on_all_cond_paired","on_N")],aes(x=coalition_size,y=mean_weight,col=kernelSHAP_reweighting_strategy))+
  geom_point()+facet_grid(~n_coalitions)
