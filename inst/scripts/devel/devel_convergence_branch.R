library(xgboost)
#library(shapr)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]
data[,new1 :=sqrt(Wind*Ozone)]
data[,new2 :=sqrt(Wind*Temp)]
data[,new3 :=sqrt(Wind*Day)]
data[,new4 :=sqrt(Wind*Solar.R)]
data[,new5 :=rnorm(.N)]
data[,new6 :=rnorm(.N)]
data[,new7 :=rnorm(.N)]


x_var <- c("Solar.R", "Wind", "Temp", "Month","Day","new1","new2","new3","new4","new5")#"new6","new7")
y_var <- "Ozone"

ind_x_explain <- 1:20
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
explanation_adaptive <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  max_n_coalitions = 500,
  prediction_zero = p0,
  adaptive = TRUE,
  print_shapleyres = TRUE, # tmp
  print_iter_info = TRUE, # tmp
  shapley_reweighting = "on_N"
)

explanation_adaptive <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  n_coalitions = 500,
  prediction_zero = p0,
  adaptive = TRUE,
  print_shapleyres = TRUE, # tmp
  print_iter_info = TRUE, # tmp
  shapley_reweighting = "on_N"
)


explanation_nonadaptive <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  n_coalitions = 400,
  prediction_zero = p0,
  adaptive = FALSE
)


explanation_adaptive <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  n_coalitions = 500,
  prediction_zero = p0,
  adaptive = TRUE,
  adaptive_args = list(initial_n_coalitions=10,convergence_tolerance=0.0001),
  print_shapleyres = TRUE, # tmp
  print_iter_info = TRUE, # tmp
  shapley_reweighting = "on_N"
)










plot(explanation_adaptive$internal$output$iter_objects$dt_iter_convergence_res$n_current_samples,
     explanation_adaptive$internal$output$iter_objects$dt_iter_shapley_sd[explain_id==1,Solar.R],type="l")
sd_full <- explanation_adaptive$internal$output$iter_objects$dt_iter_shapley_sd[explain_id==1][.N,Solar.R]
n_samples_full <- explanation_adaptive$internal$output$iter_objects$dt_iter_convergence_res[.N,n_current_samples]
sd_full0 <- sd_full*sqrt(n_samples_full)
lines(explanation_adaptive$internal$output$iter_objects$dt_iter_convergence_res$n_current_samples,
      sd_full0/sqrt(explanation_adaptive$internal$output$iter_objects$dt_iter_convergence_res$n_current_samples),type="l",col=2)


plot(explanation_adaptive$internal$output$iter_objects$dt_iter_convergence_res$n_current_samples,
     explanation_adaptive$internal$output$iter_objects$dt_iter_convergence_res$estimated_required_samples,type="l",ylim=c(0,4000),lwd=4)
for(i in 1:20){
  lines(explanation_adaptive$internal$output$iter_objects$dt_iter_convergence_res$n_current_samples,
        explanation_adaptive$internal$output$iter_objects$dt_iter_convergence_res[[5+i]],type="l",col=1+i)
}


plot(explanation_adaptive$internal$output$iter_objects$dt_iter_convergence_res$n_current_samples,
     explanation_adaptive$internal$output$iter_objects$dt_iter_shapley_sd[explain_id==1,Solar.R],type="l",ylim=c(0,2))
sd_full <- explanation_adaptive$internal$output$iter_objects$dt_iter_shapley_sd[explain_id==1][.N,Solar.R]
n_samples_full <- explanation_adaptive$internal$output$iter_objects$dt_iter_convergence_res[.N,n_current_samples]
sd_full0 <- sd_full*sqrt(n_samples_full)
lines(explanation_adaptive$internal$output$iter_objects$dt_iter_convergence_res$n_current_samples,
      sd_full0/sqrt(explanation_adaptive$internal$output$iter_objects$dt_iter_convergence_res$n_current_samples),type="l",col=2,lwd=3)

for(i in 1:20){
  lines(explanation_adaptive$internal$output$iter_objects$dt_iter_convergence_res$n_current_samples,
       explanation_adaptive$internal$output$iter_objects$dt_iter_shapley_sd[explain_id==i,Solar.R],type="l",col=1+i)
}



lines(explanation_adaptive$internal$output$dt_iter_convergence_res$n_current_samples,
      sd_full0/sqrt(explanation_adaptive$internal$output$dt_iter_convergence_res$n_current_samples),type="l",col=2)


plot(explanation_adaptive$internal$output$dt_iter_convergence_res$estimated_required_samples)

explanation_regular <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  n_coalitions = NULL,
  prediction_zero = p0,
  adaptive = FALSE
)

