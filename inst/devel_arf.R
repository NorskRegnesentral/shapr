library(xgboost)
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


e_arf <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "arf",
  phi0 = p0
)

e_emp <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "empirical",
  phi0 = p0
)

e_ctree <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  phi0 = p0
)

e_arf$shapley_values_est

e_ctree$shapley_values_est

e_arf$timing$total_time_secs
e_emp$timing$total_time_secs
e_ctree$timing$total_time_secs


e_arf$MSEv$MSEv
e_emp$MSEv$MSEv
e_ctree$MSEv$MSEv









#
# # Computing the Shapley values with kernelSHAP accounting for feature dependence using
# # the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
# explanation <- explain(
#   model = model,
#   x_explain = x_explain,
#   x_train = x_train,
#   approach = "empirical",
#   phi0 = p0
# )
#
# ### TESINT ARF ###
#
# #install.packages("arf")
# library(arf)
#
# head(x_train)
#
# x_train[,Month_fact:=as.factor(Month)]
#
# future::plan("multisession", workers = 2) # Increase the number of workers for increased performance with many features
#
# arf_xtrain <- arf::adversarial_rf(x_train, mtry = 4)
#
# params_arf <- arf::forde(arf_xtrain, x_train)
#
# evi <- data.frame(Solar.R = c(190), Temp = c(67))
#
# synth2 <- forge(params_arf, n_synth = 1000, evidence = evi)
#
