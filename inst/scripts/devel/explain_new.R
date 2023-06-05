library(xgboost)
library(shapr)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

# Looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Prepare the data for explanation
explainer <- shapr(x_train,model)
explainer2 <- shapr(x_train,model,is_python=T)
explainer3 <- shapr(x_train,is_python=T)


# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)

##### TESTS ####

explanation_new <- explain_new(
  x_test,
  approach = "gaussian",
  explainer = explainer1,
  prediction_zero = p,
  n_samples = 5*10^5,n_batches = 1
)

explanation_new$dt_shapley
#none    lstat         rm        dis     indus
#1: 22.446 5.190027 -0.9981141  0.4190562 4.2444812
#2: 22.446 1.828362 -1.3269640 -0.2576771 0.5622163
#3: 22.446 5.447883  4.6641813 -0.1288418 0.6862445
#4: 22.446 5.617564  2.5234393  0.6614170 2.1817247
#5: 22.446 1.715925  5.0150390  0.8001951 1.7526796
#6: 22.446 2.592836 -2.6699632  0.6478134 1.8333372

explanation_new <- explain_new(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p,
  n_samples = 10^5,n_batches = 4
)

explanation_new$dt_shapley
#none    lstat         rm        dis     indus
#1: 22.446 5.194753 -0.9882765  0.4020399 4.2469342
#2: 22.446 1.809298 -1.3121239 -0.2562250 0.5649886
#3: 22.446 5.447172  4.6691463 -0.1240268 0.6771758
#4: 22.446 5.626358  2.5125083  0.6672642 2.1780147
#5: 22.446 1.712585  5.0095470  0.8175396 1.7441671
#6: 22.446 2.590425 -2.6672009  0.6596539 1.8211454

explanation_new <- explain_new(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p,
  n_samples = 10^5,n_batches = 1
)

explanation_new$dt_shapley
#none     lstat         rm       dis      indus
#1: 22.446 5.2632030 -1.2526613 0.2920444  4.5528644
#2: 22.446 0.1671901 -0.7088401 0.9689005  0.3786871
#3: 22.446 5.9888022  5.5450858 0.5660134 -1.4304351
#4: 22.446 8.2142204  0.7507572 0.1893366  1.8298304
#5: 22.446 0.5059898  5.6875103 0.8432238  2.2471150
#6: 22.446 1.9929673 -3.6001958 0.8601984  3.1510531

explanation_new <- explain_new(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p,
  n_samples = 10^5,n_batches = 4
)

explanation_new$dt_shapley
#none     lstat         rm       dis      indus
#1: 22.446 5.2632030 -1.2526613 0.2920444  4.5528644
#2: 22.446 0.1671901 -0.7088401 0.9689005  0.3786871
#3: 22.446 5.9888022  5.5450858 0.5660134 -1.4304351
#4: 22.446 8.2142204  0.7507572 0.1893366  1.8298304
#5: 22.446 0.5059898  5.6875103 0.8432238  2.2471150
#6: 22.446 1.9929673 -3.6001958 0.8601984  3.1510531

#### TESTS ENDS #####

#
# print(explanation$dt)
#
# setup <- explain_setup(
#   x_test,
#   approach = "gaussian",
#   explainer = explainer,
#   prediction_zero = p
# )
#
# str(explainer,max.level = 1)
# str(setup,max.level=1)
#
explainer <- explain_setup(
   x_test,
   approach = "empirical",
   explainer = explainer,
   prediction_zero = p,
   n_batches = 4
 )

explainer0 <- explain_setup(
  x_test,
  approach = c("empirical","copula","ctree","gaussian"),
  explainer = explainer,
  prediction_zero = p,
  n_batches = 7
)

explainer0$X

#
#
# dt <- future.apply::future_lapply(X = explainer$S_batch,
#                                   FUN = batch_prepare_vS,
#                                   explainer = explainer,
#                                   future.seed = explainer$seed)
# dt <- batch_prepare_vS(explainer$S_batch[[4]],explainer)
#
#
# explanation_new <- explain_new(
#   x_test,
#   approach = "gaussian",
#   explainer = explainer,
#   prediction_zero = p,
#   n_samples = 10^5
# )



prepare_data(explainer, index_features = explainer$S_batch[[1]])
# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$dt)

# Finally we plot the resulting explanations
plot(explanation)
