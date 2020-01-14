#### Prediction with categorical input ####

rm(list=ls())

library(xgboost)
library(shapr)
library(data.table)
library(party)
##
library(microbenchmark)
library(caret)
##

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus", "rad")
x_var_org <- c("lstat", "black", "indus", "ptratio", "tax", "rad", "age", "dis", "rm", "nox")
# x_var <- c("lstat","black","indus")#"ptratio")#,"tax","rad")#,"age","dis","rm","nox")

y_var <- "medv"

Boston$rad = as.factor(Boston$rad)
Boston$chas = as.factor(Boston$chas)

loop_no_var <- c(3, 5, 6, 7, 9, 10)

timing_list <- list()

for (i in 1:length(loop_no_var)){

  x_var <- x_var_org[1:loop_no_var[i]]

  x_train <- Boston[-(1:6), x_var]
  y_train <- Boston[-(1:6), y_var]
  x_test <- Boston[1:6, x_var]


  ### SPECIAL STUFF  STARTS ###
  dummyfunc <- caret::dummyVars(" ~ .", data = rbind(x_train, x_test))
  x_train_dummy <- predict(dummyfunc, newdata = x_train)
  x_test_dummy <- predict(dummyfunc, newdata = x_test)
  ### SPECIAL STUFF ENDS ###


  # Fitting a basic xgboost model to the training data
  model <- xgboost(
    data = x_train_dummy,
    label = y_train,
    nround = 20,
    verbose = FALSE
  )

  ### SPEXIAL STUFF STARTS ###
  model$dummyfunc <- dummyfunc
  ### SPECIAL STUFF ENDS ###

  explainer <- shapr(x_train, model)

  p <- mean(y_train)

  explanation_list <- list()

  timing <- microbenchmark(explanation_list[[1]] <- explain(
    x_test,
    approach = 'ctree',
    explainer = explainer,
    prediction_zero = p,
    sample = FALSE),
    explanation_list[[2]]  <- explain(
      x_test,
      approach = 'ctree',
      explainer = explainer,
      prediction_zero = p,
      sample = FALSE,
      mc_cores = 2),
    explanation_list[[3]]  <- explain(
      x_test,
      approach = 'ctree',
      explainer = explainer,
      prediction_zero = p,
      sample = FALSE,
      mc_cores = 4),
    explanation_list[[4]]  <- explain(
      x_test,
      approach = 'ctree',
      explainer = explainer,
      prediction_zero = p,
      sample = FALSE,
      mc_cores = 8),
    explanation_list[[5]]  <- explain(
      x_test,
      approach = 'ctree',
      explainer = explainer,
      prediction_zero = p,
      sample = FALSE,
      mc_cores_simulateAllTrees = 1,
      mc_cores_sample_ctree = 2),
    explanation_list[[6]]  <- explain(
      x_test,
      approach = 'ctree',
      explainer = explainer,
      prediction_zero = p,
      sample = FALSE,
      mc_cores_simulateAllTrees = 1,
      mc_cores_sample_ctree = 4),
    explanation_list[[7]]  <- explain(
      x_test,
      approach = 'ctree',
      explainer = explainer,
      prediction_zero = p,
      sample = FALSE,
      mc_cores_simulateAllTrees = 1,
      mc_cores_sample_ctree = 8),
    explanation_list[[8]]  <- explain(
      x_test,
      approach = 'ctree',
      explainer = explainer,
      prediction_zero = p,
      sample = FALSE,
      mc_cores_simulateAllTrees = 2,
      mc_cores_sample_ctree = 1),
    explanation_list[[9]]  <- explain(
      x_test,
      approach = 'ctree',
      explainer = explainer,
      prediction_zero = p,
      sample = FALSE,
      mc_cores_simulateAllTrees = 4,
      mc_cores_sample_ctree = 1),
    explanation_list[[10]]  <- explain(
      x_test,
      approach = 'ctree',
      explainer = explainer,
      prediction_zero = p,
      sample = FALSE,
      mc_cores_simulateAllTrees = 8,
      mc_cores_sample_ctree = 1),
    times = 100)


  # Checking that all are equal
  print(length(unique(explanation_list)))

  print(timing)

  timing_list[[i]] <- timing
}

saveRDS(timing_list,"inst/devel_scripts/parallelization_results_Annabelle_100.rds")

# For n_features > 6, one should do paralellization of both parameters
# For n_features = 6, is eems that doing only for the sample_tree function is the best
# For n_featues < 6, one is better off not doing any paralellization

# Printing the Shapley values for the test data
print(explanation_list[[1]]$dt)

# Finally we plot the resulting explanations
plot(explanation_list[[1]])

## Annabelle results ##
c(3, 5, 6, 7, 9, 10)
## Re-done with 100 runs of each method
## for 3 features
## No cores

## for 5 features
## No cores

## for 6 features
## No cores

## for 7 features
## No cores

## for 9 features
## mc_cores = 8 i.e explanation_list[[4]]

## for 10 features - run out of memory
## took 23 hours!
