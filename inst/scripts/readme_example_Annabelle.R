library(xgboost)
library(shapr)
library(data.table)
library(party)
library(ggplot2)

data("Boston", package = "MASS")

# x_var <- c("lstat", "rm", "age", "tax", "dis", "indus", "crim", "nox", "ptratio", "black")
x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(Boston[-(1), x_var])
y_train <- Boston[-(1), y_var]
x_test <- as.matrix(Boston[1, x_var])

# Looking at the dependence between the features
# cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Prepare the data for explanation
explainer <- shapr(x_train, model)

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)

## --------------------------

explanation <- explain(
  x_test,
  explainer = explainer,
  approach = "ctree",
  prediction_zero = p,
  sample = FALSE,
  mincriterion = 0.95,
  seed = 1
)

explanation2 <- explain(
  x_test,
  explainer = explainer,
  approach = "ctree",
  prediction_zero = p,
  sample = FALSE,
  mincriterion = rep(0.95, 4),
  seed = 1
)

# Printing the Shapley values for the test data
print(explanation$dt)
print(explanation2$dt)

##
print(explanation$dt2)
print(explanation2$dt2)

##
explanation$dt2[, .(.N), by = .(wcomb)]
explanation2$dt2[, .(.N), by = .(wcomb)]


##
plot(explanation)
plot(explanation2)


## --------------- more tests -----------
N <- 100
old_ctree <- matrix(NA, nrow = N, ncol = 5)
old_ctree_diff_seeds <- matrix(NA, nrow = N, ncol = 5)

new_ctree <- matrix(NA, nrow = N, ncol = 5)
new_ctree_diff_seeds <- matrix(NA, nrow = N, ncol = 5)

for(i in 1:N){
  explanation <- explain(
    x_test,
    explainer = explainer,
    approach = "ctree",
    prediction_zero = p,
    sample = FALSE,
    mincriterion = 0.95,
    seed = 1)

  old_ctree[i,1] <- explanation$dt[[1]]
  old_ctree[i,2] <- explanation$dt[[2]]
  old_ctree[i,3] <- explanation$dt[[3]]
  old_ctree[i,4] <- explanation$dt[[4]]
  old_ctree[i,5] <- explanation$dt[[5]]


  explanation <- explain(
    x_test,
    explainer = explainer,
    approach = "ctree",
    prediction_zero = p,
    sample = FALSE,
    mincriterion = 0.95,
    seed = i)

  old_ctree_diff_seeds[i,1] <- explanation$dt[[1]]
  old_ctree_diff_seeds[i,2] <- explanation$dt[[2]]
  old_ctree_diff_seeds[i,3] <- explanation$dt[[3]]
  old_ctree_diff_seeds[i,4] <- explanation$dt[[4]]
  old_ctree_diff_seeds[i,5] <- explanation$dt[[5]]

  explanation <- explain(
    x_test,
    explainer = explainer,
    approach = "ctree",
    prediction_zero = p,
    sample = FALSE,
    mincriterion = rep(0.95, 4),
    seed = 1)

  new_ctree[i,1] <- explanation$dt[[1]]
  new_ctree[i,2] <- explanation$dt[[2]]
  new_ctree[i,3] <- explanation$dt[[3]]
  new_ctree[i,4] <- explanation$dt[[4]]
  new_ctree[i,5] <- explanation$dt[[5]]


  explanation <- explain(
    x_test,
    explainer = explainer,
    approach = "ctree",
    prediction_zero = p,
    sample = FALSE,
    mincriterion = rep(0.95, 4),
    seed = i)

  new_ctree_diff_seeds[i,1] <- explanation$dt[[1]]
  new_ctree_diff_seeds[i,2] <- explanation$dt[[2]]
  new_ctree_diff_seeds[i,3] <- explanation$dt[[3]]
  new_ctree_diff_seeds[i,4] <- explanation$dt[[4]]
  new_ctree_diff_seeds[i,5] <- explanation$dt[[5]]


}


## only ctree
##    none      lstat         rm        dis     indus
# 1: 22.446  5.2478733 -2.2704782  0.7477852 5.1302701
# 2: 22.446 -0.0394935 -0.6777271  0.6334870 0.8896713
# 3: 22.446  6.2698613  4.0289198 -0.4054951 0.7761810
# 4: 22.446  8.5207368 -0.4613162  0.4678893 2.4568345
# 5: 22.446  1.1297962  4.9023874  0.6606651 2.5909899
# 6: 22.446  2.3805829 -4.9595071  1.1750258 3.8079213

## ctree x 2 + gaussian x 2
##     none     lstat         rm         dis     indus
# 1: 22.446 5.1334789 -1.8077560  0.33861316 5.1911140
# 2: 22.446 0.6534682 -1.3193159  0.56640689 0.9053786
# 3: 22.446 5.9199993  4.6641804 -0.40937182 0.4946587
# 4: 22.446 8.2900067  0.4426851 -0.04711344 2.2985658
# 5: 22.446 1.2635051  4.4290735  0.74087512 2.8503849
# 6: 22.446 2.2348827 -3.9584153  0.77331270 3.3542426

## ctree x 1 + gaussian x 3
##     none    lstat        rm         dis     indus
# 1: 22.446 5.501601 -1.770074  0.06928766 5.0546347
# 2: 22.446 1.343186 -1.856806  0.48806455 0.8314933
# 3: 22.446 5.771298  4.793626 -0.10944005 0.2139824
# 4: 22.446 7.454794  1.909310 -0.65166921 2.2717096
# 5: 22.446 1.757528  4.708487  0.25634660 2.5614767
# 6: 22.446 2.924710 -3.767870  0.29449977 2.9526822

## ctree mincriterion = (0.95, 0.05) with comb_indici = 0 <-- this only takes the 2nd mincriterion
##     none       lstat         rm        dis      indus
# 1: 22.446  3.63123856 -0.4906170 -0.5979688  6.3127970
# 2: 22.446 -0.04730793 -0.3145696  0.6585474  0.5092680
# 3: 22.446  6.85616471  5.4805133 -1.0464836 -0.6207279
# 4: 22.446  9.24518581 -1.3323430  0.5237062  2.5475957
# 5: 22.446  0.45156196  5.3004154  0.9026735  2.6291880
# 6: 22.446  1.31846782 -3.4238966  1.1447784  3.3646732


## ctree mincriterion = 0.05
##     none       lstat         rm        dis      indus
# 1: 22.446  3.63123856 -0.4906170 -0.5979688  6.3127970
# 2: 22.446 -0.04730793 -0.3145696  0.6585474  0.5092680
# 3: 22.446  6.85616471  5.4805133 -1.0464836 -0.6207279
# 4: 22.446  9.24518581 -1.3323430  0.5237062  2.5475957
# 5: 22.446  0.45156196  5.3004154  0.9026735  2.6291880
# 6: 22.446  1.31846782 -3.4238966  1.1447784  3.3646732
