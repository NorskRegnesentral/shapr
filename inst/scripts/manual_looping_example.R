# Example on how to manually run batches of feature combinations through
# explain() and thereby do just a portion of the heavy computations at a time to save memory.

# The idea is to store dt_res every time, combine those in the end and run
# KernelSHAP to compute the full Shapley values in the end.

# This is just an illustration of the concept. For very memory intensive cases,
# it might be wise to save the dt_mat to disk, clear out unneeded objects from time to time,
# run the garbage collector and possibly even restart R from time to time.

library(xgboost)
library(shapr)
library(data.table)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus","nox","age","tax","ptratio")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)


# Prepare the data for explanation
explainer <- shapr(x_train, model,n_combinations = 100)

# Decides how to split the feature combinations in explainer into chunks to be passed one by one
no_batches = 10
set.seed(123)
no_samples = nrow(explainer$S)
p = mean(y_train)

x0 = 2:(no_samples-1)
x = x0[sample(1:length(x0),size = length(x0),replace = F)]
S_groups = split(x, cut(seq_along(x), no_batches, labels = FALSE))
dt_mat_list = list()
for (i in 1:no_batches){
  S_groups[[i]] = sort(S_groups[[i]])
  tmp_explainer = explainer

  # Subsetting the required tables in the explainer
  these_samples = sort(c(1,S_groups[[i]],no_samples)) # Need to always include the first and last row due to a coding thing
  tmp_explainer$S = tmp_explainer$S[these_samples,]
  tmp_explainer$W = tmp_explainer$W[,these_samples]
  tmp_explainer$X = tmp_explainer$X[these_samples]

  # Running explain for only a subset of the feature combinations
  tmp_explanation <- explain(
    x_test,
    approach = "empirical",
    explainer = tmp_explainer,
    prediction_zero = p
  )

  dt_mat_list[[i]] = cbind(tmp_explanation$dt_mat,row_id=these_samples) # Keeping the dt_mat with an identifier for which S row this is

  print(i)
  }

# Joining all the dt_mats
dt_mat_final = data.table::rbindlist(dt_mat_list)
dt_mat_final = unique(dt_mat_final)

setkey(dt_mat_final,row_id)
dt_mat_final[,row_id:=NULL]

# Running kernelSHAP on the merged version of dt_mat
cnms <- colnames(tmp_explanation$x_test)
kshap <- t(explainer$W %*% as.matrix(dt_mat_final))
dt_kshap <- data.table::as.data.table(kshap)
colnames(dt_kshap) <- c("none", cnms)

# Adding proper class etc
explanation_batch <- list(dt = dt_kshap, model = tmp_explanation$model, p = tmp_explanation$p, x_test = tmp_explanation$x_test)
attr(explanation_batch, "class") <- c("shapr", "list")

plot(explanation_batch,index_x_test = 1:4) # This is a proper shapr object, so plotting etc works

### Checking similarity to original version which is more efficient, but may also be more memory hungry

org_explanation <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)
org_explanation$dt_mat = NULL # Removing the dt_mat

all.equal(org_explanation,explanation_batch)
#[1] TRUE




