

# Other libraries
library(xgboost)
library(data.table)
library(shapr)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:20
x_train <- data[-ind_x_explain, ..x_var]
y_train <- data[-ind_x_explain, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]

# Fitting a basic xgboost model to the training data
set.seed(123) # Set seed for reproducibility
model <- xgboost::xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nrounds = 20,
  verbosity = 0
)

# Specifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

test_minimal <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  phi0 = p0,
  seed = 1,
  approach = "regression_separate",
  regression.model = parsnip::decision_tree(tree_depth = hardhat::tune(), engine = "rpart", mode = "regression"),
  regression.tune_values = expand.grid(tree_depth = c(1, 2)),
  regression.vfold_cv_para = list(v = 2),
  verbose = c("vS_details"),
  iterative = FALSE,
  max_n_coalitions = 6,
  iterative_args = list(initial_n_coalitions = 2),
  extra_computation_args = list(max_batch_size = 5)
)


