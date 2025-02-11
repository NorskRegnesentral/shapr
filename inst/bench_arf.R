library(xgboost)
library(shapr)


url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
data <- data.table::fread(url,stringsAsFactors=TRUE)

colnames(data) <- c('age', 'workclass', 'fnlwgt', 'educatoin',
                     'educatoin_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex',
                     'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')


data <- data[complete.cases(data), ]

data[,response:=(income==">50K")*1]

#x_var <- colnames(data)[c(1,3,5,11,12,13)]
x_var <- head(colnames(data),10)
y_var <- "response"

set.seed(123)

ind_x_explain <- sample(1:nrow(data), 1000)
x_train <- data[-ind_x_explain, ..x_var]
y_train <- data[-ind_x_explain, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]


## Fitting a basic xgboost model to the training data
#model <- xgboost(
#  data = as.matrix(x_train),
#  label = y_train,
#  nround = 20,
#  verbose = FALSE
#)

model <- ranger::ranger(paste0("y_train~",paste0(x_var,collapse="+")),
                        data = cbind(x_train,y_train),
                        num.trees = 100,
                        probability = TRUE)

# Specifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

# Computing the Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
#e_emp <- explain(
#  model = model,
#  x_explain = head(x_explain,10),
#  x_train = head(x_train,1000),
#  approach = "empirical",
#  phi0 = p0,iterative = FALSE
#)

future::plan("multisession", workers = 5) # Increase the number of workers for increased performance with many features

progressr::handlers(global = TRUE)
progressr::handlers("cli") # Using the cli package as backend (recommended for the

# Get an idea about the scaliability of the ARF approach

e_arf <- explain(
  model = model,
  x_explain = head(x_explain,10),
  x_train = head(x_train,1000),
  approach = "arf",
  phi0 = p0,iterative = FALSE,max_n_coalitions = 10
)


e_arf2 <- explain(
  model = model,
  x_explain = head(x_explain,50),
  x_train = head(x_train,1000),
  approach = "arf",
  phi0 = p0,iterative = FALSE,max_n_coalitions = 10
)


e_arf3 <- explain(
  model = model,
  x_explain = head(x_explain,10),
  x_train = head(x_train,5000),
  approach = "arf",
  phi0 = p0,iterative = FALSE,max_n_coalitions = 10
)


e_arf4 <- explain(
  model = model,
  x_explain = head(x_explain,10),
  x_train = head(x_train,1000),
  approach = "arf",
  phi0 = p0,iterative = FALSE,max_n_coalitions = 50
)


e_arf$timing$total_time_secs
e_arf$timing$total_time_secs*5
e_arf2$timing$total_time_secs
e_arf3$timing$total_time_secs
e_arf4$timing$total_time_secs

# > e_arf$timing$total_time_secs
# [1] 7.78881
# > e_arf$timing$total_time_secs*5
# [1] 38.94405
# > e_arf2$timing$total_time_secs
# [1] 11.73266
# > e_arf3$timing$total_time_secs
# [1] 7.52352
# > e_arf4$timing$total_time_secs
# [1] 8.814752

future::plan("multisession", workers = 10) # Increase the number of workers for increased performance with many features

e_arf0 <- explain(
  model = model,
  x_explain = head(x_explain,100),
  x_train = head(x_train,1000),
  approach = "arf",
  phi0 = p0,iterative = FALSE,max_n_coalitions = 500
)

e_ctree0 <- explain(
  model = model,
  x_explain = head(x_explain,100),
  x_train = head(x_train,1000),
  approach = "ctree",
  phi0 = p0,iterative = FALSE,max_n_coalitions = 500
)


e_arf0$timing$total_time_secs
e_ctree0$timing$total_time_secs


e_arf0$MSEv$MSEv
e_ctree0$MSEv$MSEv

# > e_arf0$timing$total_time_secs
# [1] 279.4276
# > e_ctree0$timing$total_time_secs
# [1] 771.6911
# >
#   >
#   > e_arf0$MSEv$MSEv
# MSEv     MSEv_sd
# <num>       <num>
#   1: 0.02304781 0.001990692
# > e_ctree0$MSEv$MSEv
# MSEv     MSEv_sd
# <num>       <num>
#   1: 0.02101031 0.001956703


