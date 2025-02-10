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

future::plan("multisession", workers = 4) # Increase the number of workers for increased performance with many features

progressr::handlers(global = TRUE)
progressr::handlers("cli") # Using the cli package as backend (recommended for the

e_arf <- explain(
  model = model,
  x_explain = head(x_explain,10),
  x_train = head(x_train,1000),
  approach = "arf",
  phi0 = p0,iterative = FALSE
)

# Register cores - Windows
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)

e_arf <- explain(
  model = model,
  x_explain = head(x_explain,10),
  x_train = head(x_train,1000),
  approach = "arf",
  phi0 = p0,iterative = FALSE
)



e_ctree <- explain(
  model = model,
  x_explain = head(x_explain,10),
  x_train = head(x_train,1000),
  approach = "ctree",
  phi0 = p0,iterative = FALSE
)


#e_gauss <- explain(
#  model = model,
#  x_explain = head(x_explain,10),
#  x_train = head(x_train,1000),
#  approach = "gaussian",
#  phi0 = p0,iterative = FALSE
#)

#e_emp$timing$total_time_secs
e_arf$timing$total_time_secs
e_ctree$timing$total_time_secs
#e_gauss$timing$total_time_secs


#e_emp$MSEv$MSEv
e_arf$MSEv$MSEv
e_ctree$MSEv$MSEv
#e_gauss$MSEv$MSEv


