setwd("vignettes")

library(ggplot2)
library(xgboost)
library(data.table)
library(shapr)

# Additional packages which are only used for plotting in this vignette.
# There are not listed as dependencies is shapr
library(GGally)
library(ggpubr)
library(gridExtra)



# Ensure that shapr's functions are prioritzed, otherwise we need to use the `shapr::`
# prefix when calling explain(). The `conflicted` package is imported by `tidymodels`.
conflicted::conflicts_prefer(shapr::explain, shapr::prepare_data)


# Set up the data
# Can also download the data set from the source https://archive.ics.uci.edu/dataset/275/bike+sharing+dataset
# temp <- tempfile()
# download.file("https://archive.ics.uci.edu/static/public/275/bike+sharing+dataset.zip", temp)
# bike <- read.csv(unz(temp, "day.csv"))
# unlink(temp)
bike <- read.csv("../inst/extdata/day.csv")
# Difference in days, which takes DST into account
bike$trend <- as.numeric(difftime(bike$dteday, bike$dteday[1], units = "days"))
bike$cosyear <- cospi(bike$trend / 365 * 2)
bike$sinyear <- sinpi(bike$trend / 365 * 2)
# Unnormalize variables (see data set information in link above)
bike$temp <- bike$temp * (39 - (-8)) + (-8)
bike$atemp <- bike$atemp * (50 - (-16)) + (-16)
bike$windspeed <- 67 * bike$windspeed
bike$hum <- 100 * bike$hum




x_var <- c("trend", "cosyear", "sinyear", "temp", "atemp", "windspeed", "hum")
y_var <- "cnt"

# NOTE: To avoid RNG reproducibility issues across different systems, we
# load the training-test split from a file. 80% training and 20% test
train_index <- readRDS("../inst/extdata/train_index.rds")

# Training data
x_train <- as.matrix(bike[train_index, x_var])
y_train_nc <- as.matrix(bike[train_index, y_var]) # not centered
y_train <- y_train_nc - mean(y_train_nc)


x_explain <- as.matrix(bike[-train_index, x_var])
y_explain_nc <- as.matrix(bike[-train_index, y_var]) # not centered
y_explain <- y_explain_nc - mean(y_train_nc)

# Get 6 explicands to plot the Shapley values of with a wide spread in their predicted outcome
n_index_x_explain <- 6
index_x_explain <- order(y_explain)[seq(1, length(y_explain), length.out = n_index_x_explain)]
y_explain[index_x_explain]

# Fit an XGBoost model to the training data
model <- xgboost::xgboost(
  data = x_train,
  label = y_train,
  nround = 100,
  verbose = FALSE
)

# Save the phi0
phi0 <- mean(y_train)




group_list <- list(
  trend0 = "trend",
  cosyear0 = "cosyear",
  sinyear0 = "sinyear",
  temp_group0 = c("temp", "atemp"),
  windspeed0 = "windspeed",
  hum0 = "hum"
)

causal_ordering_group <-
  list("trend0", c("cosyear0", "sinyear0"), c("temp_group0", "windspeed0", "hum0"))
confounding <- c(FALSE, TRUE, FALSE)

debugonce(explain)

explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  approach = "gaussian",
  phi0 = phi0,
  asymmetric = FALSE,
  causal_ordering = causal_ordering_group,
  confounding = confounding,
  n_MC_samples = 1000,
  group = group_list
)
