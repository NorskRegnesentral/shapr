

# Standalone example


source("inst/explain_manual_source.R")

library(xgboost)
library(data.table)
library(shapr)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

# Renaming for convenience
names(data)[2:5] <- c("G","S0","C1","C2")

x_var <- c("G","S0","C1","C2") #c("Solar.R", "Wind", "Temp", "Month")
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


Genes <- c("G")
Status <- c("S0")
Confounders <- c("C1","C2")

#### 1. hierarchy of interest: Genes before disease status ####
R_D <- R_D_Matrix(Genes, Status, Confounders,
                  Ordering_between = list("Genes", "Status"),
                  verbose = FALSE)

shap_names <- names(x_explain)

coalition_list <- parse_coalitions(shap_names, names(R_D))

explain_manual(model = model,
               x_explain = x_explain,
               x_train = x_train,
               approach = "gaussian",
               phi0 = p0,
               coalition_list = coalition_list,
               R_D = as.matrix(R_D)
)



