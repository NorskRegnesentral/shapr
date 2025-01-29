library(data.table)
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

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  phi0 = p0,
  adaptive = TRUE,max_n_coalitions = 12,
  output_args = list(keep_samp_for_vS  = TRUE)
)


dt_samp_for_vS <- copy(explanation$internal$output$dt_samp_for_vS)
dt_vS <- copy(explanation$internal$output$dt_vS)

dt_samp_for_vS[,w := w/sum(w),by=.(id_coalition,id)]

matrix(dt_samp_for_vS[,sum(w*p_hat1),by=.(id_coalition,id)]$V1,byrow = TRUE,ncol=6)

dt_vS[-1,-1]

weights <- dt_samp_for_vS[id_coalition==2 & id == 1,w]
vals <- dt_samp_for_vS[id_coalition==2 & id == 1,p_hat1]

sqrt(Hmisc::wtd.var(x = vals,weights = weights,normwt = TRUE))

dt_samp_for_vS[,sum(w*p_hat1),by=.(id_coalition,id)]
vars_id2 <- c(0,dt_samp_for_vS[id==2,Hmisc::wtd.var(x = p_hat1, weights = w,normwt=TRUE),by=.(id_coalition,id)]$V1[1:10],0)

varmat_id2 = diag(12)
diag(varmat_id2) <- vars_id2

W <- copy(explanation$internal$objects$W)

round(sqrt(diag(W %*% varmat_id2 %*% t(W)))/12,3)

explanation$shapley_values_sd
