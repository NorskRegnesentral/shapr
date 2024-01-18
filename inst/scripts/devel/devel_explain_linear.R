set.seed(12345)

data <- data.table::as.data.table(airquality)

data_complete <- data[complete.cases(airquality), ]
data_complete <- data_complete[sample(seq_len(.N))] # Sh

y_var_numeric <- "Ozone"

x_var_numeric <- c("Solar.R", "Wind", "Temp", "Month")#, "Day")

data_train <- head(data_complete, -3)
data_explain <- tail(data_complete, 3)

x_train_numeric <- data_train[, ..x_var_numeric]

x_explain_numeric <- data_explain[, ..x_var_numeric]

lm_formula_numeric <- as.formula(paste0(y_var_numeric, " ~ ", paste0(x_var_numeric, collapse = " + ")))

model_lm_numeric <- lm(lm_formula_numeric, data = data_complete)

p0 <- data_train[, mean(get(y_var_numeric))]

x_explain_numeric_new <- rbind(x_explain_numeric,t(rep(0,length(x_var_numeric))),t(rep(1,length(x_var_numeric))),use.names=FALSE)





test <-explain(model = model_lm_numeric,
               x_explain = x_explain_numeric_new,
               x_train = x_train_numeric,
               approach = "gaussian",
               shap_approach = "permutation",
               prediction_zero = p0,n_samples = 10^5)
test
#none Solar.R    Wind       Temp   Month
#1: 42.44  -8.379   7.944   15.29533  0.6522
#2: 42.44   4.919  -4.755  -11.00089 -1.0133
#3: 42.44   7.302 -25.612   -0.08335 -0.5415
#4: 42.44 -12.371  72.269 -154.31630 -6.0802
#5: 42.44 -11.922  67.037 -153.72291 -6.2782



#debugonce(explain_linear)


test2 <-explain_linear(model = model_lm_numeric,
                       x_explain = x_explain_numeric_new,
                       x_train = x_train_numeric,
                       prediction_zero = p0,
                       n_permutations=4,
                       )
test2

test2 <-explain_linear(model = model_lm_numeric,
                       x_explain = x_explain_numeric_new,
                       x_train = x_train_numeric,
                       prediction_zero = p0,
)
test2


unique(test2$internal$objects$US_list)


### We don't get the same results... Investigating by looking at the v(S) we get:

test$internal$output$dt_vS[id_combination==2]

# Look at the second id_combination (S={1})

test2$internal$objects$S[c(2,31),]

QS <- test2$internal$objects$QS_list[[2]]
QSbar <-  test2$internal$objects$QS_list[[31]]
US <- test2$internal$objects$US_list[[2]]
mu <- test2$internal$parameters$gaussian.mu
Sig <- test2$internal$parameters$gaussian.cov
x <- unlist(test2$internal$data$x_explain[1,])
coefs <- test2$internal$parameters$linear_model_coef
b <- coefs[1]
beta <- coefs[-1]

Ex <- (QSbar-US)%*%mu + (QS+US)%*%x
beta%*%Ex + b

test$internal$output$dt_vS[id_combination==2]

condmu_manual <- c(x[1],mu[-1] + Sig[-1,1]%*%solve(Sig[1,1])*(x[1]-mu[1]))
# Same as Ex


### TODO: Check also other combinations, but seems that the formula for E[x|xS] is correct, so error must be in the Tx/Tmu stuff.





