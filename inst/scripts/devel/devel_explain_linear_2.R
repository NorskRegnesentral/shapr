set.seed(12345)

p <- 4
n_train <- 10^3
beta <- rep(1,4)
b <- 1
mu <- rep(1,4)
Sig <- diag(4)

data <- as.data.table(mvtnorm::rmvnorm(n_train,mean=mu,sigma=Sig))

y <- as.vector(b + as.matrix(data)%*%beta + rnorm(n_train,sd=.01))

model <- lm(y~., data = cbind(y,data))

p0 <- as.numeric(b+mu%*%beta)

x_explain <- rbind(head(data,2),
                  t(rep(0,4)),
                  t(rep(1,4)),
                  t(1:4),
                  use.names=FALSE)


test <-explain(model = model,
               x_explain = x_explain,
               x_train = data,
               approach = "gaussian",
               shap_approach = "permutation",
               prediction_zero = p0,
               gaussian.cov_mat=Sig,
               gaussian.mu = mu,
               n_samples = 10^5)
test


#debugonce(explain_linear)


test2 <-explain_linear(model = model,
                       x_explain = x_explain,
                       x_train = data,
                       prediction_zero = p0, # this is not used
                       gaussian.cov_mat=Sig,
                       gaussian.mu=mu,
)
test2

test2$internal$objects$US_list
test2$internal$objects$QS_list
test2$internal$objects$Tmu_list
test2$internal$objects$Tx_list


### something odd, here. the tx_list is not looking the same for differnet features, although there is no differnce between them...
# Look more closely into how that is created

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





