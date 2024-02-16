set.seed(12345)
library(data.table)
library(profvis)

p <- 8
n_train <- 10^3
beta <- rnorm(p)#rep(1,p)
b <- rnorm(1)
mu <- rnorm(p)#rep(1,p)

# Random covariance matrix
A <- matrix(runif(p^2,min = -1,max=1), ncol=p)
Sig <- t(A) %*% A
#Sig <- diag(4)


data <- as.data.table(mvtnorm::rmvnorm(n_train,mean=mu,sigma=Sig))

y <- as.vector(b + as.matrix(data)%*%beta + rnorm(n_train,sd=.01))

model <- lm(y~., data = cbind(y,data))

p0 <- as.numeric(b+mu%*%beta)

x_explain <- rbind(head(data,2),
                  t(rep(0,p)),
                  t(rep(1,p)),
                  t(1:p),
                  use.names=FALSE)

profvis({
 test <-explain(model = model,
                x_explain = x_explain,
                x_train = data,
                approach = "gaussian",
                prediction_zero = p0,
                gaussian.cov_mat=Sig,
                gaussian.mu = mu,
                n_samples = 10)
})

 test

#debugonce(explain_linear)

#profvis({
  test2 <-explain_lingauss(model = model,
                           x_explain = x_explain,
                           x_train = data,
                           gaussian.cov_mat=Sig,
                           gaussian.mu=mu,n_permutations = 10^4
  )
#})

test3 <-explain_lingauss_precomputed(test2,
                                     x_explain = x_explain[rep(seq_len(5),each=2*10^5)]

)
# 10^6 explanations in 2 seconds in plain R
test3$timing$total_time_secs




test2$internal$objects$perm_dt[,.N]

all_lists <- NULL
for(i in 1:p){
  all_lists <- c(all_lists,test2$internal$objects$tmp_lists[[i]]$Udiff)
}
length(unique(all_lists))

test2$internal$objects$tmp_lists[[1]]$Udiff[[1]]+test2$internal$objects$tmp_lists[[1]]$Udiff[[2]]
test2$internal$objects$tmp_lists[[1]]$Udiff[[3]]+test2$internal$objects$tmp_lists[[1]]$Udiff[[4]]
test2$internal$objects$tmp_lists[[1]]$Udiff[[5]]+test2$internal$objects$tmp_lists[[1]]$Udiff[[6]]

test2$internal$objects$tmp_lists[[2]]$Udiff[[1]]+test2$internal$objects$tmp_lists[[2]]$Udiff[[2]]
test2$internal$objects$tmp_lists[[2]]$Udiff[[3]]+test2$internal$objects$tmp_lists[[2]]$Udiff[[4]]
test2$internal$objects$tmp_lists[[2]]$Udiff[[5]]+test2$internal$objects$tmp_lists[[2]]$Udiff[[6]]



length(unique(test2$internal$objects$tmp_lists[[1]]$Udiff))
length(unique(test2$internal$objects$tmp_lists[[2]]$Udiff))
length(unique(test2$internal$objects$tmp_lists[[3]]$Udiff))
length(unique(test2$internal$objects$tmp_lists[[4]]$Udiff))

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
coefs <- test2$internal$parameters$lingauss_model_coef
b <- coefs[1]
beta <- coefs[-1]

Ex <- (QSbar-US)%*%mu + (QS+US)%*%x
beta%*%Ex + b

test$internal$output$dt_vS[id_combination==2]

condmu_manual <- c(x[1],mu[-1] + Sig[-1,1]%*%solve(Sig[1,1])*(x[1]-mu[1]))
# Same as Ex


### TODO: Check also other combinations, but seems that the formula for E[x|xS] is correct, so error must be in the Tx/Tmu stuff.





