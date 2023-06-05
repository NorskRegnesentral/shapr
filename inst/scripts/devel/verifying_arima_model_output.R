library(shapr)

options(digits = 5) # To avoid round off errors when printing output on different systems
set.seed(123)

n <- 10^3

xreg <- cbind(rnorm(n,mean=1,sd=1),
              rnorm(n,mean=2,sd=1))

noise <- rnorm(n,mean=0,sd=0.5)

# Create AR(1)-structure
beta <- c(1.5,0)
alpha <- 0.5 # AR-coefficient
mu <- 1

y <- rep(0,n)
y[1] <- mu +beta[1]*xreg[1,1]+beta[2]*xreg[1,2]+noise[1]


for(i in 2:n){
  y[i] <- mu +alpha*y[i-1]+beta[1]*xreg[i,1]+beta[2]*xreg[i,2]+noise[i]
}
plot(y,type="l")

# In practice this model is y = 1 + y[i] + 1.5*xreg1 with independent features

#model_arima_temp <- arima(y, c(3,1,2), xreg=xreg)
model_arima_temp <- arima(y, c(1,0,0), xreg=xreg)

colnames(xreg) <- c("var1","var2")

train_idx <- 1:(n-10)
explain_idx <- n-5:4


set.seed(123)
exp <- explain_forecast(model = model_arima_temp,
                        y = y,
                        xreg = xreg,
                        train_idx = train_idx,
                        explain_idx = explain_idx,
                        explain_y_lags = 1,
                        explain_xreg_lags = c(0,1),
                        horizon = 1,
                        approach = "empirical",
                        prediction_zero = rep(mean(y),1),
                        group_lags = FALSE,
                        n_batches = 1)

# These two should be approximately equal
# For y
exp$shapley_values$Y1.1
model_arima_temp$coef[1]*(y[explain_idx]-mean(y))
#[1] -0.13500  0.20643
#[1] -0.079164  0.208118


# for xreg1
exp$shapley_values$var1.F1
model_arima_temp$coef[3]*(xreg[explain_idx+1,1]-mean(xreg[,1]))
#[1] -0.030901  1.179386
#[1] -0.12034  1.19589

# for xreg2
exp$shapley_values$var2.F1
0
#[1] 0.011555 0.031911
#[1] 0


# Close enough (maybe increase sample size n to make sure they converge as they should?)



