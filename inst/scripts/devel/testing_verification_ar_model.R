library(data.table)
options(digits = 5) # To avoid round off errors when printing output on different systems
set.seed(123)

data <- as.data.table(matrix(rnorm(100*3),ncol=3))# first column is noise, the other two are xregs

# Create AR(1)-structure
y <- rep(0,100)
y[1] <- data[1,1]/5+data[1,2]+data[1,3]
for(i in 2:100){
  y[i] <- y[i-1]+data[i,1]/5+data[i,2]+data[i,3]
}
y <- unlist(y)
plot(y)

dat <- data.table(y=y,xreg1=unlist(data[,2]),xreg2=unlist(data[,3]))

model_arima_temp <- arima(dat$y, c(2,1,0), xreg=dat[,2:3])


set.seed(123)
exp <- explain_forecast(model = model_arima_temp,
                       y = dat$y,
                       xreg = dat[, 2:3],#dat[, 2:3],
                       train_idx = 10:50,
                       explain_idx = 71:72,
                       explain_y_lags = 0,
                       explain_xreg_lags = c(0,0),
                       horizon = 2,
                       approach = "empirical",
                       prediction_zero = c(0,0),
                       group_lags = FALSE,
                       n_batches = 1,
                       timing = FALSE,
                       n_combinations = 50
)


