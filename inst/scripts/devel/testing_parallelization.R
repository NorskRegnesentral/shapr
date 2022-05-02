

library(shapr)
library(future)
library(MASS)
library(microbenchmark)
library(data.table)

p_vec <- 10#2:10
n_train_vec <- 1000
n_test_vec <- 100#c(2,10,20)
n_batches_vec <- c(1,2,4,8,16,24,32)#seq(2,20,by=5)
n_cores_vec <- c(1,2,4,8,16,24,32)#c(1,seq(2,32,by=5))
approach_vec <- c("empirical","gaussian","ctree")#rev(c("empirical","gaussian"))
reps <- 2

max_n <- 10^5
max_p <- 10
rho <- 0.3
Sigma <- matrix(rho,max_p,max_p)
diag(Sigma) <- 1
mu <- rep(0,max_p)
beta <- c(1,seq_len(max_p)/max_p)
sigma_eps <- 1

set.seed(123)
x_all <- MASS::mvrnorm(max_n,mu = mu,Sigma = Sigma)
y_all <- as.vector(cbind(1,x_all)%*%beta)+rnorm(max_n,mean = 0,sd = sigma_eps)


res_dt <- as.data.table(expand.grid(p = p_vec,
                                    n_train = n_train_vec,
                                    n_test = n_test_vec,
                                    n_batches = n_batches_vec,
                                    n_cores = n_cores_vec,
                                    approach = approach_vec))

res_dt[,n_cores:=ifelse(n_cores>n_batches,n_batches,n_cores)]
res_dt <- unique(res_dt)

res_dt[,approach:=as.character(approach)]
res_dt[,time_median:=as.numeric(NA)]
res_dt[,time_min:=as.numeric(NA)]
res_dt[,mem_alloc:=as.numeric(NA)]


for(i in seq_len(nrow(res_dt))){
#for(i in sample.int(nrow(res_dt),10)){

  set.seed(123)
  these_p <- sample.int(max_p,size=res_dt[i,p])
  these_train <- sample.int(max_n,size=res_dt[i,n_train])
  these_test <- sample.int(max_n,size=res_dt[i,n_test])

  x_train <- as.data.frame(x_all[these_train,these_p])
  x_test <- as.data.frame(x_all[these_test,these_p])

  y_train <- y_all[these_train]

  xy_train <- cbind(x_train,y=y_train)

  model <- lm(formula = y~.,data=xy_train)

  explainer <- shapr(x_train, model)
  p <- mean(y_train)


  n_batches_use <- min(nrow(explainer$S),res_dt[i,n_batches])
  n_cores_use <- res_dt[i,n_cores]
  approach_use <- res_dt[i,approach]

  #future::plan("multicore",workers=n_cores_use)
  future::plan("multisession",workers=n_cores_use)


  res0 <- bench::mark({
    explanation <- explain(
      x_test,
      approach = approach_use,
      explainer = explainer,
      prediction_zero = p,n_batches = n_batches_use
    )},iterations = reps,time_unit ='s',memory = F,
    min_time = Inf
  )

  res_dt[i,c("time_median","time_min","mem_alloc"):= list(res0$median,res0$min,res0$mem_alloc/1024^2),]

  # res_dt[p==res_dt[i,p] &
  #          n_train == res_dt[i,n_train] &
  #          n_test == res_dt[i,n_test] &
  #          n_cores == res_dt[i,n_cores] &
  #          n_batches == res_dt[i,n_batches] &
  #          approach == approach_use,
  #        res:=res0$time[3]/10^6
  # ]

  print(res_dt[i])
}

setkey(res_dt,time_median)

#res_dt[approach=="gaussian"]




# p n_train n_test n_batches n_cores  approach time_median   time_min mem_alloc
# 1: 10    1000    100         5      10 empirical    8.264136   8.199610        NA
# 2: 10    1000    100         5       5 empirical    8.277614   8.224627        NA
# 3: 10    1000    100         5      15 empirical    8.351432   8.189444        NA
# 4: 10    1000    100         5      20 empirical    8.394858   8.317760        NA
# 5: 10    1000    100         5      30 empirical    8.496488   8.453119        NA
# 6: 10    1000    100        10       5 empirical   10.534386  10.523246        NA
# 7: 10    1000    100        10      10 empirical   11.659772  11.659772        NA
# 8: 10    1000    100        10      15 empirical   11.767503  11.767503        NA
# 9: 10    1000    100        10      30 empirical   11.835323  11.835323        NA
# 10: 10    1000    100        10      20 empirical   11.902262  11.902262        NA
# 11: 10    1000    100        20       5 empirical   14.750653  14.718519        NA
# 12: 10    1000    100        20      30 empirical   15.426510  15.398783        NA
# 13: 10    1000    100        20      15 empirical   15.426532  15.388514        NA
# 14: 10    1000    100        20      20 empirical   15.468479  15.426808        NA
# 15: 10    1000    100        20      10 empirical   15.564483  15.536153        NA
# 16: 10    1000    100        10       2 empirical   16.275958  16.155311        NA
# 17: 10    1000    100         5       2 empirical   16.520838  16.484130        NA
# 18: 10    1000    100        20       2 empirical   22.812822  22.733153        NA
# 19: 10    1000    100         5       1 empirical   32.814998  32.723445        NA
# 20: 10    1000    100        10       1 empirical   33.740455  33.284869        NA
# 21: 10    1000    100        10      30  gaussian   42.697496  42.123002        NA
# 22: 10    1000    100        10      15  gaussian   43.153707  42.400444        NA
# 23: 10    1000    100        10      20  gaussian   43.331616  42.330915        NA
# 24: 10    1000    100        10      10  gaussian   43.601197  42.580585        NA
# 25: 10    1000    100        20      10  gaussian   43.713152  42.444733        NA
# 26: 10    1000    100        20       1 empirical   44.970672  44.957254        NA
# 27: 10    1000    100        20      15  gaussian   48.515789  48.364623        NA
# 28: 10    1000    100        20      30  gaussian   48.980771  48.716296        NA
# 29: 10    1000    100        20      20  gaussian   49.048357  48.585454        NA
# 30: 10    1000    100         5      10  gaussian   49.929313  49.906563        NA
# 31: 10    1000    100         5       5  gaussian   49.952981  49.428697        NA
# 32: 10    1000    100        20       5  gaussian   49.954880  49.645313        NA
# 33: 10    1000    100         5      30  gaussian   50.220795  49.894032        NA
# 34: 10    1000    100         5      20  gaussian   50.480277  50.116526        NA
# 35: 10    1000    100         5      15  gaussian   50.616905  50.517388        NA
# 36: 10    1000    100        10       5  gaussian   50.739175  48.893451        NA
# 37: 10    1000    100        20      20     ctree   79.067415  79.060347        NA
# 38: 10    1000    100        20      30     ctree   79.178795  78.830831        NA
# 39: 10    1000    100        20      10     ctree   80.194740  76.259531        NA
# 40: 10    1000    100        10      20     ctree   84.368049  83.086716        NA
# 41: 10    1000    100        10      10     ctree   84.583532  84.125999        NA
# 42: 10    1000    100        20      15     ctree   85.021570  84.921147        NA
# 43: 10    1000    100        10      30     ctree   86.293475  83.902999        NA
# 44: 10    1000    100        10      15     ctree   86.549406  85.115549        NA
# 45: 10    1000    100        20       5     ctree   92.955276  92.538537        NA
# 46: 10    1000    100        10       5     ctree   94.816191  92.215222        NA
# 47: 10    1000    100         5      15     ctree   94.846974  94.641546        NA
# 48: 10    1000    100        10       2  gaussian   95.399388  95.341892        NA
# 49: 10    1000    100         5      20     ctree   95.887569  95.437676        NA
# 50: 10    1000    100         5       5     ctree   95.938850  93.705034        NA
# 51: 10    1000    100         5      30     ctree   96.015618  92.434543        NA
# 52: 10    1000    100         5      10     ctree   96.238056  94.071784        NA
# 53: 10    1000    100        20       2  gaussian   96.379812  95.719475        NA
# 54: 10    1000    100         5       2  gaussian  109.674539 108.807517        NA
# 55: 10    1000    100        10       1  gaussian  189.596560 188.909395        NA
# 56: 10    1000    100         5       1  gaussian  191.256527 191.157274        NA
# 57: 10    1000    100        20       1  gaussian  196.929709 196.358810        NA
# 58: 10    1000    100        10       2     ctree  200.709682 200.523174        NA
# 59: 10    1000    100        20       2     ctree  200.942230 200.570071        NA
# 60: 10    1000    100         5       2     ctree  237.327601 236.488531        NA
# 61: 10    1000    100        10       1     ctree  395.500767 393.656852        NA
# 62: 10    1000    100         5       1     ctree  402.635571 401.290227        NA
# 63: 10    1000    100        20       1     ctree  403.930240 403.903723        NA
# p n_train n_test n_batches n_cores  approach time_median   time_min mem_alloc





