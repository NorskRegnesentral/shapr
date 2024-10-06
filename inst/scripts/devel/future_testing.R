
plan(multisession, workers = 5)  # Adjust the number of workers as needed
plan(sequential)  # Adjust the number of workers as needed

fun <- function(x) {
  print(x)
  if(z==0){
    if(x==5){
      Sys.sleep(1)
      z <<- 100
    }
    return(x+z)
  } else {
    return(NA)
  }
}

z <- 0




plan(multisession, workers = 5)
plan(multicore, workers = 5)

plan(sequential)

fun2 <- function(x){
  x^2
}


start <- proc.time()
for(i in 1:100){
  future.apply::future_lapply(1:10, fun2)
}
print(proc.time()-start)
#user  system elapsed
#14.985   0.045  20.323

start <- proc.time()
for(i in 1:10){
  future.apply::future_lapply(rep(1:10,10), fun2)
}
print(proc.time()-start)
#user  system elapsed
#1.504   0.005   2.009

start <- proc.time()
aa=future.apply::future_lapply(rep(1:10,100), fun2)
print(proc.time()-start)
#user  system elapsed
#0.146   0.000   0.202



