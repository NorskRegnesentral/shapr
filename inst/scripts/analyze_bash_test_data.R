

library(data.table)
### analysing bash data test

dt_mem0 <- fread("inst/scripts/memory_test_2023_new2.csv")

names(dt_mem0) <- c("date","time","mem_usage","rep","p","n_train","n_explain","n_batches","n_cores","approach","multicore_method","logfilename")

#dt_mem0 <- dt_mem0[date>="2023-01-18"]


dt_mem0[,max_mem_usage:=max(mem_usage),by=.(rep,p,n_train,n_explain,n_batches,n_cores,approach,multicore_method,logfilename)]
dt_mem0[,n_batches_real:=pmin(2^p-2,n_batches)]

dt_mem <- dt_mem0[mem_usage==max_mem_usage,.(date,time,mem_usage,rep,p,n_train,n_explain,n_batches_real,n_cores,approach,multicore_method)]

dt_mem[,mem_usage_Mb:=mem_usage/1024]

library(ggplot2)

ggplot(dt_mem,aes(x=n_batches_real,y=mem_usage_Mb,col=as.factor(n_explain),linetype=as.factor(n_train)))+
  geom_line()+
  geom_point()+
  facet_wrap(vars(approach,p),scales = "free",labeller = label_both)+
  scale_y_log10()+
  scale_x_log10()+
  ggtitle("Memory usage")

ggplot(dt_mem[p<16& p>2& approach=="empirical"],aes(x=n_batches_real,y=mem_usage_Mb,col=as.factor(n_explain)))+
  geom_line()+
  geom_point()+
  facet_wrap(vars(approach,p),scales = "free",labeller = label_both)+
  scale_y_log10()+
  scale_x_log10()+
  ggtitle("Memory usage for n_train=100")



dt_mem0[p==8 & n_explain==100 & approach=="ctree"]
dt_mem0[p==16 & n_explain==100 & approach=="ctree"]

# Wierd and inconsistent results


dt_time0 <- fread("inst/scripts/timing_test_2023_new2.csv")
#names(dt_time0) <- c("p","n_train","n_explain","n_batches","n_cores","approach","time","sys_time_start_explain","sys_time_end_explain",
#                     "secs_explain","rep","max_n","max_p","rho","sigma","mu_const","beta0","sigma_eps")

#dt_time0 <- dt_time0[time>="2023-01-18"]


dt_time0[,n_batches_real:=pmin(2^p-2,n_batches)]

dt_time <- dt_time0[,.(time,secs_explain,timing_setup,timing_test_prediction, timing_setup_computation ,timing_compute_vS ,timing_postprocessing ,timing_shapley_computation, rep,p,n_train,n_explain,n_batches_real,approach,n_combinations)]

dt_time[n_batches_real==1,secs_explain_singlebatch :=secs_explain]
dt_time[,secs_explain_singlebatch:=mean(secs_explain_singlebatch,na.rm=T),by=.(p,n_train,n_explain,approach,n_combinations)]
dt_time[,secs_explain_prop_singlebatch:=secs_explain/secs_explain_singlebatch]

ggplot(dt_time[p<14],aes(x=n_batches_real,y=secs_explain,col=as.factor(n_explain),linetype=as.factor(n_train)))+
  geom_line()+
  geom_point()+
  facet_wrap(vars(approach,p),scales = "free",labeller = label_both)+
  #scale_y_log10()+
  scale_x_log10()+
  ggtitle("Time usage")

ggplot(dt_time[p<14],aes(x=n_batches_real,y=secs_explain_prop_singlebatch,col=as.factor(n_explain),linetype=as.factor(n_train)))+
  geom_line()+
  geom_point()+
  facet_wrap(vars(approach,p),scales = "free",labeller = label_both)+
  #scale_y_log10()+
  scale_x_log10()+
  ggtitle("Time usage proportional to singlebatch")


ggplot(dt_time[p<14],aes(x=n_batches_real,y=timing_shapley_computation,col=as.factor(n_explain),linetype=as.factor(n_train)))+
  geom_line()+
  geom_point()+
  facet_wrap(vars(approach,p),scales = "free",labeller = label_both)+
  #scale_y_log10()+
  scale_x_log10()+
  ggtitle("Time usage")




ggplot(dt_time[p<16& p>2 & approach=="empirical"],aes(x=n_batches_real,y=secs_explain,col=as.factor(n_explain)))+
  geom_line()+
  geom_point()+
  facet_wrap(vars(approach,p),scales = "free",labeller = label_both)+
  #  scale_y_log10()+
  #  scale_x_log10()+
  ggtitle("Time usage for n_train=100")



#### Default for ctree + gaussian: Mye å spare minnemessig + lite å tape tidsmessig
# n_batches <- (2^p-2)
# max 100, min 10

n_batches_fun <- function(approach,p){
  n_combinations <- 2^p-2

  if(approach %in% c("ctree","gaussian","copula")){
    init <- ceiling(n_combinations/10)
    floor <- max(c(10,init))
    ret <- min(c(1000,floor))
  } else {
    init <- ceiling(n_combinations/100)
    floor <- max(c(2,init))
    ret <- min(c(100,floor))
  }
  return(ret)
}

n_batches_fun("empirical",10)


