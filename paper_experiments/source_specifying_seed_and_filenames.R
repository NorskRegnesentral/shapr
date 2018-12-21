
# Need a random seed to set unique filenames
seed0 <- print(as.numeric(Sys.time())*1000,digits=10) # Getting a time based seed.
seed0 <- abs(seed0 - signif(seed0))
seed0 <- round(seed0)
set.seed(seed0)
run_indicator <- stri_rand_strings(n = 1,length = 5)
run_date_time <- Sys.time()

# Using the pre-set seed for sampling if pre-set, otherwise the random seed generated above
this.seed <- ifelse(exists("this.seed"),this.seed,seed0)
set.seed(this.seed)
print(paste0("Setting sampling seed to ",this.seed))

current_csv_filename = paste0(initial_current_csv_filename,"__runind_",run_indicator,"__this.seed_",this.seed,".csv")
current_RData_filename = paste0(initial_current_csv_filename,"__runind_",run_indicator,"__this.seed_",this.seed,".RData")


