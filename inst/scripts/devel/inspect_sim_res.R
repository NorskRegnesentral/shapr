logfile_bash="memory_log_test_big.csv"
logfile_Rscript="timing_log_test_big.csv"


bash <- fread(file.path("inst/scripts/devel/",logfile_bash))
Rscript <- fread(file.path("inst/scripts/devel/",logfile_Rscript))

names(bash) <- c("date","time","mem","p","n_train","n_test","n_batches","n_cores","approach","multicore_method","logfilename")


bash[,mem_MB:=mem/1024]

bash[,list(max_mem_MB=max(mem_MB)),by=c("p","n_train","n_test","n_batches","n_cores","approach","multicore_method")]
str(bash)
