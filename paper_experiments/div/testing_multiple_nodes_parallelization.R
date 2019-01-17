############ MULTIPLE

primary = "jullum-ubuntu"

machineAddresses <- list(
    list(host=primary,user='jullum',
         ncore=1),
    list(host='hpc05',user='jullum',
         ncore=1)
)

primary = "jullum-ubuntu"

machineAddresses <- list(
    list(host=primary,user='jullum',
         ncore=1)
)


spec <- lapply(machineAddresses,
               function(machine) {
                   rep(list(list(host=machine$host,
                                 user=machine$user)),
                       machine$ncore)
               })
spec <- unlist(spec,recursive=FALSE)


parallelCluster <- parallel::makeCluster(type='PSOCK',
                                         master=primary,
                                         spec=spec)




