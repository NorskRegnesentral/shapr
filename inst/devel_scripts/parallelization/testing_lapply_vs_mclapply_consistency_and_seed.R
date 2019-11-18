


fun <- function(x){
  rnorm(10)
}

set.seed(123)
aa_lapply = lapply(X = 1:10,FUN = fun)

set.seed(123)
aa_mclapply_1 = parallel::mclapply(X = 1:10,FUN = fun,mc.cores = 1)

set.seed(123)
aa_mclapply_2 = parallel::mclapply(X = 1:10,FUN = fun,mc.cores = 2,mc.set.seed=F)

set.seed(123)
aa_mclapply_4 = parallel::mclapply(X = 1:10,FUN = fun,mc.cores = 4,mc.set.seed=F)

set.seed(1235)
aa_mclapply_4_v2 = parallel::mclapply(X = 1:10,FUN = fun,mc.cores = 4,mc.set.seed=F)


all.equal(aa_lapply,aa_mclapply_1)
all.equal(aa_lapply,aa_mclapply_2)
all.equal(aa_lapply,aa_mclapply_4)
all.equal(aa_mclapply_4,aa_mclapply_4_v2)

all.equal(aa_mclapply_2,aa_mclapply_4)
