


no.per.dim <- 10^3
no.dim <- 20

cc <- rep(NA,no.dim)

for (i in 1:no.dim){
    dim0 <- i

#    one <- mvtnorm::rmvnorm(1,sigma=diag(dim0))

    two <- mvtnorm::rmvnorm(no.per.dim,sigma=diag(dim0))

    bb <- rep(NA,no.per.dim)
    for (j in 1:no.per.dim){
        one <- two[j,]

        aa = t(c(one)-t(two))

        bb[j] <- mean(sqrt(rowSums(aa^2)))

    }
    cc[i] <- mean(bb)

}


cc/(1:no.dim)

cc/sqrt(1:no.dim)


cc^2/(1:no.dim)^2

cc^2/(1:no.dim)
