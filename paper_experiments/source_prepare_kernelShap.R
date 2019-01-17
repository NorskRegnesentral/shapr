## Preparing kernelShap

# All are default choices, only data are inserted.

l <- prepare_kernelShap(
    m = ncol(Xtrain),
    Xtrain = Xtrain,
    Xtest = Xtest
)
