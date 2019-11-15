# Sampling train and test data

X_GenHyp <- ifelse(exists("X_GenHyp"),X_GenHyp,F)

if (!X_GenHyp){
    XYtrain <- data.table(samp_variables(n = nTrain,
                                         pi.G = pi.G,
                                         mu.list = mu.list,
                                         Sigma.list = Sigma.list))
    XYtest <- data.table(samp_variables(n = nTest,
                                        pi.G = pi.G,
                                        mu.list = mu.list,
                                        Sigma.list = Sigma.list))


} else {
    XYtrain <- data.table(samp_variables(n = nTrain,
                                         Sigma = Sigma,
                                         beta = beta,
                                         omega = omega,
                                         lambda = lambda,
                                         mu = mu))

    XYtest <- data.table(samp_variables(n = nTest,
                                        Sigma = Sigma,
                                        beta = beta,
                                        omega = omega,
                                        lambda = lambda,
                                        mu = mu))

}

XYtrain[,y:=samp_model(.N,.SD,sd_noise=sd_noise)]
Xtrain <- copy(XYtrain)
Xtrain[,y:=NULL]

XYtest[,y:=samp_model(.N,.SD,sd_noise=sd_noise)]
Xtest <- copy(XYtest)
Xtest[,y:=NULL]

pred_zero = XYtrain[, mean(y)]

