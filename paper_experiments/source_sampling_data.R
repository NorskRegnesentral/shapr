# Sampling train and test data

XYtrain <- data.table(samp_variables(n = nTrain,
                                     pi.G = pi.G,
                                     mu.list = mu.list,
                                     Sigma.list = Sigma.list))
XYtrain[,y:=samp_model(.N,.SD,sd_noise=sd_noise)]
Xtrain <- copy(XYtrain)
Xtrain[,y:=NULL]

XYtest <- data.table(samp_variables(n = nTest,
                                    pi.G = pi.G,
                                    mu.list = mu.list,
                                    Sigma.list = Sigma.list))
XYtest[,y:=samp_model(.N,.SD,sd_noise=sd_noise)]
Xtest <- copy(XYtest)
Xtest[,y:=NULL]

pred_zero = XYtrain[, mean(y)]

