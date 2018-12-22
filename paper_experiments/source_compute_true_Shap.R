#### Conputing the true Shapley values

X_GenHyp <- ifelse(exists("X_GenHyp"),X_GenHyp,F)

if (!X_GenHyp){ # If Gauss or Gaussmix
    if(X_dim==3){
        Shapley.true = Shapley_true(model = model,
                                    Xtrain = Xtrain,
                                    Xtest = Xtest,
                                    pi.G = pi.G,
                                    mu.list = mu.list,
                                    Sigma.list = Sigma.list,
                                    int.samp=1000,
                                    l = l,
                                    pred_zero = pred_zero)
    } else { # If Gauss with X_dim == 10, not working with Gaussmix for dim 10.

        feature_list <- l$X$features
        n_threshold_true <- 10^4
        p <- ncol(l$Xtest)
        ll <- list()

        for (i in l$Xtest[, .I]) { # This may be parallelized when the prediction function is not parallelized.
            print(sprintf("%d out of %d", i, l$Xtest[, .N]))

            Gauss_samp <- lapply(
                X = feature_list,
                FUN = samp_Gauss_func,
                n_threshold = n_threshold_true,
                mu = mu.list[[1]],
                Sigma = Sigma.list[[1]],
                p = p,
                Xtest = as.matrix(l$Xtest)[i, , drop = FALSE])


            DTp <- rbindlist(Gauss_samp, idcol = "wcomb")
            DTp[, w := 1 / n_threshold]
            DTp[wcomb %in% c(1, 2 ^ p), w := 1] # Adjust weights for zero and full model

            nms <- colnames(Xtest)

            DTp[!(wcomb %in% c(1, 2 ^ p)), p_hat := pred_vector(model = model, data = .SD), .SDcols = nms]
            if(nrow(Xtest)==1){
                DTp[wcomb == 2 ^ p, p_hat := pred_vector(model = model, data = as.data.frame(rbind(Xtest,Xtest)))[1]] # Just a hack for a single prediction
            } else {
                DTp[wcomb == 2 ^ p, p_hat := pred_vector(model = model, data = as.data.frame(as.matrix(l$Xtest)[i, , drop = FALSE]))]
            }
            DTp[wcomb == 1, p_hat := pred_zero]


            DTres <- DTp[, .(k = sum((p_hat * w) / sum(w))), wcomb]
            setkey(DTres, wcomb)
            ll[[i]] <- DTres
            ll[[i]][, id := i]
        }

        DT <- rbindlist(ll)

        Kshap <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
        for (i in l$Xtest[, .I]) {
            Kshap[i, ] = l$W %*% DT[id == i, k]
        }
        Shapley.true = list(exactShap = Kshap, other_objects = list(ll = ll, DT = DT))


    }
} else{ # If GenHyp, both for dim 3 and dim 10.
    #### Create a function of this !

    feature_list <- l$X$features
    n_threshold_true <- 10^4
    p <- ncol(l$Xtest)
    ll <- list()

    for (i in l$Xtest[, .I]) { # This may be parallelized when the prediction function is not parallelized.
        print(sprintf("%d out of %d", i, l$Xtest[, .N]))


        genHyp_samp <- lapply(
            X = feature_list,
            FUN = simulateCondDistHyperbolic_new,
            n_threshold = n_threshold_true,
            Sigma = Sigma,
            lambda = lambda,
            omega = omega,
            beta = beta,
            mu = mu,
            p = p,
            Xtest = as.matrix(l$Xtest)[i, , drop = FALSE])


        DTp <- rbindlist(genHyp_samp, idcol = "wcomb")
        DTp[, w := 1 / n_threshold]
        DTp[wcomb %in% c(1, 2 ^ p), w := 1] # Adjust weights for zero and full model

        nms <- colnames(Xtest)

        DTp[!(wcomb %in% c(1, 2 ^ p)), p_hat := pred_vector(model = model, data = .SD), .SDcols = nms]
        if(nrow(Xtest)==1){
            DTp[wcomb == 2 ^ p, p_hat := pred_vector(model = model, data = as.data.frame(rbind(Xtest,Xtest)))[1]] # Just a hack for a single prediction
        } else {
            DTp[wcomb == 2 ^ p, p_hat := pred_vector(model = model, data = as.data.frame(as.matrix(l$Xtest)[i, , drop = FALSE]))]
        }
        DTp[wcomb == 1, p_hat := pred_zero]


        DTres <- DTp[, .(k = sum((p_hat * w) / sum(w))), wcomb]
        setkey(DTres, wcomb)
        ll[[i]] <- DTres
        ll[[i]][, id := i]
    }

    DT <- rbindlist(ll)

    Kshap <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
    for (i in l$Xtest[, .I]) {
        Kshap[i, ] = l$W %*% DT[id == i, k]
    }
    Shapley.true = list(exactShap = Kshap, other_objects = list(ll = ll, DT = DT))

}
