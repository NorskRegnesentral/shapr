
# Defaulting time series

# Setup
Sys.setenv(RETICULATE_PYTHON = "~/python/python_3.6/.venv/bin/python3.6")
#library(shapr)
devtools::load_all()
library(data.table)
library(reticulate)
library(keras)
library(DNBKeras)
py_config()


# Reading data and checking that it all works
testData0 <- DNBKeras::load_data(data_dir = "~/nr/project_stat/BI_PersonalisedMarketing/Explanations/Martin", type = "test")
testDatacsv <- data.table::fread("~/nr/project_stat/BI_PersonalisedMarketing/Explanations/Martin/cnn_test.csv")

model <- keras::load_model_hdf5(filepath = "~/nr/project_stat/BI_PersonalisedMarketing/Explanations/Martin/cnn6.h5")

x_all <- as.data.table(testData0$X6[,,1])
y_all <- testData0$y[,2]



these_test <- c(head(which(y_all==1)),head(which(y_all==0)))

x_test <- x_all[these_test]
y_test <- y_all[these_test]
x_train <- x_all[-these_test]
y_train <- y_all[-these_test]


predict_model.keras.engine.sequential.Sequential <- function(x,newdata){
  array_newdata <- array(as.matrix(newdata),dim = c(dim(newdata),1))
  predict(x,array_newdata)[,2]
}

all.equal(predict_model(model,head(x_test)),head(predict(model,testData0$X6)[,2])) # TRUE

all_pred <- predict_model(model,x_all)
pROC::auc(y_all, all_pred)
#Area under the curve: 0.8665 OK, so prediction is good here
######################################

Q1_days <- 1:(31+28+31)
Q2_days <- 1:(30+31+30)+max(Q1_days)
Q3_days <- 1:(31+31+30)+max(Q2_days)
Q4_days <- 1:(31+30+31)+max(Q3_days)


group <- list(Q1=paste0("V",Q1_days),
              Q2=paste0("V",Q2_days),
              Q3=paste0("V",Q3_days),
              Q4=paste0("V",Q4_days))

# Prepare the data for explanation
explainer <- shapr(head(x_train,100), model,group = group)

explain.timeseries <- function(x, explainer, approach, prediction_zero,fixed_sigma = 0.1,
                               n_samples = 1e3, n_batches = 1, seed = 1, ...) {


  if (!is.null(seed)) set.seed(seed)

  # Add arguments to explainer object
  explainer$x_test <- as.matrix(preprocess_data(x, explainer$feature_list)$x_dt)
  explainer$approach <- approach
  explainer$n_samples <- n_samples
  explainer$fixed_sigma <- fixed_sigma


  r <- prepare_and_predict(explainer, n_batches, prediction_zero, ...)
}

#index_features <- NULL
#x <- explainer
#val <- t(t(-0.5 * D) / h_optim_vec^2)
#W_kernel <- exp(val)

prepare_data.timeseries <- function(x, index_features = NULL, ...) {
  id <- id_combination <- w <- NULL # due to NSE notes in R CMD check

  if (is.null(index_features)) {
    index_features <- x$X[, .I]
  }

  S <- x$S[index_features, , drop = FALSE]
  x_train <- as.matrix(x$x_train)

  n_col <- nrow(x$x_test)

  dt_l <- list()

  for (i in seq(n_col)) {
    x_test <- x$x_test[i, , drop = FALSE]
    #x_test <- x$x_test[i, , drop = TRUE]
    dt_l[[i]] <- list()
    tmp <- list()
    tmp[[1]] <- as.data.table(x_test)
    tmp[[1]][,w:=1]
    tmp[[nrow(S)]] <- as.data.table(x_test)
    tmp[[nrow(S)]][,w:=1]

    for(j in 2:(nrow(S)-1)){
      diff_S <- diff(c(1,S[j,],1))
      Sbar_starts <- which(diff_S==-1)
      Sbar_ends <- which(diff_S==1)-1
      cond_1 <- Sbar_starts-1
      cond_2 <- Sbar_ends+1
      cond_1[cond_1==0] <- cond_2[cond_1==0]
      cond_2[cond_2==(ncol(S)+1)] <- cond_1[cond_2==(ncol(S)+1)]
      len_Sbar_segment <- Sbar_ends-Sbar_starts+1

      Sbar_segments <- data.frame(Sbar_starts,Sbar_ends,cond_1,cond_2,len_Sbar_segment)
      tmp[[j]] <- matrix(rep(x_test,nrow(x_train)),nrow=nrow(x_train),byrow = T)

      w_vec <- exp(-0.5*rowSums((matrix(rep(x_test[S[j,]==0,drop=F],nrow(x_train)),nrow=nrow(x_train),byrow = T)-x_train[,S[j,]==0,drop=F])^2)/x$fixed_sigma^2)

      for(k in seq_len(nrow(Sbar_segments))){
        impute_these <- seq(Sbar_segments$Sbar_starts[k],Sbar_segments$Sbar_ends[k])

        x_test_cond_1 <- x_test[,Sbar_segments$cond_1[k]]
        x_test_cond_2 <- x_test[,Sbar_segments$cond_2[k]]

        x_train_starts <- x_train[,Sbar_segments$Sbar_starts[k]]
        x_train_ends <- x_train[,Sbar_segments$Sbar_ends[k]]

        a_test <- x_test_cond_1
        a_train <- x_train_starts

        b_test <- (x_test_cond_2-x_test_cond_1)/Sbar_segments$len_Sbar_segment[k]
        b_train <- (x_train_ends-x_train_starts)/Sbar_segments$len_Sbar_segment[k]

        lin_mod_test <- a_test+b_test*0:(Sbar_segments$len_Sbar_segment[k]-1)
        lin_mod_train <- a_train+b_train %o% (0:(Sbar_segments$len_Sbar_segment[k]-1))

        to_impute <- (x_train[,impute_these]-lin_mod_train)+matrix(rep(lin_mod_test,nrow(x_train)),nrow=nrow(x_train),byrow = T)
        tmp[[j]][,impute_these] <- pmax(pmin(to_impute,1),0)
      }
      tmp[[j]] <- as.data.table(tmp[[j]])
      tmp[[j]][,w:=w_vec/sum(w_vec)]
    }
    dt_l[[i]] <- rbindlist(tmp,idcol = "id_combination")
    #dt_l[[i]][, w := 1/.N,by=id_combination] # IS THIS NECESSARY?
    dt_l[[i]][, id := i]
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  return(dt)
}


explanation <- explain(
  x_test,
  approach = "timeseries",
  explainer = explainer,
  prediction_zero = mean(y_all),
  fixed_sigma = 2
)

rowSums(explanation$dt)
predict_model(model,x_test)

matplot(t(as.matrix(x_test[c(3,4),])),type = "l")

### Need to create some fictive people here

# I will do that by finding some people that look good in the data, shift the data, add some noise
# same value for all on the same level, shift the data a bit, and also manually change some numbers
# in excel. Should probably also modify the individuals such that they have such behavior within the quarters

#TODO: Consider adding the weighting to the functionality above

k <- 36
plot(unlist(x_all[which(y_all==1)][k,]),type="l")
predict_model(model,x_all[which(y_all==1)][k,])
# Usable k's:
#3 (0.29 in pred)
#4 (0.51)
# 13 (0.32)
# 16 (0.40)
# 25
# 36

#### CONTINUE HERE FINDING PEOPLE TO DO PLOTTING FOR



k <- 18
plot(unlist(x_all[which(y_all==0)][k,]),type="l")
predict_model(model,x_all[which(y_all==0)][k,])
#k = 3 (0.009)
#k = 7
#k = 8
# k = 9
# k = 14
zeros <- which(y_all==0)[c(3,7,8,9,14,17)]
ones <- which(y_all==1)[c(3,4,13,16,25,36)]

x_explain <- x_all[c(zeros,ones)]
x_explain[,id:=c(zeros,ones)]
x_explain[,y:=y_all[id]]

x_explain_org <- copy(x_explain)

melt_x_explain <- melt(x_explain,id.vars = c("id","y"))

melt_x_explain[,value_round:=round(value,1)]

set.seed(12345)
melt_x_explain[,a_id:=0*rnorm(1,mean=0,sd = 0.05),by=id]
melt_x_explain[,b_id:=rnorm(1,mean=0,sd = 0.1),by=id]

melt_x_explain[,a_value:=0*rnorm(1,mean=0,sd = 0.05),by=.(id,value_round)]
melt_x_explain[,b_value:=rnorm(1,mean=0,sd = 0.1),by=.(id,value_round)]

melt_x_explain[,value2:=a_id+a_value+value*(1+b_id+b_value)]
melt_x_explain[,value2:=pmax(pmin(value2,1),0)]

melt_x_explain[,variable_num:=as.numeric(substring(variable,first = 2))]

x_explain_new <- dcast(melt_x_explain[,.(variable,id,y,value2)],formula = y+id~variable,value.var = "value2")

x_explain_new[,.(y,predict_model(model,.SD)),.SDcols=paste0("V",1:365)]
x_explain_org[,.(y,predict_model(model,.SD)),.SDcols=paste0("V",1:365)]

library(ggplot2)
gg1 <- ggplot(melt_x_explain,aes(x=variable_num,y=value2,col=y))+geom_line()+facet_grid(vars(id))
gg2 <- ggplot(melt_x_explain,aes(x=variable_num,y=value,col=y))+geom_line()+facet_grid(vars(id))
library(patchwork)
gg1+gg2
x_explain_new_csv <- dcast(melt_x_explain[,.(variable_num,value2,id)],formula =variable_num~id ,value.var="value2")

#fwrite(x_explain_new_csv,"~/nr/project_stat/BI_PersonalisedMarketing/Explanations/Martin/x_explain_new_csv.csv")
x_explain_final <- fread("~/nr/project_stat/BI_PersonalisedMarketing/Explanations/Martin/x_explain_new_fixed_csv.csv",dec = ",")
names(x_explain_final)[-1] <- c(8,9,40,837)

melt_x_explain_final2 <- melt(x_explain_final,id.vars = "variable_num")
x_explain_final2 <- dcast(melt_x_explain_final2,formula = variable~variable_num,value.var="value")

names(x_explain_final2)[-1] <- paste0("V",1:365)
pred_vec <- x_explain_final2[,.(predict_model(model,.SD)),.SDcols=paste0("V",1:365)]
x_explain_final2

dt <- data.table(variable=x_explain_final2$variable,header=paste0("id: ",1:4,",  pred = ",unlist(round(pred_vec,2))))

dt_plot <- melt_x_explain_final2[dt,on="variable"]
library(ggplot2)
gg_ts <- ggplot(dt_plot,aes(x=variable_num,y=value))+
  geom_rect(aes(xmin = min(Q1_days), xmax = max(Q1_days), ymin = -Inf, ymax = Inf), fill="grey60",alpha=.9)+
  geom_rect(aes(xmin = min(Q2_days), xmax = max(Q2_days), ymin = -Inf, ymax = Inf), fill="grey70")+
  geom_rect(aes(xmin = min(Q3_days), xmax = max(Q3_days), ymin = -Inf, ymax = Inf), fill="grey80")+
  geom_rect(aes(xmin = min(Q4_days), xmax = max(Q4_days), ymin = -Inf, ymax = Inf), fill="grey90")+
  #  theme_bw()+
  geom_line()+facet_wrap(vars(header))+
  ggtitle("Time series plots")+xlab("Days")+ylab("Balance (scaled)")

explanation_final2 <- explain(
  x_explain_final2[,.SD,.SDcols=paste0("V",1:365)],
  approach = "timeseries",
  explainer = explainer,
  prediction_zero = mean(y_all),
  fixed_sigma = 2
)

gg_explain=plot(explanation_final2,plot_phi0 = F,feature_order = 4:1)
gg_explain

library(patchwork)
gg_ts/gg_explain


size = 7 # this is a good size for the paper
theme_set(theme_bw()) # this makes a white background
p1 = plot(explanation_final2,plot_phi0 = F,feature_order = 4:1) + ggtitle("") +
  ggplot2::facet_wrap(~header,  labeller = "label_value", ncol = 2) + # scales = "free_x",
  ggplot2::theme(
    legend.text = element_text(size = size),
    legend.title = element_text(size = size),
    axis.text = element_text(size = size),
    axis.text.y = element_text(size = size),
    axis.title = element_text(size = size),
    strip.text = element_text(size = size)
  )+xlab("Feature groups")+ylab("Feature group contributions")
p1
#
ggsave( # new because the old figure is just IDs = c(1, 2, 3, 4)
  "time-series_explanations.png",
  plot = p1,
  device = 'png',
  path = '~/nr/project_stat/BI_PersonalisedMarketing/Explanations/Martin',
  scale = 1,
  width = 12,
  height = 9,
  units = "cm"
)
gg_ts_save <- gg_ts+ggtitle("")+  ggplot2::theme(
  legend.text = element_text(size = size),
  legend.title = element_text(size = size),
  axis.text = element_text(size = size),
  axis.text.y = element_text(size = size),
  axis.title = element_text(size = size),
  strip.text = element_text(size = size)
)
ggsave( # new because the old figure is just IDs = c(1, 2, 3, 4)
  "time-series_plots.png",
  plot = gg_ts_save,
  device = 'png',
  path = '~/nr/project_stat/BI_PersonalisedMarketing/Explanations/Martin',
  scale = 1,
  width = 12,
  height = 9,
  units = "cm"
)



#### Natural explanation:
# id 1: Q3 increase prob, Q4 decrease # OK 1
# id 2: Q2 increase, Q4 decrease # OK 1
# id 3: Q2 dcrease, Q3+Q4 increase # Q2 not OK 1
# id 4: Q3 increase # Not OK 1


x_explain <- x_test[c(3,4),]
predict_model(model,x_explain)

aa = prepare_data.timeseries(explainer)

devtools::load_all()

