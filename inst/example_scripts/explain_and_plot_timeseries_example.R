

# Setup
Sys.setenv(RETICULATE_PYTHON = "~/python/python_3.6/.venv/bin/python3.6")

# Make sure the martin/timeseries branch is checked out in the below git location
devtools::load_all("~/Dropbox/Local_work/Git/shapr/")
library(data.table)
library(reticulate)
library(keras)
library(DNBKeras)
library(ggplot2)

py_config()


# Reading data and checking that it all works
testData0 <- DNBKeras::load_data(data_dir = "~/nr/project_stat/BI_PersonalisedMarketing/Explanations/Martin/groupShapley-paper/time_series_data/", type = "test")
testDatacsv <- data.table::fread("~/nr/project_stat/BI_PersonalisedMarketing/Explanations/Martin/groupShapley-paper/time_series_data/cnn_test.csv")

model <- keras::load_model_hdf5(filepath = "~/nr/project_stat/BI_PersonalisedMarketing/Explanations/Martin/groupShapley-paper/time_series_data/cnn6.h5")

x_all <- as.data.table(testData0$X6[,,1])
y_all <- testData0$y[,2]

#these_test <- c(head(which(y_all==1)),head(which(y_all==0)))

#x_test <- x_all[these_test]
#y_test <- y_all[these_test]
#x_train <- x_all[-these_test]
#y_train <- y_all[-these_test]

predict_model.keras.engine.sequential.Sequential <- function(x,newdata){
  array_newdata <- array(as.matrix(newdata),dim = c(dim(newdata),1))
  predict(x,array_newdata)[,2]
}

all.equal(predict_model(model,x_all),predict(model,testData0$X6)[,2]) # TRUE

all_pred <- predict_model(model,x_all)
pROC::auc(y_all, all_pred)
#Area under the curve: 0.8665 OK, so prediction is good here
mean((all_pred-y_all)^2)
#[1] 0.04377978
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


x_explain_final <- fread("~/nr/project_stat/BI_PersonalisedMarketing/Explanations/Martin/groupShapley-paper/time_series_data/non-sensitive/x_explain_new_fixed_csv.csv",dec = ",")
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
  #ggtitle("Time series plots")+
  xlab("Days")+ylab("Balance (scaled)")




these_test <- as.numeric(names(x_explain_final)[-1])

x_test <- x_all[these_test]
y_test <- y_all[these_test]
x_train <- x_all[-these_test]
y_train <- y_all[-these_test]


explainer <- shapr(head(x_all,1000), model,group = group)


explanation_final2 <- explain(
  x_explain_final2[,.SD,.SDcols=paste0("V",1:365)],
  approach = "timeseries",
  explainer = explainer,
  prediction_zero = mean(head(y_all,1000)),
  fixed_sigma = 2 # original 2
)

gg_explain=plot(explanation_final2,plot_phi0 = F,feature_order = 4:1)
gg_explain


size = 7 # this is a good size for the paper
theme_set(theme_bw()) # this makes a white background
p1 = plot(explanation_final2,plot_phi0 = F,feature_order = 4:1,digits = 2,include_title = F) +
  ggplot2::facet_wrap(~header,  labeller = "label_value", ncol = 2) + # scales = "free_x",
  ggplot2::theme(
    legend.text = element_text(size = size),
    legend.title = element_text(size = size),
    axis.text = element_text(size = size),
    axis.text.y = element_text(size = size),
    axis.title = element_text(size = size),
    strip.text = element_text(size = size)
  )+xlab("Feature group")+ylab("groupShapley value")+
  theme(plot.margin=unit(c(0,0,0,0),"cm"))
p1
#
ggsave( # new because the old figure is just IDs = c(1, 2, 3, 4)
  "time-series_explanations_new2.png",
  plot = p1,
  device = 'png',
  path = 'plots',
  scale = 1.1,
  width = 13,
  height = 6,
  units = "cm"
)
gg_ts_save <- gg_ts+  ggplot2::theme(
  legend.text = element_text(size = size),
  legend.title = element_text(size = size),
  axis.text = element_text(size = size),
  axis.text.y = element_text(size = size),
  axis.title = element_text(size = size),
  strip.text = element_text(size = size)
)+theme(plot.margin=unit(c(0,0,0,0),"cm"))
ggsave( # new because the old figure is just IDs = c(1, 2, 3, 4)
  "time-series_plots_new2.png",
  plot = gg_ts_save,
  device = 'png',
  path = 'plots',
  scale = 1,
  width = 13,
  height = 7.5,
  units = "cm"
)


