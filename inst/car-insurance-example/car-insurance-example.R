library(ranger)
library(rsample)
library(data.table) # need this to load after rsample so that you can use melt
library(yardstick)
library(purrr)
library(ggplot2)
library(mgcv)
library(rpart)
library(rpart.plot)
library(mice)
library(shapr)

# 1 load car insurance data

data = data.table::fread("inst/car-insurance-dataset/car_insurance_claim.csv")

features = c("KIDSDRIV", "AGE", "HOMEKIDS", "YOJ", "INCOME", "PARENT1", "HOME_VAL",
             "MSTATUS", "GENDER", "EDUCATION", "OCCUPATION", "TRAVTIME",
             "CAR_USE", "BLUEBOOK", "TIF", "CAR_TYPE", "RED_CAR", "OLDCLAIM",
             "CLM_FREQ", "REVOKED", "MVR_PTS", "CAR_AGE", "URBANICITY")
response = 'CLAIM_FLAG'

# YOJ = years on job
data$BIRTH = as.Date(format(as.Date(data$BIRTH, format = "%d%b%y"), "19%y-%m-%d"))

data$INCOME = as.numeric(gsub("[\\$,]", "", data$INCOME))
data$HOME_VAL = as.numeric(gsub("[\\$,]", "", data$HOME_VAL))
data$BLUEBOOK = as.numeric(gsub("[\\$,]", "", data$BLUEBOOK))
data$OLDCLAIM = as.numeric(gsub("[\\$,]", "", data$OLDCLAIM))
data$CLM_AMT = as.numeric(gsub("[\\$,]", "", data$CLM_AMT))

data$CLAIM_FLAG = factor(data$CLAIM_FLAG)
data$PARENT1 = factor(data$PARENT1)
data$MSTATUS = factor(data$MSTATUS)
data$GENDER = factor(data$GENDER)
data$EDUCATION = factor(data$EDUCATION)
data$OCCUPATION = factor(data$OCCUPATION)
data$CAR_USE = factor(data$CAR_USE)
data$CAR_TYPE = factor(data$CAR_TYPE)
data$RED_CAR = factor(data$RED_CAR)
data$REVOKED = factor(data$REVOKED)
data$URBANICITY = factor(data$URBANICITY)

# 2 fit GLM on all data
formula = as.formula(paste("CLAIM_FLAG ~", paste(features, collapse = "+")))
model = glm(formula = formula, data = data, family = 'binomial')
summary(model)

# 3 create list of features + response
x_y = c("CLAIM_FLAG", "KIDSDRIV", "AGE", "HOMEKIDS", "YOJ", "INCOME", "PARENT1", "HOME_VAL",
        "MSTATUS", "GENDER", "EDUCATION", "OCCUPATION", "TRAVTIME",
        "CAR_USE", "BLUEBOOK", "TIF", "CAR_TYPE", "RED_CAR", "OLDCLAIM",
        "CLM_FREQ", "REVOKED", "MVR_PTS",  "CAR_AGE", "URBANICITY")
# remove "CLM_AMT",

# 4 do multiple imputation on the 5 features that have missing data
# this takes a few minutes
imputed_Data <- mice(data[, ..x_y], m = 5, maxit = 10, method = 'pmm', seed = 500)
# summary(imputed_Data)
imputed_dt = data.table(complete(imputed_Data, 2))

# 5 do 10-fold cross-validation with random forest model
set.seed(1)
CV = vfold_cv(data = imputed_dt[, ..x_y], v = 10, repeats = 1)

holdout_ranger_AUC <- function(splits, mod_form) {
  mod <- ranger(mod_form, data = analysis(splits),  probability = TRUE, importance = "impurity")
  # View(mod$variable.importance)
  holdout <- assessment(splits)
  res <- data.table("Truth" = holdout$CLAIM_FLAG)
  res$predicted_probability <- predict(mod, data = holdout)$predictions[, 2]
  # Calculate AUC
  res$auc <- rep(roc_auc(res, "Truth", "predicted_probability")$.estimate, nrow(res))

  cutoffs <- c(0.1, 0.2, 0.3, 0.5, 0.6, 0.7)
  for(j in cutoffs){
    res[, predicted_class := predicted_probability >= j]
    res[, predicted_class := as.factor(as.numeric(predicted_class))]
    p <- yardstick::precision_vec(truth = res$Truth, estimate = res$predicted_class)
    r <- yardstick::recall_vec(truth = res$Truth, estimate = res$predicted_class)
    f1 <- (2 * p * r) / (p + r)
    res[, paste0("f1_", j) := f1]

  }
  return(res)
}

CV$results <- map(CV$splits,
                  holdout_ranger_AUC,
                  formula)
CV$auc <- map_dbl(CV$results, function(x) mean(x$auc))
CV$f1 <- map_dbl(CV$results, function(x) mean(x$f1_0.5))
CV$predicted_probability <- map_dbl(CV$results, function(x) mean(x$predicted_probability))
dt <- data.frame(fold = 1:10,  prob_claim = CV$predicted_probability,  auc = CV$auc, f1 = CV$f1, Model = rep("All", 10))

dt$Model <- factor(dt$Model)
ggplot(dt, aes(as.factor(fold), auc, color = Model, group = Model)) + theme_bw() + geom_point(stat='summary', fun=sum) +
  stat_summary(fun=sum, geom="line") +
  xlab("Fold") +
  ylab("Out of sample AUC") +
  ggtitle("AUC on test set using random forest") +
  ylim(0.25, 1)

dt = data.table(dt)
dt[, mean(auc)] # 0.8145945 this is what is in the paper
dt[, mean(f1)] # 0.8682297 this is what is in the paper


# 6 fit groupshap
group1 <- c("AGE", "EDUCATION", "HOMEKIDS", "HOME_VAL", "OCCUPATION", "TRAVTIME",
            "KIDSDRIV", "MSTATUS", "PARENT1", "GENDER", "URBANICITY", "YOJ") # personal
group2 <- c("BLUEBOOK", "CAR_AGE", "CAR_TYPE", "RED_CAR") # car
group3 <- c("CLM_FREQ", "MVR_PTS", "REVOKED", "TIF")# customer info

x_train = analysis(CV$splits[[1]])
x_test = assessment(CV$splits[[1]])

group_1_ = paste(group1, collapse = "+")
group_2_ = paste(group2, collapse = "+")
group_3_ = paste(group3, collapse = "+")
temp = paste(paste(group_1_, group_2_, sep = "+"), group_3_, sep = "+")
formula2 = as.formula(paste0("CLAIM_FLAG ~ ", temp))

# 7 fit one random forest model to 9/10th of the data
model = ranger(formula2, data = x_train,  probability = TRUE, importance = "impurity")

p <- mean(as.numeric(as.character(x_train$CLAIM_FLAG)))
group_names = list(Personal_Info = group1, Car_Info = group2, Track_Record = group3)

# 8 estimate Shapley values for the three groups
explainer_group <- shapr(x_train, model, group = group_names)
explanation_group <- explain(
  x_test[1:50,],
  approach = "ctree",
  explainer = explainer_group,
  prediction_zero = p
)

# 9 for each individual, rank the Shapley values
explanation_mat = as.matrix(abs(explanation_group$dt[, c("Personal_Info", "Car_Info", "Track_Record")]))
explanation_ranking = t(apply(-explanation_mat, FUN = rank, 1))

# 10 find individuals to describe in paper
IDs = c(1, 2, 17, 4)
x_test[IDs,]

explanation_group$dt = round(explanation_group$dt[IDs,], 2)
explanation_group$p = round(explanation_group$p[IDs], 2)

saveRDS(explanation_group,"~/nr/project_stat/BI_PersonalisedMarketing/Explanations/Martin/car_insurance_explanations.rds")

size = 7 # this is a good size for the paper
theme_set(theme_bw()) # this makes a white background
p1 = plot(explanation_group, plot_phi0 = F) + ggtitle("") +
  ggplot2::facet_wrap(~header,  labeller = "label_value", ncol = 2) + # scales = "free_x",
  ggplot2::theme(
    legend.text = element_text(size = size),
    legend.title = element_text(size = size),
    axis.text = element_text(size = size),
    axis.text.y = element_text(size = size),
    axis.title = element_text(size = size),
    strip.text = element_text(size = size)
  )
p1
#
ggsave( # new because the old figure is just IDs = c(1, 2, 3, 4)
  "car-insurance-glm-3-groups-new.png",
  plot = p1,
  device = 'png',
  path = 'inst/car-insurance-example/',
  scale = 1,
  width = 9,
  height = 9,
  units = "cm"
  )



