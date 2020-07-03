library(xgboost)
library(shapr)
library(data.table)
library(party)
library(ggplot2)

if(.Platform$OS.type=="windows"){
  projDir <- "M:"
} else {
  projDir <- "/nr/project/stat"
}

## ------------- some functions ----------------------
check_for_cont <- function(col_ind, data){
  return(!is.factor(data[, col_ind]))
}

check_for_col_NA <- function(col_ind, data){
  sum_NA <- sum(is.na(data[, col_ind]))
  if(sum_NA > 0) return(FALSE)
  else return(TRUE)
}

check_for_row_NA <- function(row_ind, data){
  sum_NA <- sum(is.na(data[row_ind, ]))
  if(sum_NA > 0) return(FALSE)
  else return(TRUE)
}

## ------------------------- data ------------------------

train_data <- read.table(file = paste(projDir, "BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/data", "train.csv", sep = "/"), sep = ",", header = TRUE,
                         stringsAsFactors = TRUE)
test_data <- read.table(file = paste(projDir, "BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/data", "test.csv", sep = "/"), sep = ",", header = TRUE, stringsAsFactors = TRUE)


## first we remove the columns with NA
train_noNA <- train_data[, sapply(X = 1:ncol(train_data), FUN = check_for_col_NA, data = train_data)]

## then we keep only the columns from the train data
test_noNA0 <- test_data[, names(train_noNA)[-length(train_noNA)]]

## then we remove the rows with at least one NA
test_noNA <- test_noNA0[sapply(X = 1:nrow(test_noNA0), FUN = check_for_row_NA, data = test_noNA0) ,]


x_var <- c("MSSubClass", "YearBuilt", "LotArea", "YearRemodAdd", "GarageArea")
y_var <- "SalePrice"

x_train <- train_noNA[, x_var]
x_test <- test_noNA[, x_var]

y_train <- train_noNA[, y_var]
y_train <- round(y_train / 1000) ## convert to millions

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 50,
  verbose = FALSE
)


explainer <- shapr(x_train, model)

p <- mean(y_train)

set.seed(9)
explanation <- explain(
  x = as.data.table(x_test)[sample.int(n = nrow(x_test), size = 2),],
  approach = 'ctree',
  explainer = explainer,
  prediction_zero = p,
  sample = FALSE)


names(explanation$dt) <- c("none", "Age", "Gender", "Type of car", "# of accidents", "Time since registration")

names(explanation$x_test) <- c("Age", "Gender", "Type of car", "# of accidents", "Time since registration")

explanation$x_test$Age <- c(55, 30)
explanation$x_test$Gender <- c("Woman", "Man")
explanation$x_test$`Type of car` <- c("Buick", "Porsche")
explanation$x_test$`# of accidents` <- c("3", "1")
explanation$x_test$`Time since registration` <- c("3.2", "1.5")

print(explanation$dt)

# explanation$dt[2, ] <- -1*explanation$dt[2, ]

x1 <- c(120.1, -100.2, -160.8, 22.8, 250.54, -8.9)
sum(x1)
x2 <- c(120.1, -40.8, 130.78, 80.8, -20.5, -40.5)
sum(x2)

explanation$dt$none <- c(x1[1], x2[1])
explanation$dt$Age <- c(x1[2], x2[2])
explanation$dt$Gender <- c(x1[3], x2[3])
explanation$dt$`Type of car` <- c(x1[4], x2[4])
explanation$dt$`# of accidents` <- c(x1[5], x2[5])
explanation$dt$`Time since registration` <- c(x1[6], x2[6])

explanation$p <- c(sum(x1), sum(x2))


## -------------------------


plot_shapr_Annabelle <- function(x,
                                 digits = 3,
                                 plot_phi0 = TRUE,
                                 index_x_test = NULL,
                                 top_k_features = NULL,
                                 ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  }

  if (is.null(index_x_test)) index_x_test <- seq(nrow(x$x_test))
  if (is.null(top_k_features)) top_k_features <- ncol(x$x_test) + 1
  id <- phi <- NULL # due to NSE notes in R CMD check

  # melting Kshap
  cnms <- colnames(x$x_test)
  KshapDT <- data.table::copy(x$dt)
  KshapDT[, id := .I]
  meltKshap <- data.table::melt(KshapDT, id.vars = "id", value.name = "phi")
  meltKshap[, sign := factor(sign(phi), levels = c(1, -1), labels = c("Increases", "Decreases"))]

  # Converting and melting Xtest
  desc_mat <- format(x$x_test, digits = digits)
  for (i in 1:ncol(desc_mat)) {
    desc_mat[, i] <- paste0(cnms[i], " = ", desc_mat[, i])
  }
  desc_dt <- data.table::as.data.table(cbind(none = "none", desc_mat))
  melt_desc_dt <- data.table::melt(desc_dt[, id := .I], id.vars = "id", value.name = "description")

  # Data table for plotting
  plotting_dt <- merge(meltKshap, melt_desc_dt)

  # Adding the predictions
  predDT <- data.table::data.table(id = KshapDT$id, pred = x$p)
  plotting_dt <- merge(plotting_dt, predDT, by = "id")

  # Adding header for each individual plot
  header <- variable <- pred <- description <- NULL # due to NSE notes in R CMD check
  plotting_dt[, header := paste0("id: ", id, ", pred = ", format(pred, digits = digits + 1))]

  if (!plot_phi0) {
    plotting_dt <- plotting_dt[variable != "none"]
  }
  plotting_dt <- plotting_dt[id %in% index_x_test]
  plotting_dt[, rank := data.table::frank(-abs(phi)), by = "id"]
  plotting_dt <- plotting_dt[rank <= top_k_features]
  plotting_dt[, description := factor(description, levels = unique(description[order(abs(phi))]))]

  # Plotting
  gg <- ggplot2::ggplot(plotting_dt) +
    ggplot2::facet_wrap(~header, scales = "free_y", labeller = "label_value", ncol = 2) +
    ggplot2::geom_col(ggplot2::aes(x = description, y = phi, fill = sign)) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("steelblue", "lightsteelblue"), drop = TRUE) +
    ggplot2::labs(
      y = "Feature contribution",
      x = "Feature",
      fill = "",
      title = "Shapley value prediction explanation"
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 6),
      axis.title=element_text(size = 5), # for axis titles
      axis.text=element_text(size = 5), # for axis labels
      legend.text=element_text(size = 5),
      strip.text.x = element_text(size = 5)
    )

  return(gg)
}

## ---------------------------------
p0 <- plot_shapr_Annabelle(x = explanation, plot_phi0 = TRUE) ## removes the 'none' possibility, index_x_test =1

ggsave("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/ctree-paper/figures/example_carinsurance2.png",
       plot = p0, device = NULL, path = NULL,
       scale = 1, width = 12, height = 5.5, units = "cm",
       dpi = 300, limitsize = TRUE)


ggsave("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/ctree-paper/figures/example_carinsurance2.pdf",
       plot = p0,
       device = "pdf",
       scale = 1,
       width = 12,
       height = 5.5,
       units = "cm",
       dpi = 300,
       limitsize = TRUE)



ggsave("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/ctree-paper/CD_make_paper/Final_zip_version_Redelmeier/figures/example_carinsurance2.pdf",
       plot = p0,
       device = "pdf",
       scale = 1,
       width = 12,
       height = 5.5,
       units = "cm",
       dpi = 300,
       limitsize = TRUE)



