
### Script for fictive explanation

library(lime)
library(MASS)
iris_test <- iris[1, 1:4]
iris_train <- iris[-1, 1:4]
iris_lab <- iris[[5]][-1]
model <- lda(iris_train, iris_lab)
explanation <- lime(iris_train, model)
df <- as.data.frame(explain(iris_test, explanation, n_labels = 1, n_features = 4))

df[5,] <- df[4,]
df$label <- "Misligholdt lån"
df$case <-df$model_r2 <- ""
df$label_prob <- 0.7
df$model_prediction <- 0.7
df$feature_weight <- c(0.45,0.35,0.1,-0.05,-0.2)
df$feature_desc <- c("Antall inkassokrav = 4","Volatilitet brukskonto = 10", "Alder = 40", "Kjønn = Mann", "Sivilstatus = Gift")
lime::plot_features(df)

