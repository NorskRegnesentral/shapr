library(xgboost)
library(shapr)
library(ggplot2)
library(data.table)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

# Looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)
p <- mean(y_train)

# Prepare the data for explanation
res <- explain_final(x_train,x_test,model,approach="independence",prediction_zero=p,n_batches = 4)
plot(res)

i<- 1 # index for observation we want to plot
dt <- data.table(feat_name = paste0(colnames(res$shapley_values[,-1]), " = ", format(res$internal$data$x_explain[i,], 2) ),
                 shapley_value = as.numeric(res$shapley_values[i,-1])
                 )
dt
expected <- as.numeric(res$shapley_values[i,])[1]
observed <- res$pred_explain[i]

dt[, sign := ifelse(shapley_value > 0, "Increases", "Decreases")]
dt[, rank := frank(abs(shapley_value))]
setorder(dt, rank)
dt[, end := cumsum(shapley_value)+expected]
dt[, start := c(expected, head(end, -1))]
dt[, description := factor(feat_name, levels = unique(feat_name[order(abs(shapley_value))]))]
dt

p <- ggplot(dt, aes(x = description, fill = sign)) +
  geom_rect(aes(x=description, xmin = rank - 0.45, xmax = rank + 0.45, ymin = end,ymax = start)) +
  scale_fill_manual(values=c("steelblue", "lightsteelblue")) +
  geom_segment(x=-0.1, xend = 0.56, y=expected, yend=expected, linetype="dashed", col="dark grey") +
  labs(
    y = "Feature contribution",
    x = "Feature",
    fill = "",
    title = "Shapley value prediction explanation"
  ) +
  geom_text(aes(label = format(shapley_value,digits=2), x=rank, y=start + (end-start)/2)) +
  annotate("text",label=paste0("E(italic(f(x)))==", format(expected,digits=3)), y=expected, x=-Inf,parse = TRUE) +
  coord_flip(clip = 'off', xlim=c(0.5, 4)) +
  theme(plot.margin = unit(c(1,1,3,1), "lines")) +
  geom_segment(x=-0.1, xend = 4.46, y=observed, yend=observed, linetype="dashed", col="dark grey") +
  annotate("text",label=paste0("italic(f(x))==", format(observed,digits=3)), y=observed, x=Inf, parse = TRUE) +
  geom_segment(aes(x=ifelse(rank==last(rank), as.numeric(rank), as.numeric(rank)-0.45), xend = ifelse(rank==last(rank), as.numeric(rank), as.numeric(rank)+1.45),
                   y=end, yend=end), linetype="dashed", col="dark grey")
p



