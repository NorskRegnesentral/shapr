
library(tidyverse)
library(data.table)
library(shapr)


Data_O <- data.table::fread(file = "https://github.com/user-attachments/files/17777841/synthetic_data.csv")
# Remove rows with missing values
Data_O <- Data_O[complete.cases(Data_O),]
# Handle extremes of target
Data_O <- Data_O %>% filter(actief_in_inst_2022_SCH > 0.60)
Data_O$actief_in_inst_2022_SCH <- sqrt(Data_O$actief_in_inst_2022_SCH)

# Features
check <- as.data.frame(model.matrix(~., data = Data_O[, c(3, 32:36, 38, 55:68)]))
check[] <- lapply(check, as.numeric)
check <- as.matrix(check)
check <- check[, -1]

# Outcome variable
y <- as.numeric(Data_O$actief_in_inst_2022_SCH)

# Split dataset into training (70%) and test (30%) sets
samp <- sample(nrow(Data_O), 0.7 * nrow(Data_O))

Train1 <- check[samp, ]
Train1 <- as.data.frame(Train1)

Test1 <- check[-samp, ]
Test1 <- as.data.frame(Test1)

Y_train <- y[samp]
Y_test <- y[-samp]

# Train Random Forest model
rf.fit <- ranger::ranger(Y_train ~ .,
                         data = Train1,
                         mtry = 14,
                         max.depth = 3,
                         replace = FALSE,
                         min.node.size = 40,
                         sample.fraction = 0.8,
                         respect.unordered.factors = "order",
                         importance = "permutation")

# SHAPR
p <- mean(Y_train)
library(shapr)

progressr::handlers(global = TRUE)
explanation <- shapr::explain(
  rf.fit,
  Test1,
  Train1,
  approach = "gaussian",
  max_n_coalitions = 20,
  iterative_args = list(initial_n_coalitions=20),
  phi0 = p
)

library(ggplot2)
library(ggbeeswarm)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  plot(explanation, plot_type = "scatter")
  plot(explanation, plot_type = "beeswarm")
}

saveRDS(explanation, "explanation.rds")



explanation <- readRDS("explanation.rds")

plot(explanation, plot_type = "beeswarm", corral = "wrap")


























tmp_list <-   plot_shapr(explanation, plot_type = "beeswarm")


gg_old <- make_beeswarm_plot_old(dt_plot = tmp_list$dt_plot,
                                 col = tmp_list$col,
                                 index_x_explain = tmp_list$index_x_explain,
                                 x = tmp_list$x,
                                 factor_cols = tmp_list$factor_features)

gg_new_cex <- make_beeswarm_plot_new_cex(dt_plot = tmp_list$dt_plot,
                                         col = tmp_list$col,
                                         index_x_explain = tmp_list$index_x_explain,
                                         x = tmp_list$x,
                                         factor_cols = tmp_list$factor_features)

gg_new <- make_beeswarm_plot_new(dt_plot = tmp_list$dt_plot,
                                 col = tmp_list$col,
                                 index_x_explain = tmp_list$index_x_explain,
                                 x = tmp_list$x,
                                 factor_cols = tmp_list$factor_features,
                                 corral.corral  = "wrap", # Default. Other options: "none" (default in geom_beeswarm), "gutter", "random", "omit"
                                 corral.method = "swarm", # Default (and default in geom_beeswarm). Other options: "compactswarm", "hex", "square", "center
                                 corral.priority = "random", # Default . Other options: "ascending" (default in geom_beeswarm), "descending", "density"
                                 corral.width = 0.75, # Default. 0.9 is default in geom_beeswarm
                                 corral.cex = 0.75) # Default. 1 is default in geom_beeswarm

gg_paper3 <- make_beeswarm_plot_paper3(dt_plot = tmp_list$dt_plot,
                                       col = tmp_list$col,
                                       index_x_explain = tmp_list$index_x_explain,
                                       x = tmp_list$x,
                                       factor_cols = tmp_list$factor_features)

ggpubr::ggarrange(gg_old, gg_new_cex, gg_new, gg_paper3, labels = c("Old", "New_cex", "New", "Paper3"), nrow = 1, vjust = 2)
