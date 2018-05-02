rm(list = ls())

library(semiArtificial)
library(data.table)
library(shapr)


## Create training data ------------
data(iris)
iris <- as.data.table(iris)
iris <- iris[Species != "setosa"]
iris <- iris[, Species := as.factor(Species)]

str_formula <- "Species ~ ."
iris_generator <- rbfDataGen(formula = as.formula(str_formula), data = iris)
iris_mod <- newdata(object = iris_generator, size = 10000)
iris_mod <- as.data.table(iris_mod)

## Train models ------------
formula_list <- list(
    "Species ~ 1",
    "Species ~ Sepal.Length",
    "Species ~ Sepal.Width",
    "Species ~ Petal.Width",
    "Species ~ Sepal.Length + Sepal.Width",
    "Species ~ Sepal.Length + Petal.Width",
    "Species ~ Petal.Width + Sepal.Width",
    "Species ~ Sepal.Length + Petal.Width + Sepal.Width"
)

for (i in seq_along(formula_list)) {

    str_formula <- formula_list[[i]]
    mod <- glm(formula = as.formula(str_formula), data = iris_mod, family = binomial)
    assign(x = paste0("mod", i - 1), value = mod)
}

## Split into training and test data ------------
set.seed(1)
cnms <- c(1, 2, 4)
size <- 100
test_ind <- sample(x = iris_mod[, .I], size = size)
iris_mod[, train := TRUE][test_ind, train := FALSE]
testData <- iris_mod[train == FALSE][, .SD, .SDcols = cnms]
trainData <- iris_mod[train == TRUE][, .SD, .SDcols = cnms]

m <- length(cnms)
exact <- TRUE
sigma <- 1.75 * (trainData[, .N]) ^ (-1 / 6)
X <- get_combinations(m = m, exact = exact)
X <- get_weights(X)
W <- get_weighted_matrix(X)
dist_mat <- distance_cpp(
    comb = X[["comb"]],
    train = trainData,
    test = testData,
    ncomb = X[, .N],
    sigma = sigma
)

X <- kernelShap(
	m = m,
	trainData = trainData,
	testData = testData,
	nSamples = 200,
	p_default = .5,
	exact = TRUE,
	nRows = 1e3,
	sigma = 0,
	model = mod5
)
