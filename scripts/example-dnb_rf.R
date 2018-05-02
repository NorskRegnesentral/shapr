rm(list = ls())

library(semiArtificial)
library(data.table)
library(ranger)

data_dir <- "/nr/project/stat/BFFGB18/LIME/lime/R/"
train6 <- as.data.table(read.table(paste0(data_dir, "/train6.csv"), sep = ";", header = TRUE))
test6 <- as.data.table(read.table(paste0(data_dir, "/test6.csv"), sep = ";", header = TRUE))
train6 <- train6[, .SD, .SDcols = 1:7]
test6 <- test6[, .SD, .SDcols = 1:7]

model6 <- ranger(as.factor(default) ~ .,
				 data = train6,
				 num.trees = 500,
				 num.threads = 1,
				 verbose = TRUE,
				 probability = TRUE,
				 importance = "impurity",
				 mtry = sqrt(ncol(train6) - 1))

model <- model6
p_default <- train6[, mean(default)]
nms <- colnames(train6)[-1]
trainData <- train6[, .SD, .SDcols = nms]
testData <- test6[, .SD, .SDcols = nms]
setcolorder(testData, nms)
m <- ncol(trainData)

X <- kernelShap(
	m = m,
	trainData = trainData,
	testData = head(testData, 50),
	nSamples = 20,
	p_default = p_default,
	exact = TRUE,
	nRows = 1e3,
	sigma = 0.3,
	scale = FALSE,
	model = model
)
