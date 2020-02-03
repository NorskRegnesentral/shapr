
### Testing grouping with DNB data

library(data.table)

train6 <- fread("/nr/project_stat/BFFGB18/LIME/lime/R/train6.csv")
test6 <- fread("/nr/project_stat/BFFGB18/LIME/lime/R/test6.csv")

train6 <- train6[,c(1:3,6:13)]
test6  <- test6[,c(1:3,6:13)]

trainData <- train6[,-1]
testData  <-  test6[1:10,-1]
corMat0 <- cor(trainData)




