test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


#
library(data.table)

data <- data.table::as.data.table(airquality)
data_complete <- data[complete.cases(airquality),]
data_complete[,factorMonth:=as.factor(Month)]

