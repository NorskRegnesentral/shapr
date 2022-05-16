test_that("Initial test2", {

  tmp <- data.table::data.table(a=1,b=2)

  expect_snapshot_value(tmp,style = "json",ignore_attr=T)
  #expect_snapshot_value(list(a=c("1",2,3)),style = "json2")
   #expect_snapshot_value(list(a=c("1",2,3),
  #                            b="d"),style = "json2")
  # expect_snapshot_value(list(a=c("1",2,3),
  #                            b="d",
  #                            c=matrix(0,2,2)),style = "json2")
  # expect_snapshot_value(list(a=c("1",2,3),
  #                            b="d",
  #                            c=matrix(0,2,2),
  #                            d=data.table(a=1:3,b=3:5)),style = "json2")


})
