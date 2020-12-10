library(shapr)
library(testthat)

test_that("Test get_supported_models", {


  print(methods(model_type)) # This should list all model_type classes.

  model_type.test <- function() 1

  print(methods(model_type)) # This should list model.type.test as defined above, but it does not when ran as a test

  # .S3methods are called by methods(). Trying to provide differnet envir to that function
  env1 <- parent.frame(1)
  env2 <- sys.frame()
  env3 <- sys.parent()#
  env4 <- parent.frame(1)
  env5 <- .GlobalEnv

  print(.S3methods(model_type, envir=env1))
  print(.S3methods(model_type, envir=env2))
  print(.S3methods(model_type, envir=env3))
  print(.S3methods(model_type, envir=env4))
  print(.S3methods(model_type, envir=env5))


  # This is what I really want to work

  rm(model_type.test)

  org_models <- get_supported_models()

  print(org_models)

  model_type.test <- function() 1


  new_models <- get_supported_models() # test should be added here

  print(new_models)

  expect_equal(nrow(org_models)+1,nrow(new_models)) # This fails, it should not if test is found for model_type



  ### OLD STUFF BELOW HERE #####

  # # Simpler version which goes directly to he point
  #
  # # Here I just try to print some stuff in here to understand how the environments work
  #
  # print(sys.parent())
  # print(parent.frame(n = 10))
  # print(sys.frame())
  # print(sys.parent())
  # env <- parent.frame(1)#sys.parent()#sys.frame()
  # print(.S3methods(model_type, envir=env))
  # model_type.test <- function() 1
  #
  # print(.S3methods(model_type, envir=env))
  # #methods2(model_type)

  #  print(methods(model_type))
  #  print(methods2(model_type))

  # no_native_methods <- length(methods(model_type))
  #
  # org_models <- get_supported_models()
  #
  # print(org_models)
  #
  # model_type.test <- function() 1
  #
  # new_models <- get_supported_models()
  #
  # print(new_models)
  #
  # expect_equal(nrow(org_models),no_native_methods)
  #
  # expect_equal(nrow(org_models)+1,nrow(new_models))

})
