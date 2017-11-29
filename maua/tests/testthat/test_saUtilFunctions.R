context("saUtilFunctions.R")

test_that("saUtilFunctions() performs as expected", {

  expect_true(all(c("saUtil_exp", "saUtil_log") %in% saUtilFunctions()))

  expect_identical(saUtilFunctions("saUtil_exp"), "saUtil_exp")
  
  expect_identical(saUtilFunctions("exp"), "saUtil_exp")

  expect_identical(saUtilFunctions("log"), "saUtil_log")

  expect_error(saUtilFunctions("noUtil"), "is not in")

  expect_error(saUtilFunctions(7), "'saUtilFun' must be NULL or")
              
  expect_error(saUtilFunctions(c("t", "y")), "'saUtilFun' must be NULL or")
    
})
