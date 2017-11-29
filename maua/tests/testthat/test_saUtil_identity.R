context("saUtil_identity")

test_that("saUtil_identity() returns values as expected", {

  x <- rnorm(50, sd = 5)
  u <- saUtil_identity(x)
  
  expect_true(max(abs(x - u)) < 1e-15)

  expect_true(inherits(u, "saUtilCall"))
  
})

