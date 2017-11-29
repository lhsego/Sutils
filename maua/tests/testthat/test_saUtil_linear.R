context("saUtil_linear")

test_that("saUtil_linear() returns values as expected", {

  x <- rnorm(50, sd = 5)

  u <- saUtil_linear(x, urange = c(-1, 2))
  u1 <- Smisc::linearMap(x, D = range(x), R = c(-1, 2))
  u2 <- saUtil_exp(x, theta = 0, urange = c(-1, 2))
  expect_true(max(abs(u - u1)) < 1e-15)
  expect_true(max(abs(u - u2)) < 1e-15)

  # Verify endpoints match
  expect_true(max(abs(saUtil_linear(c(4, 7), urange = c(-1, 3)) - c(-1, 3))) < 1e-15)
  expect_true(max(abs(saUtil_linear(c(5, 22), urange = c(4, -7)) - c(4, -7))) < 1e-15)

  expect_true(inherits(u, "saUtilCall"))
  
})

