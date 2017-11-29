context("saUtil_log")

test_that("saUtil_log() returns values as expected", {

  expect_equal(as.vector(saUtil_log(exp(0:10), urange = c(0, 10))), 0:10, tol = 1e-15)

  expect_equal(as.vector(saUtil_log(c(1,10))), 0:1, tol = 1e-15)

  p <- c(0.3, 0.8, 0.99, 0.05, 1/7)
  #  hardCode(as.vector(saUtil_log(p, zrange = c(1/7, 1))), vert = FALSE, vname = "pt")
  pt <- c(0.381280371598641, 0.885326898869128, 0.994835149064626, -0.53950184956296, 0)
  expect_equal(as.vector(saUtil_log(p, zrange = c(1/7, 1))), pt, tol = 1e-15)
  
  # Verify endpoints match
  expect_true(max(abs(saUtil_log(c(4, 7), urange = c(-1, 3)) - c(-1, 3))) < 1e-15)
  expect_true(max(abs(saUtil_log(c(-5, 22), urange = c(4, -7), shift = -6) - c(4, -7))) < 1e-15)

  # Check class
  expect_true(inherits(saUtil_log(exp(0:10), urange = c(0, 10)), "saUtilCall"))
  
})
