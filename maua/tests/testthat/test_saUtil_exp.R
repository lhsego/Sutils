context("saUtil_exp")

test_that("saUtil_exp() returns values as expected", {

  x <- 1:10
  theta0 <- attributes(saUtil_exp(x, certEquiv = 4))$parms$theta
  expect_equal(as.vector(saUtil_exp(x, theta = theta0)), as.vector(saUtil_exp(x, certEquiv = 4)))

  u <- saUtil_exp(x, theta = 3, urange = c(1, 0))
  # from Smisc::hardCode(as.vector(u), vert = F, vname = "ux")
  ux <- c(1, 0.701678771157839, 0.487922270083583, 0.334759044225178, 0.225012797269146,
          0.146376175107186, 0.0900305731703804, 0.0496571851695323, 0.0207283885529804, 0)
  expect_true(max(abs(u - ux)) < 1e-15)

  # Verify endpoints match
  expect_true(max(abs(saUtil_exp(c(4, 7), urange = c(-1, 3), theta = 0) - c(-1, 3))) < 1e-15)
  expect_true(max(abs(saUtil_exp(c(5, 22), urange = c(4, -7), theta = 0) - c(4, -7))) < 1e-15)

  expect_true(inherits(u, "saUtilCall"))
  
})
