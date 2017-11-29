context("saUtil()")

test_that("saUtil() peforms as expected for exponential", {
    
   # Some data
   x <- rnorm(50)
   rx <- range(x)
   ce <- quantile(x, 0.7)

   # Creating a function
   uf <- saUtil("exp", zrange = rx, urange = c(1, 0), certEquiv = ce)
   expect_true(is.function(uf))
   cuf <- uf(x)
    
   # Calling the utility function directly
   cuf1 <- saUtil("exp", z = x, zrange = rx, urange = c(1, 0), certEquiv = ce)

   expect_identical(cuf, cuf1)

})


test_that("saUtil() peforms as expected for log", {

   # Some data
   x <- rnorm(50)
   rx <- range(x)
   sx <- min(x) - 0.5

   # Creating a function
   uf <- saUtil("log", shift = sx, zrange = rx, urange = c(-1, 1))
   expect_true(is.function(uf))
   cuf <- uf(x)
    
   # Calling the utility function directly
   cuf1 <- saUtil("log", z = x, shift = sx, zrange = rx, urange = c(-1, 1))

   expect_identical(cuf, cuf1)

})

test_that("error's produced by saUtil() are as expected", {

    expect_error(saUtil("exp", noParm = 20), "'noParm' is not a valid argument to 'saUtil_exp'")
    
    expect_error(saUtil("exp", noParm = 20, certEquiv = 7, no2parm = 7), "'noParm', 'no2parm' are not valid arguments to 'saUtil_exp'")
   
    expect_error(saUtil("exp", certEquiv = TRUE), "'zrange' must be supplied when 'z = NULL'")

    expect_error(saUtil("exp", certEquiv = TRUE, zrange = c(1, 2)), "'certEquiv' must be a single numeric value")

    expect_error(saUtil("ep", certEquiv = TRUE, zrange = c(1, 2)), "'ep' is not in")

    expect_error(saUtil("saUtil_log", noParm = 20, certEquiv = 7, shift = -2),
                 "'noParm', 'certEquiv' are not valid arguments to 'saUtil_log'")

    expect_error(saUtil("saUtil_log", shift = -2, zrange = c(1, -2)), "'zrange' must be a numeric vector of length 2 with")
    
})
