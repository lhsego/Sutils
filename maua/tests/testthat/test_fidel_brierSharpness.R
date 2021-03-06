context("fidel_brierSharpness()")

p1 <- runif(9)
p2 <- runif(9, 0, 1 - p1)
p2[7] <- 0
p3 <- 1 - p1 - p2
   
X <- data.frame(prob_7 = p1,
                prob_9 = p2,
                prob_2 = p3,
                extra = rnorm(9))

Xp <- as.matrix(X[,1:3])
correctResult <- -1 * apply(Xp * (1 - Xp), 1, sum)

# TESTS
test_that("fidel_brierSharpness() returns values as expected", {

  x <- fidel_brierSharpness(X, 1:3, outCol = "ls")$ls

  expect_equal(x, correctResult)

}) 

test_that("The behavior of fidel_brierSharpness() with NA's", {

   X1 <- X
   X1[1, 3] <- NA
   X1[9, 2] <- NA

   expect_error(fidel_brierSharpness(X1, c("prob_9", "prob_2", "prob_7"), outCol = "ls"),
                "Error : fidel_brierSharpness(", fixed = TRUE)

   # An extra column value is NA
   X1 <- X
   X1[8, 4] <- NA

   x1 <- fidel_brierSharpness(X1, 1:3, outCol = "ls")$ls

   expect_equal(x1, correctResult)
    
})


test_that("fidel_brierSharpness() hits endpoints correctly", {

  X2 <- data.frame(pa = c(0.25, 1),
                   pb = c(0.25, 0),
                   pc = c(0.25, 0),
                   pd = c(0.25, 0))

  x2 <- fidel_brierSharpness(X2, 1:4, outCol = "b")$b
  x2s <- fidel_brierSharpness(X2, paste("p", letters[1:4], sep = ""), outCol = "h", scale = TRUE)$h

  expect_equal(x2, c(1/4 - 1, 0))
  expect_equal(x2s, c(0, 1))
    
})

test_that("fidel_brierSharpness() returns errors as expected", {

  expect_error(fidel_brierSharpness(TRUE, "that"), "'X' must be a dataframe")
  
  expect_error(fidel_brierSharpness(TRUE, "that"), "Error : fidel_brierSharpness(", fixed = TRUE)

  expect_error(fidel_brierSharpness(data.frame(x = 10, y = 12), "p"), "'p' is not in", fixed = TRUE)

  expect_error(fidel_brierSharpness(data.frame(a = factor(1:2), b1 = TRUE, b2 = c(0.5, 0.7)), c("b1", "b2")),
               "Error : fidel_brierSharpness(", fixed = TRUE)

  expect_error(fidel_brierSharpness(data.frame(a = factor(1:2), b1 = c(0.2, 0.3), b2 = c(0.5, 0.7)), c("b1", "b2")),
               "The columns in 'X' selected", fixed = TRUE)

  expect_warning(fidel_brierSharpness(data.frame(a = factor(1:2), b1 = c(0.5, 0.3), b2 = c(0.5, 0.7)), c("b1", "b2"), outCol = "a"),
                 "In fidel_brierSharpness(", fixed = TRUE)
  
})
