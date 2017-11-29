context("fidel_logScore()")

p1 <- runif(9)
p2 <- runif(9, 0, 1 - p1)
p3 <- 1 - p1 - p2
   
# Notice how the names of the probability columns "prob_7", "prob_9", and "prob_2" have
# "2", "7", and "9" in them, which are the values of truth column, "verdad"
X <- data.frame(verdad = factor(rep(c(2, 7, 9), 3)),
                prob_7 = p1,
                prob_9 = p2,
                prob_2 = p3,
                extra = rnorm(9))

correctResult <- log(c(p3[1], p1[2], p2[3], p3[4], p1[5], p2[6], p3[7], p1[8], p2[9]))

# TESTS
test_that("fidel_logScore() returns values as expected", {

  x <- fidel_logScore(X, "verdad", c(FALSE, TRUE, TRUE, TRUE, FALSE), outCol = "ls")$ls

  expect_equal(x, correctResult)

}) 

test_that("The behavior of fidel_logScore() with NA's", {

   X1 <- X
   # A truth value is NA
   X1[2, 1] <- NA
   # A true probability is NA
   X1[1, 4] <- NA
   # A false probability is NA
   X1[9, 2] <- NA

   expect_error(fidel_logScore(X1, "verdad", 2:4, outCol = "ls"),
                "Error : fidel_logScore(", fixed = TRUE)

   # An extra column value is NA
   X1 <- X
   X1[8, 5] <- NA

   x1 <- fidel_logScore(X1, 1, 2:4, outCol = "ls")$ls

   expect_equal(x1, correctResult)
    
})


test_that("fidel_logScore() hits endpoints correctly", {

  X2 <- data.frame(pa = c(0.25, 1, 0, 0),
                   pb = c(0.25, 0, 0, 0.5),
                   pc = c(0.25, 0, 1, 0.5),
                   pd = c(0.25, 0, 0, 0),
                   tr = factor(c("d", "a", "b", "d"), levels = letters[1:4]))

  x2 <- fidel_logScore(X2, "tr", 1:4, outCol = "b")$b
  x2s <- fidel_logScore(X2, "tr", paste("p", letters[1:4], sep = ""), outCol = "h", scale = TRUE)$h

  expect_equal(x2, c(-log(4), 0, -Inf, -Inf))
  expect_equal(x2s, c(0, 1, -Inf, -Inf))
    
})

test_that("fidel_logScore() returns errors as expected", {

  expect_error(fidel_logScore(TRUE, "this", "that"), "'X' must be a dataframe")
  expect_error(fidel_logScore(TRUE, "this", "that"), "Error : fidel_logScore(", fixed = TRUE)

  expect_error(fidel_logScore(data.frame(x = 10, y = 12), "p", "q"), "'p' is not in", fixed = TRUE)
  expect_error(fidel_logScore(data.frame(x = 10, y = 12), "x", "q"), "'q' is not in", fixed = TRUE)

  expect_error(fidel_logScore(data.frame(x = 10, y = 12), c("x", "y"), "x"), "Error : fidel_logScore(", fixed = TRUE)

  expect_error(fidel_logScore(data.frame(a = factor(1:2), b1 = TRUE, b2 = c(0.5, 0.7)), "a", c("b1", "b2")),
               "Error : fidel_logScore(", fixed = TRUE)

  expect_error(fidel_logScore(data.frame(a = factor(1:2), b1 = c(0.2, 0.3), b2 = c(0.5, 0.7)), "a", c("b1", "b2")),
               "The columns in 'X' selected", fixed = TRUE)

  expect_warning(fidel_logScore(data.frame(a = factor(1:2), b1 = c(0.5, 0.3), b2 = c(0.5, 0.7)), "a", c("b1", "b2"), outCol = "a"),
                 "In fidel_logScore(", fixed = TRUE)

  expect_error(fidel_logScore(data.frame(a = factor(1:2, levels = 1:3), b1 = c(0.2, 0.3), b2 = c(0.8, 0.7)), "a", c("b1", "b2")),
               "Error : fidel_logScore(", fixed = TRUE)

  expect_error(fidel_logScore(data.frame(a = factor(1:2), b1 = c(0.2, 0.3), c1 = c(0.8, 0.7)), "a", c("b1", "c1")),
               "Error : fidel_logScore(", fixed = TRUE)
  
})
