context("calcFidelity() -- checking formats of inputs and outputs")

# load the data
data(exClassified, package = "SQM")
d <- exClassified

test_that("Test different types of grouping variables (character, int, factor, numeric),", {

    # Test integer
    out1 <- calcFidelity(d, truthVar = "truthClass", predVar = "predictedClass", groupVar = "group")

    # Test character
    d2 <- d
    d2$group <- as.character(d2$group)
    out2 <- calcFidelity(d2, truthVar = "truthClass", predVar = "predictedClass", groupVar = "group")

    # Reorder using order from out1
    out2$aggregate <- out2$aggregate[c(1, 3:10, 2),]
    rownames(out2$aggregate) <- NULL

    out2$byClass <- out2$byClass[c(setdiff(1:100, 11:20), 11:20),]
    rownames(out2$byClass) <- NULL

    # Test factor
    d3 <- d
    d3$group <- as.factor(d3$group)
    out3 <- calcFidelity(d3, truthVar = "truthClass", predVar = "predictedClass", groupVar = "group")

    # Test numeric
    d4 <- d
    d4$group <- d4$group + 0.732
    out4 <- calcFidelity(d4, truthVar = "truthClass", predVar = "predictedClass", groupVar = "group")

    # These should be equal
    expect_equal(out1$aggregate[,-1], out2$aggregate[,-1])
    expect_equal(out1$aggregate[,-1], out3$aggregate[,-1])
    expect_equal(out1$aggregate[,-1], out4$aggregate[,-1])

    # And these should be equal
    expect_equal(out1$byClass[,-c(1, 2)], out2$byClass[,-c(1, 2)])
    expect_equal(out1$byClass[,-c(1, 2)], out3$byClass[,-c(1, 2)])
    expect_equal(out1$byClass[,-c(1, 2)], out4$byClass[,-c(1, 2)])

    # And these all should be true
    expect_true(is.factor(out1$byClass$class))
    expect_true(is.factor(out2$byClass$class))
    expect_true(is.factor(out3$byClass$class))
    expect_true(is.factor(out4$byClass$class))
    
    expect_true(is.integer(out1$byClass$group))
    expect_true(is.character(out2$byClass$group))
    expect_true(is.factor(out3$byClass$group))
    expect_true(is.numeric(out4$byClass$group))
    
    expect_true(is.integer(out1$aggregate$group))
    expect_true(is.character(out2$aggregate$group))
    expect_true(is.factor(out3$aggregate$group))
    expect_true(is.numeric(out4$aggregate$group))    
    
})



## a <- exampleSignatures
## a <- a[a$signatureID == 1,]

## #calcFidelity(a, truthVar = "truthClass", predVar = "predictedClass")# methods = c("accuracy","nothing","something"))

## out <- calcFidelity(exampleSignatures, truthVar = "truthClass", predVar = "predictedClass", groupVars = "signatureID")

## b <- exampleSignatures
## b$sVar <- c("b", rep(c("a","b"), each = 210))

## out <- calcFidelity(b, truthVar = "truthClass", predVar = "predictedClass", groupVars = c("signatureID","sVar"))

## set.seed(7)

## # Make a dataset that will work
## a <- expand.grid(g1 = rep(1:3, each = 4), g2 = rep(letters[1:2], each = 5))

## # Test a groupVar with only one level
## a$pred <- sample(rep(1:3, 40))
## a$truth <- sample(rep(1:3, 40))

## out <- calcFidelity(a, truthVar = "truth", predVar = "pred", groupVars = c("g1", "g2"))

## out1 <- calcFidelity(a, truthVar = "truth", predVar = "pred")
