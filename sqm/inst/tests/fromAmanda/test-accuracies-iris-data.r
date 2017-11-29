library('testthat')
library('MASS')
library('plyr')
library('SQM')
library('tools') # Needed for md5sum()

context("Test that the accuracies are computed correctly with the iris data set.")

test_that("The accuracies for the iris data sets are computed correctly.", {

  set.seed(42)

  truthClass <- iris[, 5]
  classes <- levels(truthClass)

  # Creates two sets of signatures by randomly sampling from the true classes
  # to construct the predicted classes
  signatures <- rbind(
                      cbind.data.frame(signatureID = 1, truthClass = truthClass,
                            predictedClass = sample(truthClass)),
                      cbind.data.frame(signatureID = 2, truthClass = truthClass,
                            predictedClass = sample(truthClass))
                     )

  # Now, we write these generated signatures to a file as if they are the original
  # signatures to be inputted.
  inputFile <- "temp_signatures.csv"
  write.table(signatures, file = inputFile, sep = ",", row.names = FALSE,
              col.names = FALSE, quote = FALSE)
  inputtedSignatures <- readAccuracy(inputFile)

  # Now, we calculate the accuracy estimates for each of the aggregate measures.
  accEstimatesMicro <- calcAccuracy(inputtedSignatures)
  accEstimatesMacro <- calcAccuracy(inputtedSignatures, aggregate = "macro")

  # Now, we delete the temp signatures file.
  unlink(inputFile)

  # The byClass accuracy estimates should be the same regardless of which
  # aggregate method is selected.
  expect_equal(accEstimatesMicro$byClass, accEstimatesMacro$byClass)

  # Below, we manually compute the accuracy estimates for each 'signature' to
  # make a fair comparison with the estimates calculated with the SQM package.
  # This is useful in determining if the accuracy estimates are correctly
  # implemented, but also ensures that the estimates are not changed
  # unnecessarily in future versions.

  # Constructs the confusion matrix for each 'signature'
  # Rows are the truthClass
  # Columns are the predictedClass
  sigConfusion <- tapply(seq_len(nrow(signatures)), signatures$signatureID, function(i) {
    table(signatures$truthClass[i], signatures$predictedClass[i])
  })

  # Two temporary lists used to hold the numerators and denominators for each of
  # the aggregate measures.
  macroAgg <- list()
  microAgg <- list()

  # For each 'signature', we calculate the byClass and aggregate estimates of
  # each accuracy estimator.
  sigSummaries <- lapply(sigConfusion, function(sigConf) {
    macroAgg$accuracy <<- list(num = 0, denom = 0)
    macroAgg$precision <<- list(num = 0, denom = 0)
    macroAgg$recall <<- list(num = 0, denom = 0)
    macroAgg$sensitivity <<- list(num = 0, denom = 0)
    macroAgg$Fscore <<- list(num = 0, denom = 0)
    macroAgg$specificity <<- list(num = 0, denom = 0)
  
    microAgg$accuracy <<- list(num = 0, denom = 0)
    microAgg$precision <<- list(num = 0, denom = 0)
    microAgg$recall <<- list(num = 0, denom = 0)
    microAgg$sensitivity <<- list(num = 0, denom = 0)
    microAgg$Fscore <<- list(num = 0, denom = 0)
    microAgg$specificity <<- list(num = 0, denom = 0)
  
    sigSummary <- lapply(seq_len(nlevels(truthClass)), function(i) {
      out <- list()
      # Computes TP, FP, TN, and FN for class i
      TP <- sigConf[i, i]
      FP <- sum(sigConf[i, -i])
      TN <- sum(sigConf[-i, -i])
      FN <- sum(sigConf[-i, i])
      out$accuracy <- (TP + TN) / (TP + FP + TN + FN)
      out$precision <- TP / (TP + FP)
      out$recall <- TP / (TP + FN)
      out$sensitivity <- out$recall
      out$Fscore <- 2 * TP / (2 * TP + FN + FP)
      out$specificity <- TN / (FP + TN)
  
      macroAgg$accuracy$num <<- macroAgg$accuracy$num + out$accuracy
      macroAgg$accuracy$denom <<- macroAgg$accuracy$denom + 1
      macroAgg$precision$num <<- macroAgg$precision$num + out$precision
      macroAgg$precision$denom <<- macroAgg$precision$denom + 1
      macroAgg$recall$num <<- macroAgg$recall$num + out$recall
      macroAgg$recall$denom <<- macroAgg$recall$denom + 1
      macroAgg$sensitivity$num <<- macroAgg$sensitivity$num + out$sensitivity
      macroAgg$sensitivity$denom <<- macroAgg$sensitivity$denom + 1
      macroAgg$specificity$num <<- macroAgg$specificity$num + out$specificity
      macroAgg$specificity$denom <<- macroAgg$specificity$denom + 1
  
      microAgg$accuracy$num <<- microAgg$accuracy$num + out$accuracy
      microAgg$accuracy$denom <<- microAgg$accuracy$denom + 1
      microAgg$precision$num <<- microAgg$precision$num + (TP)
      microAgg$precision$denom <<- microAgg$precision$denom + (TP + FP)
      microAgg$recall$num <<- microAgg$recall$num + (TP)
      microAgg$recall$denom <<- microAgg$recall$denom + (TP + FN)
      microAgg$sensitivity$num <<- microAgg$sensitivity$num + (TP)
      microAgg$sensitivity$denom <<- microAgg$sensitivity$denom + (TP + FN)
      microAgg$specificity$num <<- microAgg$specificity$num + (TN)
      microAgg$specificity$denom <<- microAgg$specificity$denom + (FP + TN)
  
      out
    })
  
    names(sigSummary) <- classes
  
    sigSummary$macro$accuracy <- macroAgg$accuracy$num / macroAgg$accuracy$denom
    sigSummary$macro$precision <- macroAgg$precision$num / macroAgg$precision$denom
    sigSummary$macro$recall <- macroAgg$recall$num / macroAgg$recall$denom
    sigSummary$macro$sensitivity <- macroAgg$sensitivity$num / macroAgg$sensitivity$denom
    sigSummary$macro$specificity <- macroAgg$specificity$num / macroAgg$specificity$denom
    sigSummary$macro$Fscore <- with(sigSummary$macro,
                                    2 * precision * recall / (precision + recall))
  
    sigSummary$micro$accuracy <- microAgg$accuracy$num / microAgg$accuracy$denom
    sigSummary$micro$precision <- microAgg$precision$num / microAgg$precision$denom
    sigSummary$micro$recall <- microAgg$recall$num / microAgg$recall$denom
    sigSummary$micro$sensitivity <- microAgg$sensitivity$num / microAgg$sensitivity$denom
    sigSummary$micro$specificity <- microAgg$specificity$num / microAgg$specificity$denom
    sigSummary$micro$Fscore <- with(sigSummary$micro,
                                    2 * precision * recall / (precision + recall))
    
    sigSummary
  })

  # The byClass estimates for signature 1 should be equal.
  estSQM <- subset(accEstimatesMicro$byClass, signatureID == 1)
  estTest <- sigSummaries$`1`

  # We compare the 'setosa' class from SQM with the manually computed ones.
  expect_equal(estTest$setosa$accuracy, estSQM$accuracy[1])
  expect_equal(estTest$setosa$precision, estSQM$precision[1])
  expect_equal(estTest$setosa$recall, estSQM$recall[1])
  expect_equal(estTest$setosa$sensitivity, estSQM$sensitivity[1])
  expect_equal(estTest$setosa$specificity, estSQM$specificity[1])
  expect_equal(estTest$setosa$Fscore, estSQM$Fscore[1])
  
  # We compare the 'versicolor' class from SQM with the manually computed ones.
  expect_equal(estTest$versicolor$accuracy, estSQM$accuracy[2])
  expect_equal(estTest$versicolor$precision, estSQM$precision[2])
  expect_equal(estTest$versicolor$recall, estSQM$recall[2])
  expect_equal(estTest$versicolor$sensitivity, estSQM$sensitivity[2])
  expect_equal(estTest$versicolor$specificity, estSQM$specificity[2])
  expect_equal(estTest$versicolor$Fscore, estSQM$Fscore[2])
  
  # We compare the 'virginica' class from SQM with the manually computed ones.
  expect_equal(estTest$virginica$accuracy, estSQM$accuracy[3])
  expect_equal(estTest$virginica$precision, estSQM$precision[3])
  expect_equal(estTest$virginica$recall, estSQM$recall[3])
  expect_equal(estTest$virginica$sensitivity, estSQM$sensitivity[3])
  expect_equal(estTest$virginica$specificity, estSQM$specificity[3])
  expect_equal(estTest$virginica$Fscore, estSQM$Fscore[3])
  
  # The byClass estimates for signature 2 should be equal.
  estSQM <- subset(accEstimatesMicro$byClass, signatureID == 2)
  estTest <- sigSummaries$`2`
  
  # We compare the 'setosa' class from SQM with the manually computed ones.
  expect_equal(estTest$setosa$accuracy, estSQM$accuracy[1])
  expect_equal(estTest$setosa$precision, estSQM$precision[1])
  expect_equal(estTest$setosa$recall, estSQM$recall[1])
  expect_equal(estTest$setosa$sensitivity, estSQM$sensitivity[1])
  expect_equal(estTest$setosa$specificity, estSQM$specificity[1])
  expect_equal(estTest$setosa$Fscore, estSQM$Fscore[1])
  
  # We compare the 'versicolor' class from SQM with the manually computed ones.
  expect_equal(estTest$versicolor$accuracy, estSQM$accuracy[2])
  expect_equal(estTest$versicolor$precision, estSQM$precision[2])
  expect_equal(estTest$versicolor$recall, estSQM$recall[2])
  expect_equal(estTest$versicolor$sensitivity, estSQM$sensitivity[2])
  expect_equal(estTest$versicolor$specificity, estSQM$specificity[2])
  expect_equal(estTest$versicolor$Fscore, estSQM$Fscore[2])
  
  # We compare the 'virginica' class from SQM with the manually computed ones.
  expect_equal(estTest$virginica$accuracy, estSQM$accuracy[3])
  expect_equal(estTest$virginica$precision, estSQM$precision[3])
  expect_equal(estTest$virginica$recall, estSQM$recall[3])
  expect_equal(estTest$virginica$sensitivity, estSQM$sensitivity[3])
  expect_equal(estTest$virginica$specificity, estSQM$specificity[3])
  expect_equal(estTest$virginica$Fscore, estSQM$Fscore[3])
  
  # The micro estimates within each signature should be equal.
  
  # We compare the micro estimates for signature 1 from SQM with the manually
  # computed ones.
  estSQM <- subset(accEstimatesMicro$aggregate, signatureID == 1)
  estTest <- sigSummaries$`1`
  
  expect_equal(estTest$micro$accuracy, estSQM$accuracy[1])
  expect_equal(estTest$micro$precision, estSQM$precision[1])
  expect_equal(estTest$micro$recall, estSQM$recall[1])
  expect_equal(estTest$micro$sensitivity, estSQM$sensitivity[1])
  expect_equal(estTest$micro$specificity, estSQM$specificity[1])
  expect_equal(estTest$micro$Fscore, estSQM$Fscore[1])
  
  # We compare the micro estimates for signature 1 from SQM with the manually
  # computed ones.
  estSQM <- subset(accEstimatesMicro$aggregate, signatureID == 2)
  estTest <- sigSummaries$`2`
  
  expect_equal(estTest$micro$accuracy, estSQM$accuracy[1])
  expect_equal(estTest$micro$precision, estSQM$precision[1])
  expect_equal(estTest$micro$recall, estSQM$recall[1])
  expect_equal(estTest$micro$sensitivity, estSQM$sensitivity[1])
  expect_equal(estTest$micro$specificity, estSQM$specificity[1])
  expect_equal(estTest$micro$Fscore, estSQM$Fscore[1])
  
  # The macro estimates within each signature should be equal.
  
  # We compare the macro estimates for signature 1 from SQM with the manually
  # computed ones.
  estSQM <- subset(accEstimatesMacro$aggregate, signatureID == 1)
  estTest <- sigSummaries$`1`
  
  expect_equal(estTest$macro$accuracy, estSQM$accuracy[1])
  expect_equal(estTest$macro$precision, estSQM$precision[1])
  expect_equal(estTest$macro$recall, estSQM$recall[1])
  expect_equal(estTest$macro$sensitivity, estSQM$sensitivity[1])
  expect_equal(estTest$macro$specificity, estSQM$specificity[1])
  expect_equal(estTest$macro$Fscore, estSQM$Fscore[1])
  
  # We compare the macro estimates for signature 1 from SQM with the manually
  # computed ones.
  estSQM <- subset(accEstimatesMacro$aggregate, signatureID == 2)
  estTest <- sigSummaries$`2`
  
  expect_equal(estTest$macro$accuracy, estSQM$accuracy[1])
  expect_equal(estTest$macro$precision, estSQM$precision[1])
  expect_equal(estTest$macro$recall, estSQM$recall[1])
  expect_equal(estTest$macro$sensitivity, estSQM$sensitivity[1])
  expect_equal(estTest$macro$specificity, estSQM$specificity[1])
  expect_equal(estTest$macro$Fscore, estSQM$Fscore[1])
})

# Now, we want to ensure that the accuracy estimates computed via the end-user
# function, afAccuracy, are the same as those from calcAccuracy. The purpose
# is to ensure that afAccuracy does not make any changes to the estimates.
test_that("The estimates from afAccuracy are the same as calcAccuracy", {
  set.seed(42)

  truthClass <- iris[, 5]
  classes <- levels(truthClass)

  # Creates two sets of signatures by randomly sampling from the true classes
  # to construct the predicted classes
  signatures <- rbind(
                      cbind.data.frame(signatureID = 1, truthClass = truthClass,
                            predictedClass = sample(truthClass)),
                      cbind.data.frame(signatureID = 2, truthClass = truthClass,
                            predictedClass = sample(truthClass))
                     )

  # Now, we write these generated signatures to a file as if they are the
  # original signatures to be inputted.
  inputFile <- "temp_signatures.csv"
  write.table(signatures, file = inputFile, sep = ",", row.names = FALSE,
              col.names = FALSE, quote = FALSE)
  inputtedSignatures <- readAccuracy(inputFile)

  # Now, we calculate the accuracy estimates for each of the aggregate measures.
  outCalcAccuracyMicro <- calcAccuracy(inputtedSignatures)
  outCalcAccuracyMacro <- calcAccuracy(inputtedSignatures, aggregate = "macro")

  outAfAccuracyMicro <- afAccuracy(inputFile = inputFile)
  outAfAccuracyMacro <- afAccuracy(inputFile = inputFile, aggregate = "macro")

  expect_equal(outCalcAccuracyMicro, outAfAccuracyMicro)
  expect_equal(outCalcAccuracyMacro, outAfAccuracyMacro)

  # Now, we delete the temp signatures file.
  unlink(inputFile)
})

# Next, we want to ensure that the accuracy estimates that are outputted to a
# file via the end-user function, afAccuracy, are the same as those from
# calcAccuracy. The purpose is to ensure that afAccuracy does not make any
# changes to the estimates.
test_that("The file output estimates from afAccuracy equal calcAccuracy", {
  set.seed(42)

  truthClass <- iris[, 5]
  classes <- levels(truthClass)

  # Creates two sets of signatures by randomly sampling from the true classes
  # to construct the predicted classes
  signatures <- rbind(
                      cbind.data.frame(signatureID = 1, truthClass = truthClass,
                            predictedClass = sample(truthClass)),
                      cbind.data.frame(signatureID = 2, truthClass = truthClass,
                            predictedClass = sample(truthClass))
                     )

  # Now, we write these generated signatures to a file as if they are the
  # original signatures to be inputted.
  inputFile <- "temp_signatures.csv"
  write.table(signatures, file = inputFile, sep = ",", row.names = FALSE,
              col.names = FALSE, quote = FALSE)
  inputtedSignatures <- readAccuracy(inputFile)

  # Now, we calculate the accuracy estimates for each of the aggregate measures.
  outCalcAccuracyMicro <- calcAccuracy(inputtedSignatures)
  outCalcAccuracyMacro <- calcAccuracy(inputtedSignatures, aggregate = "macro")

  outAfAccuracyMicro <- afAccuracy(inputFile = inputFile,
                                   outputFilePrefix = 'micro')
  outAfAccuracyMacro <- afAccuracy(inputFile = inputFile,
                                   outputFilePrefix = 'macro',
                                   aggregate = 'macro')

  # The estimates from afAccuracy are outputted to the following CSV files.
  # We input the files and then compare them with the estimates computed
  # from calcAccuracy.
  microByClassFile <- 'micro_byClass.csv'
  microAggregateFile <- 'micro_aggregate.csv'
  macroByClassFile <- 'macro_byClass.csv'
  macroAggregateFile <- 'macro_aggregate.csv'

  inMicroAggregateFile <- read.csv(microAggregateFile)
  inMicroByClassFile <- read.csv(microByClassFile)
  inMacroAggregateFile <- read.csv(macroAggregateFile)
  inMacroByClassFile <- read.csv(macroByClassFile)

  inMicroAggregateFile$signatureID <- factor(inMicroAggregateFile$signatureID)
  inMicroByClassFile$signatureID <- factor(inMicroByClassFile$signatureID)
  inMacroAggregateFile$signatureID <- factor(inMacroAggregateFile$signatureID)
  inMacroByClassFile$signatureID <- factor(inMacroByClassFile$signatureID)

  inMicroByClassFile$class <- factor(inMicroByClassFile$class)
  inMacroByClassFile$class <- factor(inMacroByClassFile$class)
  
  expect_equal(inMicroByClassFile, outCalcAccuracyMicro$byClass)
  expect_equal(inMicroAggregateFile, outCalcAccuracyMicro$aggregate)
  expect_equal(inMacroByClassFile, outCalcAccuracyMacro$byClass)
  expect_equal(inMacroAggregateFile, outCalcAccuracyMacro$aggregate)


  # Now, we delete the temp signatures file and the output files.
  unlink(inputFile)
  unlink(microByClassFile)
  unlink(microAggregateFile)
  unlink(macroByClassFile)
  unlink(macroAggregateFile)
})


