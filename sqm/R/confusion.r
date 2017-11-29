#' Computes the confusion summary for a vector of classifications and a ground
#' truth vector.
#'
#' For a vector of classifications and truth labels, we create a confusion
#' matrix. We allow binary and multi-class classifications and compute the
#' following four measures for each class:
#'
#' \itemize{
#'  \item True Positives  (TP)
#'  \item True Negatives  (TN)
#'  \item False Positives (FP)
#'  \item False Negatives (FN)
#' }
#'
#' For multi-class classification, we consider each class in a binary context.
#' For example, suppose that we have the three food condiment classes: ketchup,
#' mustard, and other. When calculating the TP, TN, FP, and FN values for
#' ketchup, we consider each observation as either 'ketchup' or 'not ketchup.'
#' Similarly, for mustard, we would consider 'mustard' and 'not mustard', and for
#' other, we would consider 'other' and 'not other.'
#'
#' With the above counts for each class, we can quickly calculate a variety
#' of class-specific and aggregate classification accuracy measures.
#'
#' \code{truthClass} and \code{predictedClass} must have the same length, and they
#' must both be factors with the same levels.
#' 
#' @param truthClass factor containing ground truth classification labels
#' 
#' @param predictedClass factor containing predicted classification labels
#' 
#' @return list with the results of confusion matrix results for each class.

confusion <- function(truthClass, predictedClass) {

  # Verify required conditions
  stopifnot(is.factor(truthClass),
            is.factor(predictedClass),
            length(truthClass) == length(predictedClass),
            setequal(levels(truthClass), levels(predictedClass)))
    
  # The confusion matrix contains a summary of the correct and incorrect
  # classifications by class.
  confusionMatrix <- table(predictedClass, truthClass)
  
  # For each class, we compute the true positives, true negatives,
  # false positives, and false negatives from the confusion matrix.
  classSummary <- lapply(seq_len(nlevels(truthClass)), function(i) {
    classSummary <- list()
    classSummary$truePos <- confusionMatrix[i, i]
    classSummary$trueNeg <- sum(confusionMatrix[-i, -i])
    classSummary$falsePos <- sum(confusionMatrix[i, -i])
    classSummary$falseNeg <- sum(confusionMatrix[-i, i])
    classSummary$classSampleSize <- sum(confusionMatrix[,i])
    classSummary
  })
  names(classSummary) <- levels(truthClass)

  # We return to the calling code the summary for each class,
  # the total sample size, the number of classes
  confusionSummary <- list()
  confusionSummary$classSummary <- classSummary
  confusionSummary$sampleSize <- length(truthClass)
  confusionSummary$numClasses <- nlevels(truthClass)

  # Add confusion class to return object
  class(confusionSummary) <- c("confusion", class(confusionSummary))

  return(confusionSummary)
  
} # confusion

