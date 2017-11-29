##' Calculate the log score for each observation in a dataframe
##'
##' Calculate the log score for each observation in a dataframe, probably as a
##' precursor to calculating the utility function for fidelity when classifier
##' output is a vector of probabilities.
##'
##' The data.frame \code{signatures} has the following columns: signatureID, 
##' truthClass, and multiple columns beginning with 'predictionClass' followed by
##' the class name. Each signature (denoted by 'signatureID') consists of many
##' observations, each of which has a true class membership (denoted by
##' 'truthClass') and a corresponding probability value for membership in each 
##' class (denoted by 'predictionClass'+className). The rows of \code{signatures} 
##' correspond to each observation for which a classification is made.
##'
##' The log score is simply the logarithm of the probability that is assigned by
##' the classifier to the true class.
##' The log score ranges from \eqn{(-\infty,0]} where larger scores are
##' better. Typically we would expect the log sore to be transformed via a linear
##' transformation to make it a utility function.
##' 
##' @export
##'
##' @param signatures data.frame containing the true classes and class prediction
##' probabilities for each signature. See details.
##'
##' @param lowerBoundCoef A positive fraction, in [0, 1], that is
##' multiplied by the smallest nonzero true-class probability, thereby defining
##' a positive lower bound of probabilities belonging to the true class and avoiding
##' log scores of \code{-Inf}.
##' This lower bound probability is substituted for cases where the probability of
##' the true class is 0.  If \code{lowerBoundCoef = 0} and if the probability of the true class
##' is 0, the resulting \code{-Inf} is left unchanged.
##'
##' @param base The base of the logarithm. Defaults to the natural logarithm.
##'
##' @param keepPreds A logical indicating whether the columns containing the probabilities
##' for each class (the \code{predictionClass} columns)
##' should be retained or not in the output dataframe.
##'
##' @return The \code{signatures} dataframe is returned with an additional column,
##' \code{logScore}, which is the logarithm of the probability of the true class. The
##' \code{predictionClass} columns are retained if \code{keepPreds = TRUE}. 
##'
##' @author Landon Sego
##'
##' @examples
##' data(exampleScoreData)
##' y <- fidelityLogScore(exampleScoreData)
##' head(y)

fidelityLogScore <- function(signatures,
                             lowerBoundCoef = 0.5,
                             base = exp(1),
                             keepPreds = FALSE) {

 # Check inputs
 stopifnot(is.data.frame(signatures) | is.matrix(signatures),
           is.numeric(lowerBoundCoef),
           lowerBoundCoef >= 0,
           lowerBoundCoef <= 1,
           is.numeric(base))
 
  # Verify that 'signatures' contains the required column names
  if (!all(c("signatureID", "truthClass") %in% colnames(signatures)))
    stop("'signatures' must contain columns labled 'signatureID' and 'truthClass'")

  if (sum(grepl("predictionClass", colnames(signatures))) < 2)
    stop("'signatures' must contain at least two columns labeled 'predictionClass'",
         "followed by the class name")
  
 classNames <- sort(unique(signatures$truthClass))
  class.column.order <- gsub("predictionClass", "", 
    colnames(signatures)[grep("^predictionClass", colnames(signatures))])

  # Check that truth classes match column headers
  if (!setequal(classNames, class.column.order)) {
    stop(paste("Missing probability prediction column(s) for class(es):", 
        paste(setdiff(classNames, class.column.order), collapse=", ")))
  }

  # Check to make sure probabilities in each row sum to 1. If not warn and rescale.
  ind <- abs(rowSums(
    signatures[, paste("predictionClass", classNames, sep="")]) - 1) > 1e-12
  if (any(ind)) {
    warning("Rescaling probabilities so they sum to 1")
    signatures[ind, paste("predictionClass", classNames, sep = "")] <-
      signatures[ind, paste("predictionClass", classNames, sep = "")]/
      rowSums(signatures[ind, paste("predictionClass", classNames, sep = "")])
  }


  # Create a single column that contains the probability assigned to the true
  # class.  This will probably be slow--and could be optimized later.
  trueProb <- double(NROW(signatures))

  for (i in 1:NROW(signatures))
    trueProb[i] <- signatures[i, paste("predictionClass", signatures[i, "truthClass"], sep = "")]

  # Now calculate the logscore
  logTrueProb <- log(trueProb, base = base)

  # Now convert any -Inf to the log of the lower bound using lowerBoundCoef
  if (lowerBoundCoef > 0) {
    
    if (any(trueProb <= 0)) {

      # Find the log score of the second largest probability value
      secondLargest <- log(min(trueProb[trueProb > 0]) * lowerBoundCoef,
                           base = base)

      # Replace -Inf with secondLargest
      logTrueProb[logTrueProb == -Inf] <- secondLargest
      
    }
  }

  # Verify whether 'logScore' already exists in the data frame
  if ("logScore" %in% colnames(signatures))
    warning("The column called 'logScore' in 'signatures' will be overwritten")
 
  # Now add the logScores back into the dataframe
  signatures$logScore <- logTrueProb

  # Remove the predictionClass columns if requested
  if (!keepPreds) 
    signatures <- signatures[, -which(grepl("predictionClass", colnames(signatures)))]

  return(signatures)
 
} # fidelityLogScore
