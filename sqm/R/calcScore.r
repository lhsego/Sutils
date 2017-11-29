##' Calculates scores for a given matrix of predicted class
##' probabilities, along with the corresponding truth classes.
##'
##' For a dataframe of signatures, this function computes an assortment of 
##' scoring rule metrics for each signature. A data frame is returned containing 
##' the scoring metrics for each signature. 
##'
##' The data.frame \code{signatures} has the following columns: signatureID, 
##' truthClass, and multiple columns beginning with 'predictionClass' followed by
##' the class name. Each signature (denoted by 'signatureID') consists of many
##' observations, each of which has a true class membership (denoted by
##' 'truthClass') and a corresponding probability value for membership in each 
##' class (denoted by 'predictionClass'+className). The rows of \code{signatures} 
##' correspond to each observation for which a classification is made.
##'
##' The available scoring methods can be viewed with
##' \code{\link{scoreMethods}}.
##'
##' @export
##' 
##' @param signatures data.frame containing the true classes and class prediction
##' probabilities for each signature. See details.
##' 
##' @param methods vector of accuracy methods that will be used. For a list of
##' available accuracy methods, see \code{\link{scoreMethods}}.
##' 
##' @param ... Additional arguments to methods
##' 
##' @return A data.frame containing scores for each signature
##' 
##' @seealso \code{\link{logScore}}, \code{\link{brierScore}}
##'
##' @author Amanda White, Landon Sego
##'
##' @examples
##' # Load data
##' data(exampleScoreData)
##'
##' # Now calculate the scores on the whole data set
##' calcScore(exampleScoreData)

## TODO:
## It would be nice if the 'signatures' dataframe could also accomodate a 'weights' column that would
## calculate weighted scores--this would allow us to calculate risk or expected utility, with scores being
## the loss functions or the single attribute utility functions

calcScore <- function(signatures,
                      methods = scoreMethods(),
                      ...) {

  # Verify arguments
  stopifnot(is.data.frame(signatures),
            is.vector(methods),
            is.character(methods))

  # Verify that 'signatures' contains the required column names
  if (!all(c("signatureID", "truthClass") %in% colnames(signatures)))
    stop("'signatures' must contain columns labled 'signatureID' and 'truthClass'")

  if (sum(grepl("predictionClass", colnames(signatures))) < 2)
    stop("'signatures' must contain at least two columns labeled 'predictionClass' followed by the class name")

  # Match the methods arguments
  scoreMethods <- match.arg(methods, scoreMethods(), several.ok = TRUE)

  # for each scoringMethod, call function, then merge all results
  scoringFunctions <- lapply(scoreMethods, get)
  names(scoringFunctions) <- scoreMethods

  # Calculate the scores for each method
  resList <- lapply(scoringFunctions, function(ff) ff(signatures, ...))
  
  if (length(resList) > 1) {

    # Initialize
    res <- resList[[1]]

    # Merge together all the statistics calculated by the various functions
    for (r in names(resList[-1]))
      res <- merge(res, resList[[r]], by = "signatureID")
    
  }
  else {
    res <- sort.data.frame(resList[[1]], ~signatureID)
  }


  # Return results
  return(res)
  
} # calcScore

