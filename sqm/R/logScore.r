##' Calculates log scores for a given matrix of predicted class
##' probabilities.  
##'
##' For a data.frame of signatures, class prediction probabilities and true 
##' classes, this function computes the logarithmic score
##' for each signature and returns a data frame containing the results.
##'
##' The data.frame \code{signatures} has the following columns: signatureID, 
##' truthClass, and multiple columns beginning with 'predictionClass' followed by
##' the class name. Each signature (denoted by 'signatureID') consists of many
##' observations, each of which has a true class membership (denoted by
##' 'truthClass') and a corresponding probability value for membership in each 
##' class (denoted by 'predictionClass'+className). The rows of \code{signatures} 
##' correspond to each observation for which a classification is made.
##'
##' Let \eqn{p_k, k = 1,\ldots,n}{p_k, k=1,...,n} indicate the class predictions
##' where each \eqn{p_k}{p_k} corresponds to a single line in the \code{signatures}
##' data.frame and \eqn{n = nrow(signatures)}.  Let \eqn{J} denote the number of
##' classes. Let \eqn{\delta_{jk} = 1}{\delta_jk = 1} if 
##' and only if element \eqn{k} truly belongs to class \eqn{j}, 0 otherwise. Then
##' the log score is defined to be 
##' \deqn{L = \frac{1}{n}\sum_{k=1}^n\sum_{j=1}^J \delta_{jk}log(p_jk)}{L = (1/n)\sum_k \sum_j (\delta_jk)log(p_jk)}
##' The log score has a range of \eqn{(-\infty,0]} where larger scores are
##' better. 
##' 
##' Strictly proper scoring rules can be decomposed into the sum of two 
##' components, one of which measures the sharpness of probability assignments
##' (i.e. the degree to which the probability assignment is placed on a single
##' or very few classes) and the other which is a measure of calibration or 
##' accuracy (i.e. the degree to which the probability assignments match the true
##' class). 
##'
##' In the decomposition of the log score, the measure of sharpness is given by
##' the negative entropy, or information, of the probabilities.
##' \deqn{I = -\frac{1}{n}\sum_{k=1}^n\sum_{j=1}^J(p_jk)log(p_jk)}{I = -(1/n)\sum_k \sum_j (p_jk)log(p_jk)}
##' The log score and information are returned by this function.
##' 
##' To facilitate side-by-side comparison with the Brier score (see
##' \code{\link{brierScore}}) we can transform the log score so that the maximum
##' value is 1 and a uniform probability assignment
##' gives a score of 0. The unscaled log score for uniform probability 
##' assignments is \eqn{L = -log(J)}. Then the scaled log score is given by 
##' \deqn{L^R = 1+1/log(J)*L}
##' The scaled log score takes values in \eqn{(-\infty, 1]}.
##' Likewise, the information scores are scaled in the same manner. 
##' If \code{scale=TRUE} the scaled log score and scaled 
##' information are returned. 
##' 
##' @export
##' @param signatures data.frame containing the true classes and class prediction
##' probabilities for each signature. See details.
##' @param scale a boolean value: Should the scores be scaled so 1 is the 
##' maximum and 0 corresponds to uniform predictions.
##' @return a data.frame containing signature IDs, log scores,
##' and information scores

logScore <- function(signatures, scale = TRUE) {

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
    signatures[ind, paste("predictionClass", classNames, sep="")] <-
      signatures[ind, paste("predictionClass", classNames, sep="")]/
      rowSums(signatures[ind, paste("predictionClass", classNames, sep="")])
  }
  
  scores <- by(signatures, signatures$signatureID, 
    function(sigSubset) {
      sum(by(sigSubset, sigSubset$truthClass, 
        function(sigSubset2) {
          sum(log(sigSubset2[, 
            paste("predictionClass", sigSubset2[1, "truthClass"], sep="")]))
        }
      ))/nrow(sigSubset)
    }
  )

  information <- by(signatures, signatures$signatureID,
    function(sigSubset) {

      # The 'unlist' vectorizes the dataframe so that we may more easily define 0 * log 0 to be 0      
      m <- unlist(sigSubset[, paste("predictionClass", classNames, sep="")])

      sum(ifelse(m <= 0, 0, m * log(m))) / nrow(sigSubset)
      
    }
  )

  if (scale) {
    scores <- 1+1/log(length(classNames))*scores
    information <- 1+1/log(length(classNames))*information
  }
  
  return(data.frame(signatureID=names(scores), logScore=as.vector(scores), 
                    information=as.vector(information)))
  
} # logScore

