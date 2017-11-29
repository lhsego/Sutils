#' Calculates Brier scores for a given matrix of predicted class
#' probabilities  
#'
#' For a data.frame of signatures, class prediction probabilities and true 
#' classes, this function computes the Brier score [Hand, 1997]
#' for each signature and returns a data frame containing the results.
#'
#' The data.frame \code{signatures} has the following columns: signatureID, 
#' truthClass, and multiple columns beginning with 'predictionClass' followed by
#' the class name. Each signature (denoted by 'signatureID') consists of many
#' observations, each of which has a true class membership (denoted by
#' 'truthClass') and a corresponding probability value for membership in each 
#' class (denoted by 'predictionClass'+className). The rows of \code{signatures} 
#' correspond to each observation for which a classification is made.
#'
#' Let \eqn{p_k, k = 1,\ldots,n}{p_k, k=1,...,n} indicate the class 
#' prediction probability vectors, where each \eqn{p_k}{p_k} corresponds 
#' to a single line in the \code{signatures}
#' data.frame and \eqn{n = nrow(signatures)}.  Let \eqn{J} denote the number of
#' classes. Let \eqn{\delta_{jk} = 1}{\delta_jk = 1} if 
#' and only if element \eqn{k} truly belongs to class \eqn{j}, 0 otherwise. Then
#' the Brier score is defined to be 
#' \deqn{B = \frac{1}{n}\sum_{k=1}^n\sum_{j=1}^J(\delta_{jk} - p_jk)^2}{B = (1/n)\sum_k \sum_j (\delta_jk - p_jk)^2}
#' The Brier score has a range of \eqn{[0,2]} where smaller scores are
#' better. 
#' 
#' Strictly proper scoring rules can be decomposed into the sum of two 
#' components, one of which measures the sharpness of probability assignments
#' (i.e. the degree to which the probability assignment is placed on a single
#' or very few classes) and the other which is a measure of calibration or 
#' accuracy (i.e. the degree to which the probability assignments match the true
#' class). 
#' 
#' We can define the component of the Brier score decomposition that measures 
#' the sharpness of probability predictions thusly:
#' \deqn{S = -(1/n)\sum_{k=1}^n\sum_{j=1}^J(p_jk)(1 - p_jk)}{S = -(1/n)\sum_k \sum_j (p_jk)(1-p_jk)}
#' The Brier score and sharpness are returned by this function.
#' 
#' To facilitate side-by-side comparison with the log score (see
#' \code{\link{logScore}}) we can transform the Brier score so that the maximum
#' value is 1 and a uniform probability assignment 
#' gives a score of 0. The unscaled Brier score for uniform probability 
#' assignments is \eqn{B = (J-1)/J}. Then the scaled Brier score is given by 
#' \deqn{B^R = 1+J/(1-J)*B}
#' The scaled Brier score takes values in \eqn{[1-J^2/(J-1), 1]}.
#' The sharpness scores are scaled in nearly the same manner, except that the 
#' coefficient is the negative of that in the previous equation:
#' \deqn{S^R = 1+J/(J-1)*S}
#' If \code{scale=TRUE} 
#' the scaled Brier score and scaled sharpness are returned. 
#'
#' @references Hand, D.J. (1997). Construction and Assessment of Classification 
#' Rules. John Wiley & Sons.
#' 
#' @export
#' @param signatures data.frame containing the true classes and class prediction
#' probabilities for each signature. See details.
#' @param scale a boolean value: should the scores be scaled so 1 is the 
#' maximum and 0 corresponds to uniform predictions.
#' @return a data.frame containing signature IDs, Brier scores
#' and sharpness scores
#'
#' @author Amanda White
#' 
brierScore <- function(signatures, scale = TRUE) {
  
  classNames <- sort(unique(signatures$truthClass))
  class.column.order <- gsub("predictionClass", "", 
    colnames(signatures)[grep("^predictionClass", colnames(signatures))])
    
  ##Check that truth classes match column headers
  if (!setequal(classNames, class.column.order)) {
    stop(paste("Missing probability prediction column(s) for class(es):", 
        paste(setdiff(classNames, class.column.order), collapse=", ")))
  }

  ##Check to make sure probabilities in each row sum to 1. If not warn and rescale.
  ind <- abs(rowSums(
    signatures[, paste("predictionClass", classNames, sep="")]) - 1) > 1e-12
  if (any(ind)) {
    warning("Rescaling probabilities so they sum to 1")
    signatures[ind, paste("predictionClass", classNames, sep="")] <-
      signatures[ind, paste("predictiQonClass", classNames, sep="")]/
      rowSums(signatures[ind, paste("predictionClass", classNames, sep="")])
  }
  
  scores <- by(signatures, signatures$signatureID, 
    function(sigSubset) {
      numeric.truth <- as.numeric(factor(sigSubset$truthClass,
        levels=class.column.order))
      delta.mat <- delta_ij(numeric.truth, ncol=length(classNames))
      x <- delta.mat - sigSubset[, paste("predictionClass",
        class.column.order, sep="")]
      sum(x^2)/nrow(sigSubset)
    }
  )
  
  sharpness <- by(signatures, signatures$signatureID, 
    function(sigSubset) {
      m <- sigSubset[, paste("predictionClass", 
        classNames, sep="")]
      -sum(m*(1-m))/nrow(sigSubset)
    }
  )

  if (scale) {
    nclasses <- length(classNames)
    scores <- 1 + nclasses/(1-nclasses)*scores
    sharpness <- 1 + nclasses/(nclasses-1)*sharpness
  }
  
  data.frame(signatureID=names(scores), brierScore=as.vector(scores), 
    sharpness=as.vector(sharpness))
}


## A helper function for brierScore()
## Creates a delta(i,j) matrix such that 
## delta[i,j] == 1 iff truth[i] == j, else 0
## Note: truth must be numeric.
delta_ij <- function(truth, ncol = max(truth)) {
    dij <- matrix(0, nrow=length(truth), ncol=max(truth))
    dij[cbind(1:length(truth), truth)] <- 1
    dij
}
