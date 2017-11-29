##' Calculate the Brier sharpness for each observation
##'
##' @details
##' The Brier sharpness is \emph{- sum_k (P_k * (1 - P_k)} where \emph{P_k} is the predicted probability that the true class is \emph{k}.
##' This metric is discussed by Murphy (1973).
##'
##' Note that for \code{scale = TRUE} and \code{scale = FALSE}, larger is better.
##'
##' Setting \code{scale = TRUE} scales the Brier sharpness so that it is negative if the classifies does worse than guessing (i.e.,
##' worse than assinging uniform probabilities). Predicted probabilities with a 
##' maximum sharpness have a scaled value of \code{1} (which occurs when one class
##' has a predicted probability of 1 and the remainder are 0). 
##'
##' The linear transformation
##' that takes place when \code{scale = TRUE} maps the Brier sharpness, which ranges in \emph{[(1 / K) - 1, 0]},
##' to \emph{[0, 1]}, \emph{K} being the number of classes.
##' Additional information about the scaling is available in Section 3.2.1 of Holmes et al. (2013).
##' 
##' @export
##' @param X a data frame containing observations (i.e., instances, examples) on the rows, along with probability predictions for each
##' class.  May contain other columns as well.
##'
##' @param probs A character vector, logical vector, or vector of numeric indexes that identify the columns in \code{X} that contain
##' the predicted probabilities of the classes.  This is passed to
##' \href{http://pnnl.github.io/docs-Smisc/rd.html#selectelements}{selectElements()} from the
##' \href{http://pnnl.github.io/docs-Smisc}{Smisc} package.
##'
##' @param outCol A character string that indicates the name of the column that will contain the Brier sharpness.
##'
##' @param scale A logical indicating whether the Brier sharpness should be linearly scaled.  See Details.
##'
##' @return The data frame \code{X} is returned, with an appended column that contains the Brier sharpness.
##'
##' @references Murphy AH. (1973). A new vector partition of the probability score. Journal of Applied Meteorology, 12:595-600.
##'
##' Holmes AE, Sego LH, Webb-Robertson BJ, et al. (2013). An Approach for Assessing the Signature Quality of
##' Various Chemical Assays when Predicting the Culture Media Used to Grow Microorganisms.  Pacific Northwest National Laboratory,
##' PNNL-22126.
##'
##' @examples
##' p1 <- runif(9)
##' p2 <- runif(9, 0, 1 - p1)
##' p3 <- 1 - p1 - p2
##'
##' X <- data.frame(prob_7 = p1,
##'                 prob_9 = p2,
##'                 prob_2 = p3,
##'                 extra = rnorm(9))
##' 
##' fidel_brierSharpness(X, 1:3)

fidel_brierSharpness <- function(X, probs, outCol = "brierSharpness", scale = FALSE) {

  # After checking all the args, this leaves 'Xprobs' in the environment of this function
  Smisc::sepList(check_entropy_brierSharpness_args(X, probs, outCol, scale))

  # Calculate the Brier sharpness
  brierSharpness <- apply(Xprobs * (Xprobs - 1), 1, sum)

  # Scale if asked
  if (scale) {
    brierSharpness <- Smisc::linearMap(brierSharpness, D = c(1 / length(probs) - 1, 0), R = c(0, 1))
  }
  
  # Add the brierSharpness to the output
  X[,outCol] <- brierSharpness

  # Return the data frame
  return(X)

} # fidel_brierSharpness

