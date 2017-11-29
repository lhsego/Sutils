##' Calculate the entropy for each observation
##'
##' @details
##' The entropy is \emph{- sum_k (P_k * log(P_k)} where \emph{P_k} is the predicted probability that the true class is \emph{k}.
##'
##' Note that for \code{scale = FALSE}, smaller is better, whereas for \code{scale = TRUE}, larger is better.
##'
##' Setting \code{scale = TRUE} scales the entropy so that it is negative if the classifies does worse than guessing (i.e.,
##' worse than assinging uniform probabilities). Predicted probabilities with a 
##' minimum entropy (maximum information) have a scaled entropy of \code{1} (which occurs when one class
##' has a predicted probability of 1 and the remainder are 0). 
##'
##' The linear transformation
##' that takes place when \code{scale = TRUE} maps the entropy, which ranges in \emph{[0,  log(K)]},
##' (\emph{K} being the number of classes), to \emph{0, 1]}, where \emph{0} indicates uniform probability assignments and \emph{1}
##' indicates all the probability was assigned to a single class.
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
##' @param outCol A character string that indicates the name of the column that will contain the entropy.
##'
##' @param scale A logical indicating whether the entropy should be linearly scaled.  See Details.
##'
##' @return The data frame \code{X} is returned, with an appended column that contains the entropy.
##' 
##' @references Holmes AE, Sego LH, Webb-Robertson BJ, et al. (2013). An Approach for Assessing the Signature Quality of
##' Various Chemical Assays when Predicting the Culture Media Used to Grow Microorganisms.  Pacific Northwest National Laboratory,
##' PNNL-22126.
##'
##' @examples
##' # Construct probability vectors that sum (piecewise) to 1
##' p1 <- runif(9)
##' p2 <- runif(9, 0, 1 - p1)
##' p3 <- 1 - p1 - p2
##'
##' X <- data.frame(prob_7 = p1,
##'                 prob_9 = p2,
##'                 prob_2 = p3,
##'                 extra = rnorm(9))
##' 
##' fidel_entropy(X, 1:3)

fidel_entropy <- function(X, probs, outCol = "entropy", scale = FALSE) {

  # After checking all the args, this leaves 'Xprobs' in the environment of this function
  Smisc::sepList(check_entropy_brierSharpness_args(X, probs, outCol, scale))

  # Add the entropy to the output
  intermed <- Xprobs * log(Xprobs)

  # If any Xprobs are 0, then set value to 0 for the sake of continuity, since 0 * log(0) = NaN,
  # This is mathematically OK because limit of x * log(x) = 0 as x approaches 0 from above.
  if (any(Xprobs == 0)) {
    intermed[Xprobs == 0] <- 0
  }

  # Finish calculation of entropy
  entropy <- -1 * apply(intermed, 1, sum)

  # Scale if requested
  if (scale) {
    entropy <- Smisc::linearMap(entropy, D = c(0, log(length(probs))), R = c(1, 0))
  }
  
  # Finish calculation of the entropy
  X[,outCol] <- entropy

  # Return the data frame
  return(X)

} # fidel_entropy


# Function to check and prepare args for further use in fidel_entropy and fidel_brierSharpness
check_entropy_brierSharpness_args <- function(X, probs, outCol, scale) {

  # Check on X
  Smisc::stopifnotMsg(if (is.data.frame(X)) {
                        !is.null(colnames(X))
                      } else FALSE,
                      "'X' must be a dataframe with column names",
                      is.character(outCol) & (length(outCol) == 1),
                      "'outCol' must be a character string",
                      is.logical(scale) & (length(scale) == 1),
                      "'scale' must be TRUE or FALSE",
                      level = 4)

  # Verify these columns exist in X
  probs <- Smisc::selectElements(probs, colnames(X))

  # Check probs
  Smisc::stopifnotMsg(
      
    # Length of probs must be at least 2
    length(probs) >= 2,
    "At least two columns must be selected by 'probs'",

    # No missing values
    all(complete.cases(X[,probs])),
    "All values in 'X' selected by 'probs' must have non-missing values",
      
    level = 4
  )

  # Check probs
  Smisc::stopifnotMsg(
      
    # probs are numeric
    if (all(sapply(X[,probs], is.numeric))) {
      # probs in [0, 1]
      all(sapply(X[,probs], function(x) all(x <= 1) & all(x >= 0))) &
      # probs sum to 1
      (max(abs(apply(X[,probs], 1, sum) - 1)) < 1e-10)
    } else FALSE,
      
    paste("The columns in 'X' selected by 'probs' must be probabilities:\n",
          "numeric values in [0, 1], with each row summing to 1", sep = ""),
      
    level = 4
  )

  # Check outCol
  if (outCol %in% colnames(X)) {
    callingFn <- deparse(sys.calls()[[sys.nframe() - 3]])
    warning("In ", callingFn, "\n",
            "The column called '", outCol, "' in 'X' will be overwritten because 'outCol = ", outCol, "'",
            call. = FALSE)
  }
  
  return(list(Xprobs = as.matrix(X[,probs])))
    
} # check_entropy_brierSharpness_args
