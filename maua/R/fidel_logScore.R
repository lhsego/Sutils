##' Calculate the log score for each observation
##' 
##' @details The column indicated by \code{truth} must be a factor whose levels indicate all possible classes.  The columns indicated by
##' \code{prob} must have names which contain the corresponding level names in the truth column.
##'
##' The log score is the natural logartihtm of the probability assigned to the true class.  It is calculated for each row separately.
##'
##' Note that for \code{scale = TRUE} and \code{scale = FALSE}, larger is better.
##'
##' Setting \code{scale = TRUE} scales the log score so that a negative score indicates the
##' classifier has done worse than guessing (i.e., worse than uniform probability assignmetns).  If the scale log score
##' is \emph{1}, the classifier has chosen perfectly by assigning probability \emph{1} to the correct class. 
##'
##' The linear transformation that takes place when \code{scale = TRUE} maps the log score, which ranges in \emph{[-Inf, 0]},
##' to \emph{[-Inf, 1]}.  A unform probability assignment receives a scaled log score of 0.
##' Additional information about the scaling is available in Section 3.2.1 of Holmes et al. (2013).
##' 
##' @export
##' @param X a data frame containing observations (i.e., instances, examples) on the rows, along with a truth column and
##' probability predictions for each class.  May contain other columns as well.
##'
##' @param truth A character string, logical vector, or numeric index that identifies the column in \code{X} that contains the
##' truth classes. This is passed to \href{http://pnnl.github.io/docs-Smisc/rd.html#selectelements}{selectElements()}
##' from the \href{http://pnnl.github.io/docs-Smisc}{Smisc} package.
##'
##' @param probs A character vector, logical vector, or vector of numeric indexes that identify the columns in \code{X} that contain
##' the predicted probabilities of the classes.  This is passed to
##' \href{http://pnnl.github.io/docs-Smisc/rd.html#selectelements}{selectElements()}.
##'
##' @param outCol A character string that indicates the name of the column that will contain the log scores.
##'
##' @param scale A logical indicating whether the log scores should be linearly scaled.  See Details.
##'
##' @return The data frame \code{X} is returned, with an appended column that contains the log scores.
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
##' # Notice how the names of the probability columns "prob_7", "prob_9", and "prob_2" have
##' # "2", "7", and "9" in them, which are the values of truth column, "verdad"
##' X <- data.frame(verdad = factor(rep(c(2, 7, 9), 3)),
##'                 prob_7 = p1,
##'                 prob_9 = p2,
##'                 prob_2 = p3,
##'                 extra = rnorm(9))
##' 
##' fidel_logScore(X, "verdad", 2:4)

fidel_logScore <- function(X, truth, probs, outCol = "logScore", scale = FALSE) {

  # After checking all the args, this leaves character values of 'truth' and 'probs',
  # as well as 'levelNames', 'matches', and the matrix 'Xprobs' in
  # the environment of this function
  Smisc::sepList(check_logScore_brierScore_args(X, truth, probs, outCol, scale))

  # Construct the truth indicator matrix
  indMatrix <- truthIndicator(X, truth, probs, levelNames, matches)

  # logScore
  logScore <- log(apply(Xprobs * indMatrix, 1, sum))

  # Scale it if requested.  Mapping a uniform guess (1 / number of classes) to 0,
  # mapping a perfect guess (1) to 1
  if (scale) {
    logScore <- Smisc::linearMap(logScore, D = c(-log(length(probs)), 0), R = c(0, 1))
  }
  
  # Add the logScore to the output
  X[,outCol] <- logScore

  # Return the data frame
  return(X)

} # fidel_logScore


# Function to check and prepare args for further use in fidel_logScore and fidel_brierScore
check_logScore_brierScore_args <- function(X, truth, probs, outCol, scale) {

  # Check on X, outCol, and scale
  Smisc::stopifnotMsg(
      
    # X
    if (is.data.frame(X)) {
      !is.null(colnames(X))
    } else FALSE,
    "'X' must be a dataframe with column names",
      
    # outCol
    is.character(outCol) & (length(outCol) == 1),
    "'outCol' must be a character string",

    # scale
    is.logical(scale) & (length(scale) == 1),
    "'scale' must be TRUE or FALSE",
      
    level = 4
  )

  # Verify these columns exist in X and select them
  truth <- Smisc::selectElements(truth, colnames(X))
  probs <- Smisc::selectElements(probs, colnames(X))

  # Check the truth and the probs
  Smisc::stopifnotMsg(
      
    # Only 1 truth column
    length(truth) == 1, 
    "Only one column from 'X' should be selected for 'truth'",

    # truth col is a factor
    is.factor(X[,truth]),
    "The column in 'X' selected by 'truth' must be a factor",

    # Length of probs must be at least 2
    length(probs) >= 2,
    "At least two columns must be selected by 'probs'",

    # truth and probs must be different
    !any(truth %in% probs),
    "The column selected by 'truth' must not be among the columns selected by 'probs'",

    # No missing values
    all(complete.cases(X[,c(truth, probs)])),
    "All values in 'X' selected by 'truth' and 'probs' must have non-missing values",
      
    level = 4
  )

  # More checks on probs
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
 
  # Get the level names
  levelNames <- levels(X[,truth])
  names(levelNames) <- levelNames

  # Check levels
  Smisc::stopifnotMsg(
    length(levelNames) == length(probs),
    "The number of levels in 'truth' must be equal to the number of columns selected by 'probs'",
    level = 4
  )

  # A 1 to 1 match needs to occur between probs and levelNames.  
  matches <- lapply(levelNames, function(x) grep(x, probs, fixed = TRUE))

  # Verify 1 and only 1 match has been made
  Smisc::stopifnotMsg(
   !any(lengths(matches) > 1),
   "One (or more) of the levels in the truth column match more than one of the columns in 'X' selected by 'probs'",
   !any(lengths(matches) == 0),
   "One (or more) of the levels in the truth column does not have a matching column in 'X' selected by 'probs'",
   level = 4
  )

  return(list(truth = truth, probs = probs, Xprobs = as.matrix(X[,probs]), levelNames = levelNames,
              matches = unlist(matches)))
    
} # check_logScore_brierScore_args


# Same aguments as fidel_logScore, with 'levelNames' and 'matches' produced by 'check_logScore_brierScore_args'
truthIndicator <- function(X, truth, probs, levelNames, matches) {

  # Convert the truth variable to character, make sure all the values are present in 'levelNames'
  truthClasses <- as.character(X[,truth])

  # Sanity check.  I can't imagine this ever happening
  Smisc::stopifnotMsg(all(unique(truthClasses) %in% levelNames),
                      "Not all values of 'as.character(X[,truth])' are in 'levels(X[,truth])'",
                      level = 3)

  # Now create a vector of column numbers representing the truth cases
  truthCols <- matches[truthClasses]

  # Create a matrix of 0's
  ind <- matrix(0, ncol = length(probs), nrow = nrow(X))

  # Create a matrix over which we'll operate
  indt <- cbind(ind, truthCols)
  nc <- ncol(indt)

  # A function that assigns 1 to the column with the true class
  assign1 <- function(Xrow) {

    colIndex <- Xrow[nc]

    Xrow[colIndex] <- 1

    return(Xrow[-nc])
      
  } # assign1
  
  return(t(apply(indt, 1, assign1)))
  
} # truthIndicator


  
