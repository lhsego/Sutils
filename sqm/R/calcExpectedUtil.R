##' Calculate expected utility for each signature
##'
##' Calculate the expected utility (or other function) for each signature
##'
##' 
##' Weights are normalized for each \code{signatureID}.
##' 
##' @export
##' 
##' @param testData A data frame containing attributes used to distinguish
##' the signature systems on the columns
##' and observations on the rows. If \code{testData} has column called \code{signatureID},
##' the expected utility is calculated separately for each \code{signatureID}.
##'
##' @param utilFun A function whose single argument is a data frame that calculates
##' the integrand (typically multiattribute utility)
##' for each row in the data frame and adds a column
##' called \code{integrandLabel} containing the value to be integrated to \code{testData}.
##' This function should be aware of the format and column names of \code{testData}.
##'
##' @param integrandLabel The name of the variable added to \code{testData} by \code{utilFun} that
##' contains the integrand.
##' 
##' @param outputLabel A character string giving the name of the output column that will be
##' added to \code{testData} that contains the expected value for each \code{signatureID}.
##' 
##' @param weights A numeric vector of non-negative values that approximate the joint
##' distribution of all the variables over which the expected utility will be calculated.
##' \code{weights} should have the same number of elements as the number of rows in
##' \code{testData}.
##' Alternatively, this can be a character string indicating the name of a column in \code{testData} which
##' contains the weights. If \code{NULL}, equal weight is given to each observation.
##'
## TODO add: @param adjustRandom A logical indicating whether attributes that are stochastic
## (i.e. those which change from observation to observation and are not constant for a \code{signatureID})
## should have their endpoints adjusted in order
##'
##' @return A data frame with the expected utility calculated for each \code{signatureID}
##'
##' @author Landon Sego
##' @examples
##' # Generate single attribute utility functions
##' sa.funs <- list(a1 = saUtil(theta = 2, zrange = c(-2, 4), urange = c(1, 0)),
##'                 a2 = saUtil(urange = c(1,0), theta = -1.7))
##' # The weights
##' alphas <- c(a2 = 0.2, a1 = 0.8)
##' 
##' # Cook up some data
##' someData <- data.frame(signatureID = rep(c("A", "B"), each = 5),
##'                        a1 = runif(10, -2, 4),
##'                        a2 = rpois(10, 3), row.names = letters[1:10],
##'                        weights = rep(1:5, 2))
##' 
##' # Create a utility function
##' uf <- maUtil(alpha = alphas, saUtilFun = sa.funs)
##' 
##' calcExpectedUtil(someData, uf, weights = "weights")


calcExpectedUtil <- function(testData, utilFun, integrandLabel = "utility",
                             outputLabel = "expectedUtility", weights = NULL) {

  # Check arguments
  stopifnot(is.data.frame(testData),
            inherits(utilFun, "function"),
            "signatureID" %in% colnames(testData))
#            !(integrandLabel %in% colnames(testData)))

  # Create a variable name for the weights if it wasn't provided
  if (is.null(weights) | !is.character(weights)) {

    # Make sure the weightName doesn't duplicate one of the column names
    wn <- make.names(c(colnames(testData), "weights"), unique = TRUE)
    weightName <- wn[length(wn)]

  }
  else {
    weightName <- weights
  }
  
  # Get weights inserted into the data frame
  if (!is.null(weights)) {

    # If weights is a character string
    if (is.character(weights)) {
      
      if (length(character) > 1)
        stop("Only one variable in 'testData' can be selected for 'weights'")
      
      if (!(weights %in% colnames(testData)))
        stop("'testData' does not contain the variable '", weights, "'")

      stopifnot(is.numeric(testData[,weights]),
                all(testData[,weights] >= 0))
      
    } # If weights a character string

    # If a vector of numeric weights were provided
    else {
      
      # Check weights variable
      stopifnot(is.vector(weights),
                is.numeric(weights),
                all(weights >= 0),
                length(weights) == NROW(testData))

      # If weights have names, make them match the data...
      if (!is.null(names(weights))) {
        
        stopifnot(setequal(names(weights), rownames(testData)))
        testData[, weightName] <- weights[rownames(testData)]

      }
      # And if they don't have names, just add them in...
      else {
        testData[, weightName] <- weights
      }

    } # else 
    
  }
  # If weights not provided, set them to uniform
  else {
     testData[, weightName] <- 1
  }


  # Apply utilFun across the testData first
  tDataUtil <- utilFun(testData)[, c("signatureID", weightName, integrandLabel)]

  # Function to calculate expected value of 'theIntegrand' for each signature
  eU <- function(dSub) {

    # Normalized weights
    nwts <- dSub[,weightName] / sum(dSub[,weightName], na.rm = TRUE)

    # Calculate the expected value
    return(t(nwts) %*% dSub[,integrandLabel])

  } # eU

  # Calculate the expected value for each signatureID
  u <- unlist(as.list(by(tDataUtil, tDataUtil$signatureID, eU)))

  # Convert results to a data frame and return
  out <- data.frame(signatureID = names(u), row.names = 1:length(u))

  out[, outputLabel] <- u

  return(out)
  
} # calcExpectedUtil
