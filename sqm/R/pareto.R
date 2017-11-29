##' Identify the pareto frontier of a set of attributes
##'
##' Identify the pareto frontier of a set of attributes
##'
##' To summarize across multiple observations within a \code{signatureID}, the expected value
##' of each attribute named in \code{orientation} is calculated using the \code{weights}.
##'
##' Weights are normalized for each \code{signatureID}.
##'
##' The \code{frontier} column is generated using \code{\link{idParetoFrontier}}.
##' 
##' @export
##' 
##' @param testData A data frame containing attributes used to distinguish
##' the signature systems on the columns
##' and observations on the rows. If \code{testData} has column called \code{signatureID},
##' the expected utility is calculated separately for each \code{signatureID}.
##'
##' @param orientation A named vector of 1's and -1's, where the names identify the columns
##' in \code{testData} that will be used in the pareto analysis. A value of \code{1} indicates the
##' utility of the variable is increasing, i.e. larger values of the variable are preferred.
##' A value of \code{-1} indicates the utility of the variable is decreasing, i.e., smaller values
##' of the variable are preferred to larger ones.
##'
##' @param weights A numeric vector of non-negative values that approximate the joint
##' distribution of all the variables over which the expectation will be calculated.
##' \code{weights} should have the same number of elements as the number of rows in
##' \code{testData}.
##' Alternatively, this can be a character string indicating the name of a column in \code{testData} which
##' contains the weights. If \code{NULL}, equal weight is given to each observation.
##'
##' @return A data frame with the expected value of each attribute, calculated for
##' each \code{signatureID},
##' and an additional column called \code{frontier} indicating which \code{signatureID}s
##' constitute the pareto frontier.
##'
## TODO: @references Keeney & Raiffa 1976, Holmes, Sego, et al. 2013
##'
##' @author Landon Sego
##' @examples
##'
##' # Cook up some data
##' someData <- data.frame(signatureID = rep(LETTERS[1:5], each = 5),
##'                        a1 = runif(25, -2, 4),
##'                        a2 = rpois(25, 3), row.names = letters[1:25],
##'                        weights = rep(1:5, 5))
##' 
##' # Identify the pareto frontier
##' p <- pareto(someData, c(a1 = -1, a2 = 1), weights = "weights")
##' print(p)
##' plot(p)

pareto <- function(testData, orientation, weights = NULL) {

  # Check arguments
  stopifnot(is.data.frame(testData),
            "signatureID" %in% colnames(testData),
            is.vector(orientation),
            all(names(orientation) %in% colnames(testData)),
            !("signatureID" %in% names(orientation)))

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

  # New names for the expected values of the attributes
  eNames <- paste("expected", names(orientation), sep = ".")

  # Function to calculate expected value for each signature
  eZ <- function(dSub) {

    nwts <- dSub[,weightName] / sum(dSub[,weightName], na.rm = TRUE)

    # Prepare the matrix of attribute values
    dSSub <- as.matrix(dSub[,names(orientation)])
    colnames(dSSub) <- eNames

    # Calculate the expected value
    return(t(nwts) %*% dSSub)

  } # eZ

  # Make sure all elements in orientation are numeric
  if (!all(unlist(lapply(testData[,names(orientation)], is.numeric))))
    stop("All elements of 'testData' that are named in 'orientation'\n",
         "must be numeric")


  # Function to convert the matrices into vectors
  convert <- function(x) {

    if (NROW(x) != 1)
      stop("Unexpected result in convert(): the matrix has more than 1 row")

    y <- as.vector(x)
    names(y) <- colnames(x)
    
    return(y)

  } # convert
  
  # Calculate the expected utility for each signatureID
  u <- by(testData, testData$signatureID, eZ)
  u <- list2df(lapply(u, convert), col.names = eNames)

  # Clean the data frame
  u$signatureID <- rownames(u)
  u <- u[,c("signatureID", eNames)]
  rownames(u) <- 1:NROW(u)

  # Change the names of the orienation vector to help identify the pareto
  # frontier
  names(orientation) <- eNames

  # Identify the pareto frontier
  u <- idParetoFrontier(u, orientation)

  return(u)
  
} # pareto
