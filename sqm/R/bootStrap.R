##' Generate and calculate statistics from bootstrap samples
##'
##' Generate and calculate statistics from bootstrap samples of the test data
##'
##' Sampling is performed with replacement, stratified by signature ID. The indexes of \code{testData}
##' that indicate each bootstrap sample are created by \code{\link{genBootstrapIndexes}}.
##' 
##' The functions indicated in \code{calcFun} should each have a single argument
##' that operate on the entirety of
##' \code{testData} to calculate some selection of fidelity, scoring, utility, or
##' summary metrics for each \code{signatureID}. That is, they will produce a single
##' row of output for each \code{signatureID}, with the metrics on the columns.
##' These functions will be applied to each bootstrap replicate of \code{testData}, and
##' they must produce conformable output, i.e., they must
##' return data frames with the same number of rows, each returned data frame
##' must have a single column \code{signatureID}, and remaining columns should
##' ideally be named uniquely across the set of functions.
##'
##' @export
##' 
##' @param testData A dataframe that will be bootstrapped, must have a column called \code{signatureID}
##'
##' @param calcFun a character vector of functions that calculate a particular quality metric
##' on \code{testData}. See 'Details.'
##'
##' @param seed An integer value that sets the seed of the random number generator
##' for repeatibility.  If \code{NULL}, the seed is chosen by R based on the
##' system time.
##'
##' @param n An integer indicating the number of bootstrap replicates to calculate.
##'
##' @param nJobs The number of parallel jobs that will be run using \code{\link{mclapply}}.
##'
##' @param summarizeFun a character vector of the names of summary functions that
##' take a vector of data and compute a SCALAR summary statistic.  E.g., \code{\link{mean}},
##' \code{\link{median}}, \code{\link{sd}}, etc. Additional arguments to these functions
##' are currently not supported.  Defaults to \code{NULL}, in which case no summaries of
##' the bootstrapped samples are calculated.
##' 
##' @return A data frame containing of the results calculated by the functions in \code{calcFun},
##' one row for each \code{signatureID}, with a column indicating the
##' bootstrap sample if bootstrapping was requested.  If one or more
##' summary functions (via \code{summarizeFun})
##' were indicated, then \code{\link{bootStrap}} returns a list, where each element of the list is a dataframe
##' of the requested scores, summarized across the bootstraps for each \code{signatureID}.
##' 
##' @author Landon Sego
##'
##' @examples
##'
## TODO:  These examples are probably too many--and they have a lot of check
##        cases in them that should be reserved for a test script, either in
##        a separate testing script, or perhaps embedded in the example but
##        commented out so they do not appear in the example but are run
##        during R CMD CHECK...
##' data(exampleScoreData)
##' 
##' # Now calculate the scores on the whole data set
##' e1 <- bootStrap(exampleScoreData, scoreMethods(), n = 2, nJobs = 2)
##' print(e1)
##'           
##' e2 <- bootStrap(exampleScoreData, scoreMethods(), n = 2, nJobs = 2, seed = 283)
##' e3 <- bootStrap(exampleScoreData, scoreMethods(), n = 2, nJobs = 2, seed = 283)
##' e4 <- bootStrap(exampleScoreData, scoreMethods(), n = 2, nJobs = 2)
##' 
##' # Should be different
##' identical(e1, e2)
##' # Should be the same
##' identical(e2, e3)
##' # Should be different
##' identical(e3, e4)
##' 
##' # Get bootstrap estimates and calculate some summary stats on those bootstrap estimates
##' e5 <- bootStrap(exampleScoreData, "logScore", n = 6, seed = 3913, 
##'                 summarizeFun = c("mean", "max", "min", "sd"))
##' print(e5)
##' 
##' # Check the results of summarizing e5
##' e6 <- bootStrap(exampleScoreData, "logScore", n = 6, seed = 3913)
##' e6.mean <- list2df(by(e6, e6$signatureID, function(x) c(logScore = mean(x$logScore),
##'                                                         information = mean(x$information))))
##' # Manipulate e5.mean a bit for comparison with e6.mean
##' e5.mean <- e5$mean
##' rownames(e5.mean) <- e5.mean$signatureID
##' 
##' # Should be identical
##' identical(e5.mean[, -1], e6.mean)
##'
##' # Try it using 'calcScore'
##' cScore <- function(X) calcScore(X, scale = FALSE)
##' bootStrap(exampleScoreData, "cScore", n = 2, summarizeFun = c("mean","max"))
##' 
##' # An example using a multiAttribute utility function
##' 
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
##' eu <- function(d) calcExpectedUtil(d, maUtil(alpha = alphas, saUtilFun = sa.funs),
##'                                    weights = "weights")
##' 
##' # Bootstrap results
##' bootStrap(someData, "eu", n = 3)
##' 
##' # Look at means and standard deviations
##' bootStrap(someData, "eu", n = 3, summarizeFun = c("mean", "sd"))


bootStrap <- function(testData,
                      calcFun, 
                      seed = NULL,
                      n = 1,
                      nJobs = 1,
                      summarizeFun = NULL) {

  # testData must be a data frame
  stopifnot(is.data.frame(testData))
  stopifnot("signatureID" %in% colnames(testData))

  stopifnot(is.numeric(n) & is.numeric(nJobs))

  # Check the calcFun
  stopifnot(is.vector(calcFun) & is.character(calcFun))

  # Create the function list
  calcFunList <- lapply(as.list(calcFun), get)
  names(calcFunList) <- calcFun

  if (!all(unlist(lapply(calcFunList, inherits, what = "function"))))
    stop("All elements in 'calcFun' must be functions")

  # Set the list that mclapply will operate over
  aList <- as.list(1:as.integer(n))

  # The function for identifying the indexes of the bootstrap sample
  selInd <- function() {
    genBootstrapIndexes(testData$signatureID)
  }

  # This function calculates the scores for a single bootstrap sample
  cFun <- function(listInd) {

    # Calculate each function
    resList <- lapply(calcFunList, function(ff) ff(testData[selInd(),]))

    # Initialize the data frame
    res <- resList[[1]]

    # Merge the rest of the elements of resList in
    if (length(resList) > 1) {

      # Merge together all the statistics calculated by the various functions
      for (r in names(resList[-1]))
        res <- merge(res, resList[[r]], by = "signatureID", sort = FALSE)
      
    }

    # Do a final sort
    res <- sort.data.frame(res, ~signatureID)
    
    # Add in the bootstrap sample label
    res$bootstrapSample <- listInd

    # Append bootstrap sample label to the original rownames
    rownames(res) <- paste(rownames(res), listInd, sep = ".")

    return(res)
    
  } # cFun

  # Set the random number generator so that it will work properly with mclapply
  original.RNGkind <- RNGkind("L")

  # Set the random seed
  if (!is.null(seed)) {
    stopifnot(is.numeric(seed))
    set.seed(as.integer(seed))
  }

  # Now calculate the scores over the bootstrap samples
  results <- list2df(mclapply(aList, cFun, mc.cores = as.integer(nJobs)))
  
  # Restore default RNG
  RNGkind(original.RNGkind[1])

  # If summary functions were requested
  if (!is.null(summarizeFun)) {

    stopifnot(is.vector(summarizeFun) & is.character(summarizeFun))

    # Build a list to work on
    sumFunList <- as.list(summarizeFun)
    names(sumFunList) <- summarizeFun

    # For checking the summarizing functions
    checkVector <- abs(rnorm(5, mean = 10))
    
    checkFun <- function(sFun) {

      # Make sure it's actually a function
      if (!inherits(get(sFun), "function"))
        stop("Invalid summarizing function: '", sFun, "' is not a function")
      
      # Verify the vector produces a scalar, numeric output
      check.sFun <- eval(call(sFun, checkVector))

      if (length(check.sFun) != 1)
        stop("Invalid summarizing function: '", sFun,
             "' does not return scalar values")

      return(NULL)
    }

    # Check the summarizing functions
    # Much ado about nothing
    nothing <- lapply(sumFunList, checkFun)

    # Throw in a dummy column to keep the output from collapsing in the event
    # there is only one column that will be summarized
    # Make sure the dummy name doesn't duplicate one of the column names
    dum <- make.names(c(colnames(results), "dummy"), unique = TRUE)
    dummyName <- dum[length(dum)]
    results[, dummyName] <- abs(rnorm(NROW(results), mean = 10))

    # The summarising function
    summarizingFun <- function(sFun) {

      # by group summary
      out <- list2df(by(results[, -which(colnames(results) %in%
                                         c("signatureID", "bootstrapSample"))],
                        results$signatureID,
                        function(x) sapply(x, get(sFun)), simplify = FALSE))

      # Make the signatureID its own column
      out <- cbind(data.frame(signatureID = rownames(out)), out)
      rownames(out) <- NULL

      # Remove the dummy column and return
      return(out[, -which(colnames(out) == dummyName)])

    } # summarizingFun

    # Calculate the summaries over the bootstrap samples
    results <- lapply(sumFunList, summarizingFun)

  } # If summary functions were provided

  # If no summaries
  else {

    # Move 'signatureID' and 'bootstrapSample' columns to the left
    statCols <- colnames(results)[!(colnames(results) %in% c("signatureID", "bootstrapSample"))]
    results <- results[,c("signatureID", "bootstrapSample", statCols)]
    
    # Final sort
    results <- sort.data.frame(results, ~signatureID + bootstrapSample)

  }
    
  # Return results
  return(results)
  
} # bootStrap

