# Find the level of maxAbsError and maxRelError for which two dataframes or matrices are equivalent



##' Finds the level of numerical equivalence of two dataframes or matrices
##'
##' Wrapper function for \code{dframeEquiv} that identifies the smallest
##' \code{maxAbsError} and \code{maxRelError} for which two data frames or
##' matrices are still equivalent
##'
##' The function tests the equivalence of \code{d1} and \code{d2} over the
##' matrix of combinations of \code{maxAbsError} and \code{maxRelError}, each
##' with values \code{1e-minPrec, 1e-(minPrec+1), ..., 1e-(maxPrec-1),
##' 1e-maxPrec}.
##'
##' @export find.dframeEquiv
##' @param d1 The first dataframe or matrix
##' @param d2 The dataframe or matrix that will be compared to \code{d1}
##' @param minPrec An integer ranging from 0 to 22 indicating the smallest
##' exponent in \code{maxAbsError} and \code{maxRelError} that will be
##' considered
##' @param maxPrec An integer ranging from 1 to 22 indicating the largest
##' exponent in \code{maxAbsError} and \code{maxRelError} that will be
##' considered.  It must be larger than \code{minPrec}.
##' @param returnMatrix \code{=TRUE} returns a boolean matrix indicating the
##' result of the comparison for the various combinations of \code{maxAbsError}
##' and \code{maxRelError}
##' @return If \code{d1} is not equal to \code{d2} when
##' \code{maxAbsError=1e-minPrec} and \code{maxRelError=1e-minPrec} then
##' \code{NULL} is returned, along with the message that describes the
##' inequality. Otherwise, a list with the following components is returned:
##'
##' \item{leastAbsError}{A named vector indicating values of the the smallest
##' \code{maxAbsError} and the corresponding \code{maxRelError} that resulted in
##' equivalence} \item{leastRelError}{A named vector indicating values of the
##' the smallest \code{maxRelError} and the corresponding \code{maxAbsError}
##' that resulted in equivalence} \item{returnMatrix}{A boolean matrix whose
##' rows (and rownames) correspond to the exponents of the \code{maxAbsError}
##' and whose columns (and colnames) correspond to the exponents of the
##' \code{maxRelError}.  Values of \code{TRUE} indicate combinations of
##' \code{maxMaxError} and \code{maxRelError} for which the two
##' dataframes/matrices were equivalent.}
##' @author Landon Sego
##' @seealso \code{\link{dframeEquiv}}
##' @keywords misc
##' @examples
##'
##' # Number of rows different
##' a <- matrix(rnorm(20), nrow=4)
##' b <- a[1:3,]
##' find.dframeEquiv(a, b)
##'
##' # Equivalent
##' x <- data.frame(x=letters[1:6], y=rnorm(6), z=rnorm(6))
##' y <- x
##' find.dframeEquiv(x, y)
##'
find.dframeEquiv <- function(d1, d2,  minPrec = 5, maxPrec = 15, returnMatrix = FALSE) {

  # This is a dumb and slow algorithm, but it works

  # Checks for maxPrec and minPrec
  if (maxPrec %% 1) {
    maxPrec <- round(maxPrec)
    warning("'maxPrec' converted to an integer value of ", maxPrec)
  }

  if (minPrec %% 1) {
    minPrec <- round(minPrec)
    warning("'minPrec' converted to an integer value of ", minPrec)
  }

  if (!(maxPrec %in% 1:22))
    stop("'maxPrec' should be an integer in 1:22\n")


  if (!(minPrec %in% 0:22))
    stop("'minPrec' should be an integer in 0:22\n")

  if (maxPrec <= minPrec)
    stop("'maxPrec' must be greater than 'minPrec'\n")


  # Checks for returnMatrix
  if (!is.logical(returnMatrix))
    stop("'returnMatrix' must be 'TRUE' or 'FALSE'\n")

  # Initialize output
  leastAbsError <- NULL
  leastRelError <- NULL

  # Verify equivalence on the least level before diving in
  firstcomp <- dframeEquiv(d1, d2, maxAbsError=1/10^minPrec, maxRelError=1/10^minPrec, verbose=FALSE)

  if (firstcomp$equiv) {

    precVec <- minPrec:maxPrec
    n <- length(precVec)

    # Create matrix of equivalence values
    # maxAbsError on the rows, i
    # maxRelError on the columns, j
    equivM <- matrix(NA, ncol=n, nrow=n, dimnames=list(precVec, precVec))

    # Calculate the equivalencies
    for (i in 1:n)
      for (j in 1:n)
        equivM[i,j] <- dframeEquiv(d1, d2,
                                   maxAbsError=1/10^precVec[i],
                                   maxRelError=1/10^precVec[j],
                                   verbose=FALSE)$equiv


    # Find the least absolute error
    cont <- TRUE

    # Move backwards over the rows
    for (i in n:1) {
      # Move backwards over the columns within a row
      for (j in n:1) {
        if (equivM[i,j]) {
          leastAbsError <- c(AbsError=1/10^precVec[i], RelError=1/10^precVec[j])
          cont <- FALSE
          unknown.leastAbsError <- i == n
          break
        }
      } # for j
      if (!cont)
        break
    } # for i

    # Find the least relative error
    cont <- TRUE

    # Move over the columns
    for (j in n:1) {
      # Move over the rows within a column
      for (i in n:1) {
        if (equivM[i,j]) {
          leastRelError <- c(AbsError=1/10^precVec[i], RelError=1/10^precVec[j])
          cont <- FALSE
          unknown.leastRelError <- j == n
          break
        }
      } # for i
      if (!cont)
        break
    } # for j

    out <- list(leastAbsError=leastAbsError, leastRelError=leastRelError)

    if (returnMatrix)
      out[["returnMatrix"]] <- equivM

    if (unknown.leastAbsError)
      cat("The least absolute error is only known to be smaller than ",
          as.character(1/10^maxPrec),
          ".  You may want to increase the value of 'maxPrec'.\n\n", sep="")

    if (unknown.leastRelError)
      cat("The least relative error is only known to be smaller than ",
          as.character(1/10^maxPrec),
          ".  You may want to increase the value of 'maxPrec'.\n\n", sep="")

  } # if equivalent when maxAbsError=1 and maxRelError=1

  else {
    d1.d2 <- paste("'", deparse(substitute(d1)), "' and '",
                   deparse(substitute(d2)), "'", sep="")
    cat(d1.d2, "are not equivalent when maxAbsError =", as.character(1/10^minPrec),
        "and maxRelError =", as.character(1/10^minPrec),
        "\nMessage from the call to 'dframeEquiv':\n")
    print(firstcomp$msg)
    out <- NULL
  }

  out

} # find.dframeEquiv
