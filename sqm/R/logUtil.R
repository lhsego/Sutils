##' The single attribute logarithmic utility function
##'
##' The single attribute (uni-dimensional) natural logarithmic utility function
##'
##' The log utility function is given by:
##' \deqn{u(z) = urange[1] + (urange[2] - urange[1]) * \frac{log(z - shift) -
##'       log(zrange[1] - shift)}{log(zrange[2] - shift) - log(zrange[1] - shift)}}
##' Note \eqn{u(zrange[1]) = urange[1])} and \eqn{u(zrange[2]) = urange[2])}.
##'
##' @export
##' 
##' @param z A vector of attribute measurements, or values, in their native scale
##' @param shift A numeric value that shifts the \code{z} values by \code{z - shift} before taking the log.
##' @param zrange A vector with 2 elements containing the minimum and maximum possible values of the attribute.
##' However, because the log function goes to \code{-Infty} as \code{z} goes to 0, \code{zrange[1]} fixes a
##' lower endpoint such that \eqn{u(zrange[1]) = urange[1]}.
##' @param urange A vector with 2 elements indicating the range of the utility function. \code{urange[1]}
##' is the mapping of \code{zrange[1]}, and \code{urange[2]} is the mapping of \code{zrange[2]}.
##' @return The utility values of \code{z}
##' 
##' @examples
##' # An identify
##' logUtil(exp(0:10), urange = c(0, 10))
##' 
##' # Just mapping the log to 0, 1
##' logUtil(1:10)
##' 
##' # Suppose we have seven classes, and p contains probabilities assigned
##' # to the true class for 5 observations
##' p <- c(0.3, 0.8, 0.99, 0.05, 1/7)
##' 
##' # This calculate the log score, but ensure that scores that do worse than guessing (i.e. uniform)
##' # are mapped to < 0
##' logUtil(p, zrange = c(1/7, 1))
##' 
##' # Example of the shift to move us away from NA and -Inf
##' x <- logUtil(-5:7, shift = -5.00001)
##' print(x)
##' plot(x)
##' attributes(x)

logUtil <- function(z,
                    shift = 0,
                    zrange = range(z),
                    urange = c(0, 1)) {

  # Check the ranges
  stopifnot(is.vector(z),
            is.numeric(z),
            is.numeric(shift),
            length(shift) == 1,
            is.numeric(zrange),
            is.numeric(urange),
            length(zrange) == 2,
            length(urange) == 2,
            zrange[1] < zrange[2],
            all(z > shift),
            all(zrange > shift))

  # Calculate utility
  util <- linearMap(log(z - shift), D = log(zrange - shift), R = urange)

  # Set the attributes
  attributes(util) <- c(attributes(util),
                        list(saUtilMethod = "logUtil",
                             parms = list(z = z, shift = shift, zrange = zrange, urange = urange)))
  # Set class
  class(util) <- c("saUtilCall", class(util))

  # Restore names
  if (!is.null(nz <- names(z)))
    names(util) <- nz
  
  # Return the object
  return(util)
  
} # logUtil

