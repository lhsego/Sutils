##' The single attribute logarithmic utility function
##'
##' @details
##' The log utility function may be increasing or decreasing (depending on the choice of
##' \code{urange}).  Raw data values are shifted by \code{shift} prior to taking the log.
##' 
##' The log utility function is given by:
##' \emph{u(z) = urange[1] + (urange[2] - urange[1]) * A / B}
##' where \emph{A = log(z - shift) - log(zrange[1] - shift)} and
##' \emph{ B = log(zrange[2] - shift) - log(zrange[1] - shift)}.
##' 
##' Note \emph{u(zrange[1]) = urange[1]} and \emph{u(zrange[2]) = urange[2]}.
##'
##' @export
##' @param z A numeric vector of attribute measurements, or values, in their native scale
##' 
##' @param shift A numeric value that shifts the \code{z} values by \code{z - shift} before taking the log. One situtation where this
##' is useful is when one or more values of \code{z} are negative.
##' 
##' @param zrange A vector with 2 elements containing the minimum and maximum possible values of the attribute.
##' However, because the log function goes to \code{-Infty} as \code{z} goes to 0, \code{zrange[1]} fixes a
##' lower endpoint such that \emph{u(zrange[1]) = urange[1]}.
##' 
##' @param urange A vector with 2 elements indicating the range of the utility function. \code{urange[1]}
##' is the mapping of \code{zrange[1]}, and \code{urange[2]} is the mapping of \code{zrange[2]}.
##' 
##' @return An object of class \code{saUtilCall}, which includes the utility values of \code{z}.  This object can be
##' printed or plotted via \code{\link{print.saUtilCall}} or \code{\link{plot.saUtilCall}}.
##' 
##' @examples
##' # An identify
##' saUtil_log(exp(0:10), urange = c(0, 10))
##' 
##' # Just mapping the log(1:10) to (0, 1)
##' saUtil_log(1:10)
##' 
##' # Suppose we have seven classes, and p contains probabilities assigned
##' # to the true class for 5 observations
##' p <- c(0.3, 0.8, 0.99, 0.05, 1/7)
##' 
##' # This calculate the log score, but ensure that scores that do worse than
##' # guessing (i.e. uniform, 1/7) are mapped to a negative value
##' saUtil_log(p, zrange = c(1/7, 1))
##' 
##' # Example of the shift to move us away from NA and -Inf
##' x <- saUtil_log(-5:7, shift = -5.00001)
##' print(x)
##' plot(x)
##' attributes(x)

saUtil_log <- function(z,
                       shift = 0,
                       zrange = range(z),
                       urange = c(0, 1)) {

  # Check the inputs
  Smisc::stopifnotMsg(is.vector(z) & is.numeric(z),
                      "'z' must be a numeric vector",
                      
                      is.numeric(shift) & (length(shift) == 1),
                      "'shift' must be a single numeric value",
                      
                      if (is.numeric(zrange) & (length(zrange) == 2)) {
                        zrange[1] < zrange[2]
                      } else FALSE,
                      "'zrange' must be a numeric vector of length 2 with zrange[1] < zrange[2]",
                      
                      is.numeric(urange) & (length(urange) == 2),
                      "'urange' must be a numeric vector of length 2")
                      
  # Checks with dependencies
  Smisc::stopifnotMsg(all(z > shift),
                      "All values of 'z' must be greater than 'shift'",
                      all(zrange > shift),
                      "All values of 'zrange' must be greater than 'shift'")

  # Calculate utility
  util <- Smisc::linearMap(log(z - shift), D = log(zrange - shift), R = urange)

  # Set the attributes
  attributes(util) <- c(attributes(util),
                        list(saUtilFun = "saUtil_log",
                             parms = list(z = z, shift = shift, zrange = zrange, urange = urange)))
  # Set class
  class(util) <- c("saUtilCall", class(util))

  # Restore names
  if (!is.null(nz <- names(z)))
    names(util) <- nz
  
  # Return the object
  return(util)
  
} # saUtil_log

