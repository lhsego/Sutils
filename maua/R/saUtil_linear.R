##' The single attribute linear utility function
##'
##' @details
##' The linear function may be increasing or decreasing (depending on the choice of
##' \code{urange}).
##'
##' @export
##' @param z A numeric vector of attribute measurements, or values, in their native scale
##' 
##' @param zrange A vector with 2 elements containing the minimum and maximum possible values of the attribute.
##' 
##' @param urange A vector with 2 elements indicating the range of the utility function. \code{urange[1]}
##' is the mapping of \code{zrange[1]}, and \code{urange[2]} is the mapping of \code{zrange[2]}.
##' 
##' @return An object of class \code{saUtilCall}, which includes the utility values of \code{z}. This object can be
##' printed or plotted via \code{\link{print.saUtilCall}} or \code{\link{plot.saUtilCall}}.
##' 
##' @examples
##' # Some "data"
##' x <- 1:10
##' # Increasing utility
##' saUtil_linear(x)
##' 
##' # Decreasing utility
##' u <- saUtil_linear(x, urange = c(1, 0))
##' print(u)
##' plot(u)
##' attributes(u)

saUtil_linear <- function(z, zrange = range(z), urange = c(0, 1)) {

  # Check the inputs
  Smisc::stopifnotMsg(is.vector(z) & is.numeric(z),
                      "'z' must be a numeric vector",
                      
                      if (is.numeric(zrange) & (length(zrange) == 2)) {
                        zrange[1] < zrange[2]
                      } else FALSE,
                      "'zrange' must be a numeric vector of length 2 with zrange[1] < zrange[2]",
                      
                      is.numeric(urange) & (length(urange) == 2),
                      "'urange' must be a numeric vector of length 2")
  
  # Calculate the utility as requested
  util <- Smisc::linearMap(z, D = zrange, R = urange)

  # Set the attributes
  attributes(util) <- c(attributes(util),
                        list(saUtilFun = "saUtil_linear",                        
                             parms = list(z = z, zrange = zrange, urange = urange)))
  # Set class
  class(util) <- c("saUtilCall", class(util))

  # Restore names
  if (!is.null(nz <- names(z))) {
    names(util) <- nz
  }
  
  # Return the utility function and theta
  return(util)

} # saUtil_linear
