##' The single attribute utility function that returns its argument
##'
##' @export
##' @param z A numeric vector of attribute measurements, or values, in their native scale
##' 
##' @return An object of class \code{saUtilCall}, which is equal to \code{z}. This object can be
##' printed or plotted via \code{\link{print.saUtilCall}} or \code{\link{plot.saUtilCall}}.
##' 
##' @examples
##' # Some "data"
##' x <- 1:10
##' # Identity utility
##' saUtil_identity(x)

saUtil_identity <- function(z) {

  # Check the inputs
  Smisc::stopifnotMsg(is.vector(z) & is.numeric(z),
                      "'z' must be a numeric vector")
  
  # Calculate the utility as requested
  util <- z

  # Set the attributes
  attributes(util) <- c(attributes(util),
                        list(saUtilFun = "saUtil_identity"),
                             parms = list(z = z))
  # Set class
  class(util) <- c("saUtilCall", class(util))

  # Restore names
  if (!is.null(nz <- names(z))) {
    names(util) <- nz
  }
  
  # Return the utility function and theta
  return(util)

} # saUtil_identity
