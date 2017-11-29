##' The single attribute exponential utility function
##'
##' @details
##' The exponential utility function may be increasing or decreasing (depending on the choice of
##' \code{urange}), and concave, convex, or linear (depending on the choice of \code{theta}).
##'
##' @export
##' @param z A numeric vector of attribute measurements, or values, in their native scale
##' 
##' @param zrange A vector with 2 elements containing the minimum and maximum possible values of the attribute.
##' 
##' @param urange A vector with 2 elements indicating the range of the utility function. \code{urange[1]}
##' is the mapping of \code{zrange[1]}, and \code{urange[2]} is the mapping of \code{zrange[2]}.
##' 
##' @param theta A parameter that controls the convexity of the exponential utility function. For \code{theta > 0}
##' the utility is convex, for \code{theta < 0}, concave, and for \code{theta = 0}, it's linear.
##' 
##' @param certEquiv A numerical value indicating the certainty equivalent, which, if provided, will
##' be used to solve for the value of \code{theta}--and that solution will overide the value of \code{theta}
##' in the function call. See Section 3.5.1.1 of the reference for more discussion on the certainty equivalent.
##' 
##' @return An object of class \code{saUtilCall}, which includes the utility values of \code{z}. This object can be
##' printed or plotted via \code{\link{print.saUtilCall}} or \code{\link{plot.saUtilCall}}.
##' 
##' @references Holmes AE, Sego LH, Webb-Robertson BM, et al.  "An Approach for
##' Assessing the Signature Quality of Various Chemical Assays when
##' Predicting the Culture Media Used to Grow Microorganisms."  Technical
##' Report, Pacific Northwest National Laboratory, PNNL-22126.  February, 2013.
##' 
##' @examples
##' # Some "data"
##' x <- 1:10
##' # Increasing utility
##' saUtil_exp(x, theta = 3)
##' 
##' # Decreasing utility
##' u <- saUtil_exp(x, theta = 3, urange = c(1, 0))
##' print(u)
##' plot(u)
##' attributes(u)
##' 
##' # Obtain the value of theta using the certainty equivalent
##' theta0 <- attributes(saUtil_exp(x, certEquiv = 4))$parms$theta
##' print(theta0)

saUtil_exp <- function(z, theta = 0, zrange = range(z), urange = c(0, 1), certEquiv = NULL) {

  # Check the inputs
  Smisc::stopifnotMsg(is.vector(z) & is.numeric(z),
                      "'z' must be a numeric vector",
                      
                      is.numeric(theta) & (length(theta) == 1),
                      "'theta' must be a single numeric value",
                      
                      if (is.numeric(zrange) & (length(zrange) == 2)) {
                        zrange[1] < zrange[2]
                      } else FALSE,
                      "'zrange' must be a numeric vector of length 2 with zrange[1] < zrange[2]",
                      
                      is.numeric(urange) & (length(urange) == 2),
                      "'urange' must be a numeric vector of length 2",
                      
                      if (!is.null(certEquiv)) {
                        is.numeric(certEquiv) & (length(certEquiv) == 1)
                      } else TRUE,
                      "'certEquiv' must be a single numeric value")

  # Define variables we'll use later
  gamma <- sign(diff(urange))
  
  # If the certainty equivalent is provided, it will override the theta
  if (!is.null(certEquiv)) {

    # Sanity check
    if ((certEquiv < zrange[1] + 1e-08) | (certEquiv > zrange[2] - 1e-08))
      stop("'certEquiv' must be in zrange: [", paste(zrange, collapse = ", "), "]")
  
    # Midpoint for z
    z.t <- mean(zrange)
  
    # When the certEquiv is equal to the midpoint of z, its linear
    if (abs(certEquiv - z.t) < 1e-08)
      theta <- 0

    # If it's not linear
    else {

      # The midpoint of u
      u.t <- mean(urange)
      z.star <- Smisc::linearMap(certEquiv, D = zrange)
        
      # Define the exponential single attribute utility as a function of theta
      saUtil.obj <- function(theta) {
        u.z <- (exp(gamma * theta * z.star) - 1) / (exp(gamma * theta) - 1)
        return(Smisc::linearMap(u.z, D = c(0, 1), R = urange) - u.t)
      }
      
      # A Boolean for whether the range of theta should be positive (or negative)
      theta.range.positive <- (((certEquiv > z.t) & (gamma == 1)) | ((certEquiv < z.t) & (gamma == -1)))
    
      # To determine whether theta.star is large enough,
      # verify the endpoints of the interval do not have the same sign
      theta.star <- 10
      iter <- 0
    
      while (iter < 10^4) {
    
        theta.interval <- c(1e-08, theta.star)
    
        # Reverse the interval if the range of theta is negative
        if (!theta.range.positive) {
          theta.interval <- rev(-1 * theta.interval)
        }
    
        # If the sign of the objective function at the endpoints are not opposite
        # then expand the inverval
        if (sum(sign(saUtil.obj(theta.interval))) != 0) {
          theta.star <- theta.star + 10
        } 
        else {
          break
        }
    
        # Increment the counter
        iter <- iter + 1
    
      } # while
    
      # Now solve for theta using the most recent value of theta.interval
      theta <- uniroot(saUtil.obj, theta.interval)$root
    
    } # else it's not linear
    
  } # if a certainty equivalent was provided


  # Define the utility function
  util_fun <- function(Z) {
  
    # Now calculate the exponential utility
    z.star <- Smisc::linearMap(Z, D = zrange)
    
    # If theta is not 0
    if (abs(theta) > 1e-08) {
        u.z <- (exp(gamma * theta * z.star) - 1) / (exp(gamma * theta) - 1)
    }
    
    # If theta is 0, make it linear
    else { 
      u.z <- z.star
      theta <- 0
    }
  
    # Calculate the utility
    return(Smisc::linearMap(u.z, D = c(0, 1), R = urange))

  } # util_fun

  # If certainty equivalent was not provided, and if urange is [0, 1] or [1, 0], solve for it now
  if (is.null(certEquiv)) {

    if (all(sort(urange) == c(0, 1)))
      certEquiv <- uniroot(function(x) util_fun(x) - 0.5, zrange)$root

  }
  
  # Calculate the utility as requested
  util <- util_fun(z)

  # Set the attributes
  attributes(util) <- c(attributes(util),
                        list(saUtilFun = "saUtil_exp",                        
                             parms = list(z = z, theta = theta, zrange = zrange, urange = urange,
                                          certEquiv = certEquiv)))
  # Set class
  class(util) <- c("saUtilCall", class(util))

  # Restore names
  if (!is.null(nz <- names(z)))
    names(util) <- nz
  
  # Return the utility function and theta
  return(util)

} # saUtil_exp
