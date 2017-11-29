##' Lists the available single attribute utility methods
##'
##' This function enumerates the single attribute utility function methods
##'
##' By default, this function returns a vector with all of the available single attribute
##' utility methods that are used within the SQM package. This function also acts as a
##' lookup to determine if a particular score method is available. If 'method'
##' is specified, then a lookup is performed, and either TRUE or FALSE is returned
##' to indicate the presence or absence of the given 'method, respectively.
##' 
##' @export saUtilMethods
##' @param method a character string for single attribute utility  method. If no value is
##' specified, then all available score methods are returned. 
##' @return By default, a vector with all available single attribute utility methods.

saUtilMethods <- function(method = NULL) {

  methods <- c("expUtil", "logUtil")
    
  if (is.null(method)) {
    return(methods)
  } else {
    return(method %in% methods)
  }

} # saUtilMethods


