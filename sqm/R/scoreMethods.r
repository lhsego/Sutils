##' Lists the SQM classification score methods.
##'
##' This function enumerates the score methods available in the SQM package.
##'
##' By default, this function returns a vector with all of the available score
##' methods that are used within the SQM package. This function also acts as a
##' lookup to determine if a particular score method is available. If 'method'
##' is specified, then a lookup is performed, and either TRUE or FALSE is returned
##' to indicate the presence or absence of the given 'method, respectively.
##'
##' @export
##' @param method a character string for an score method. If no value is
##' specified, then all available score methods are returned. 
##' @return By default, a vector with all available score methods. 
scoreMethods <- function(method = NULL) {
  
    methods <- c("brierScore", "logScore")
    
  if (is.null(method)) {
    methods
  } else {
    method %in% methods
  }

}
