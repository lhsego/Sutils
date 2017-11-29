##' Lists the available multiattribute utility methods
##'
##' This function enumerates the multiattribute utility function methods
##'
##' By default, this function returns a vector with all of the available multiattribute
##' utility methods that are used within the SQM package. This function also acts as a
##' lookup to determine if a particular score method is available. If 'method'
##' is specified, then a lookup is performed, and either TRUE or FALSE is returned
##' to indicate the presence or absence of the given 'method, respectively.
##'
##' The methods themselves need to operate in the following way: The should calculate
##' the multiattribute utility for each row of data frame of attributes, \code{Z}, and
##' they should return the data frame with the calculated utility added as an additional
##' column to the dataframe.
##' 
##' @rdname maUtilMethods
##' @export maUtilMethods
##' @param method a character string for multiattribute utility  method. If no value is
##' specified, then all available score methods are returned. 
##' @return By default, a vector with all available multiattribute utility methods.

maUtilMethods <- function(method = NULL) {

  methods <- c("addMultUtil")
    
  if (is.null(method)) {
    return(methods)
  } else {
    return(method %in% methods)
  }


} # maUtilMethods


