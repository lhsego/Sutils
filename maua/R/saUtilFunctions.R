##' Lists the available single attribute utility functions
##'
##' By default, this function returns a vector with all of the available single attribute
##' utility functions that are available in the \code{maua} package. It can also be used to
##' verify that a single attribute utility function is available in the package.
##'
##' @details 
##' All the single attribute
##' utility functions are of the form \emph{saUtil_name} where \emph{name} is description of the function. Either
##' the full function name or the abbreviated, description, \emph{name}, can be supplied to the  \code{saUtilFun} argument.
##'
##' The existence of \code{saUtilFunctions()} facilitates the addition of new single attribute utility functions to the package.
##' 
##' @export 
##' @param saUtilFun If not \code{NULL}, this should be a character string with the full name of the single attribute utility function,
##' or the abbreviated name that describes the function.  See Examples.
##' 
##' @return By default, a vector with all available single attribute utility functions, or if a single attribute utility function
##' is provided to \code{saUtilFun}, the name of the function is returned if it exists.  Otherwise an error is thrown.
##'
## @seealso \code{\link{fidelFunctions}}
##'
##' @examples
##' saUtilFunctions()
##'
##' # The single attribute exponential utility function.
##' # Note how both the full name of the function, "saUtil_exp" and
##' # its abbreviation both work:
##' saUtilFunctions("saUtil_exp")
##' saUtilFunctions("exp")

saUtilFunctions <- function(saUtilFun = NULL) {

  Smisc::stopifnotMsg(if (!is.null(saUtilFun)) {
                        is.character(saUtilFun) & (length(saUtilFun) == 1)
                      } else TRUE,
                      "'saUtilFun' must be NULL or a single character string indicating the single attribute utility function")

  # The single attribute utility functions currently available in the package
  saUtilFuns <- c("saUtil_exp", "saUtil_log", "saUtil_linear", "saUtil_identity")

  if (is.null(saUtilFun)) {
    return(saUtilFuns)
  }
  else {

    # The possible values that can be supplied to 'saUtilFun'
    possibleVals <- c(saUtilFuns, sub("saUtil_", "", saUtilFuns))

    # Verify what was provided is in the possible values
    res <- Smisc::selectElements(saUtilFun, possibleVals)

    # Add the 'saUtil_' prefix if needed
    if (substr(res, 1, 7) != "saUtil_") {
       res <- paste("saUtil", res, sep = "_")
    }

    return(res)
    
  }

} # saUtilFunctions
