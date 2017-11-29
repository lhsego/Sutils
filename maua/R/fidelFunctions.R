##' Lists the available functions that measure classifier fidelity
##'
##' By default, this function returns a vector with all of the available fidelity
##' functions that are available in the \code{maua} package. It can also be used to
##' verify that a fidelity function is available in the package.
##'
##' @details 
##' All the single attribute
##' utility functions are of the form \emph{fidel_name} where \emph{name} is description of the function. Either
##' the full function name or the abbreviated, description, \emph{name}, can be supplied to the \code{fidelFun} argument.
##'
##' \code{fidelFunctions()} facilitates the addition of new single attribute utility functions to the package.
##' 
##' @export 
##' @param saUtilFun If not \code{NULL}, this should be a character string with the full name of the fidelity function,
##' or the abbreviated name that describes the function.  See Examples.
##' 
##' @return By default, a vector with all available fidelity functions, or if a single fidelity function
##' is provided to \code{sa}, the name of the function is returned if it exists.  Otherwise an error is thrown.
##'
##' @seealso \code{\link{saUtilFunctions}}
##' 
##' @examples
##' fidelFunctions()
##'
##' # The single attribute exponential utility function.
##' # Note how both the full name of the function, "fidel_exp" and
##' # its abbreviation both work:
##' fidelFunctions("fidel_exp")
##' fidelFunctions("exp")

fidelFunctions <- function(fidelFun = NULL) {

  Smisc::stopifnotMsg(if (!is.null(fidelFun)) {
                        is.character(fidelFun) & (length(fidelFun) == 1)
                      } else TRUE,
                      "'fidelFun' must be NULL or a single character string indicating the fidelity function")

  # The single attribute utility functions currently available in the package
  fidelFuns <- c("fidel_", "fidel_log", "fidel_linear", "fidel_identity")

  if (is.null(fidelFun)) {
    return(fidelFuns)
  }
  else {

    # The possible values that can be supplied to 'fidelFun'
    possibleVals <- c(fidelFuns, sub("fidel_", "", fidelFuns))

    # Verify what was provided is in the possible values
    res <- Smisc::selectElements(fidelFun, possibleVals)

    # Add the 'fidel_' prefix if needed
    if (substr(res, 1, 7) != "fidel_") {
       res <- paste("fidel", res, sep = "_")
    }

    return(res)
    
  }

} # fidelFunctions
