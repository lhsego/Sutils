##' The R Implementation of the SQM Analytic Framework
##'
##' The SQM package contains the R code for the Signature Quality Metrics project
##' under the Signature Discovery Initiative (SDI) at Pacific Northwest
##' National Laboratory (PNNL). The purpose of the SQM project is to provide
##' subject-matter experts (SMEs) with a set of tools to assess the quality of a
##' set of signatures in terms of accuracy, cost, risk, and utility. For each of
##' these components, we have provided easy-to-use, accessible functions to
##' measure the quality of signatures.
##'
##' @docType package
##' @name SQM
##' @aliases SQM package-SQM
##' @useDynLib SQM
##' @examples
##' \dontrun{
##' # To run all the test scripts in the SQM package, type
##' example(SQM)
##' 
# Functions that are called when SQM package is loaded or unloaded

.onLoad <- function(libname, pkgname) {

  banner.text <- paste("\nWelcome to the SQM package, version ",
                       packageDescription("SQM", fields = "Version"), ".\n\n", sep = "")

  packageStartupMessage(banner.text)
  
}

# This function would be called when namespace is unloaded (if it were not in the search path
# because it had been loaded via loadNamespace("SQM")
.onUnload <- function(libpath) {

  if (is.loaded("discreteLoss", PACKAGE = "SQM")) {
  
    confirm <- try(library.dynam.unload("SQM", libpath), silent = TRUE)
    
    if (class(confirm) != "try-error")
      cat("SQM shared objects are unloaded\n")
    else
      cat(confirm,"\n")
  }
}

