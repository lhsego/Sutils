#' Reads the signatures and utility fitting arguments from given files.
#'
#' For given files, this function reads in the signatures and the utility fit
#' parameters with which we will calculate the corresponding utility for each
#' signature.
#'
#' Each row of the file specified in \code{varUtilityFile} corresponds to each
#' variable provided in the file specified in 'signaturesFile'.  For each
#' variable, a univariate utility function must be determined. To determine the
#' fit, a variable's (practical) minimum and maximum values must be specified as
#' well as values that must be fit. Only one row should correspond to a given
#' variable. To fit these functions, we require the following columns in
#' \code{varUtilityFile} for each variable:
#'
#' \itemize{
#'   \item varName - Name of the variable
#'   \item min - Variable's minimum value.
#'   \item max - Variable's maximum value.
#'   \item x0 - Value of the variable with specified utility value in c0.
#'   \item y0 - Specified utility value for the variable value, x0.
#'   \item weights - Coefficient that will be used to aggregate the utility
#'   \item increasing - TRUE/FALSE. Should the utility function be increasing?
#' }
#'
#' The values specified in \code{x0} and \code{y0} are used to determine the
#' 'best' fit for the utility functions.
#'
#' The \code{weights} are coefficients used to calculate a linear combination of
#' the utility functions for a set of variables. By default, the weights are
#' standardized so that they sum to one. This can be disabled by setting
#' \code{standardizeWeights} to FALSE.
#'
#' We require that the input files are in the CSV file format (i.e. the file
#' delimiter is a comma).
#'
#' @export
#' @param signaturesFile string with the file name corresponding to the measured
#' variables for each signature. The column headers should be given for this
#' file: the first column header should be 'signatureID', while the remaining
#' column headers should name the variables.
#' @param varUtilityFile string with the filename corresponding to the inputs to
#' fit the utility functions for each variable. Each row corresponds to each
#' variable given in the 'signaturesFile'. Column headers should not be given
#' in this file.
#' @return a list with five data.frame corresponding to the inputted costs for
#' each signature.
readUtility <- function(signaturesFile, varUtilityFile) {
  signatures <- read.csv(signaturesFile, header = TRUE, stringsAsFactors = FALSE)

  varUtility <- read.csv(varUtilityFile, header = TRUE,
                                stringsAsFactors = FALSE)

  list(signatures = signatures, varUtility = varUtility)
}
