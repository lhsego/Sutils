#' Signature Quality Metrics (SQM): Utility Component
#'
#' A key goal of the SQM framework is to compare the efficacy of a variety
#' of signatures. One important comparison metric is the utility of each
#' signature. This function computes the utility for each signature from a set of
#' input files and is intended to be the end-user function for calculating
#' utility for a set of attributes from a variety of signatures.
#'
#' The first column of each file should contain an identifier to distinguish each
#' signature. The signature identifier should be unique. We refer to this
#' signature identifies as \code{signatureID}.
#'
#' For each attribute, a univariate utility function must be determined. To
#' determine the fit, a variable's (practical) minimum and maximum values must
#' be specified as well as values that must be fit. Only one row should
#' correspond to a given variable. To fit these functions, the data.frame
#' \code{varUtility} must supply the following columns:
#'
#' \itemize{
#'   \item varName - Name of the variable
#'   \item xmin - Variable's minimum value.
#'   \item xmax - Variable's maximum value.
#'   \item x0 - Value of the variable with specified utility value in c0.
#'   \item y0 - Specified utility value for the variable value, x0.
#'   \item weights - Coefficient that will be used to aggregate the utility
#'   scores
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
#' A list is returned containing three elements:
#'
#' \enumerate{
#'   \item obsUtility - list
#'   \item aggregate - data.frame
#'   \item varUtilityFit - data.frame
#' }
#'
#' The list in \code{obsUtility} stores in \code{byVar} the utility values for
#' each observation's variables. The linear combination of these utility values
#' for each variable is given in \code{utility}. In other words, the fitted
#' utility functions are applied to the data given in \code{signatures} and
#' returned in a matrix, \code{byVar}. The linear combination of each row of
#' \code{byVar} is stored in \code{utility} with weights given in \code{weights}.
#'
#' The returned data.frame \code{aggregate} provides the aggregation of the
#' utility scores for each signature. More specifically, we calculate the
#' arithmetic mean of the utility values for the observations within each signature.
#'
#' The returned data.frame \code{varUtilityFit} contains the fitted arguments for
#' the utility function for each variable.
#'
#' The list returned from \code{afUtility} will be the same whether or not the
#' results are exported to files. However, if an output file is specified,
#' then the results will be \code{invisible}. See \code{\link{invisible}} for
#' details.
#'
#' If an output file prefix is provided in \code{outputFilePrefix}, then an
#' output will be created for each of the elements in the named list above. Note
#' that the output files will be prepended with the string given in
#' \code{outputFilePrefix} followed by a hyphen ('_'). For more details about
#' the outputted files, see the \code{writeUtility} function.
#'
#' The input file should be in the CSV format. Likewise, the output files will be
#' in the CSV format.
#'
#' @export
#' @param signaturesFile string with the file name containing the measured
#' attributes for a set of signatures.
#' @param utilityAttributesFile string with the file name containing the
#' attributes and the required arguments to determine the utility function will
#' be used for each attribute.
#' @param standardizeWeights logical. If \code{TRUE} (by default), we standardize
#' the utility function weights (coefficients) so that they sum to one.
#' @param outputFilePrefix string with the filename prefix that will be used for
#' each of the output files where the utility results will be stored. If
#' \code{NULL} (default), then the results are not exported to files. See details.
#' @param digits the number of digits after the decimal to use in the results
#' that are outputted to files. If NULL, no rounding is performed.
#' @return a list that contains the utility results for each each signature. See
#' Details.
#' @examples
#' # For this example, we construct a set of signatures with arbitrary names
#' # and values. Also, we provide the varUtility data.frame for these values.
#' # This example include 3 observations from each of 5 signatures. Each
#' # observation has 4 measured variables.
#' set.seed(42)
#' varNames <- c("var1", "var2", "var3", "var4")
#' x0 <- c(2, 20,  50,  100)
#' y0 <- c(0.25, 0.4, 0.6, 0.8)
#' xmin <- c(1, 10, 30, 80)
#' xmax <- c(5, 25, 60,  105)
#' weights <- seq_along(varNames)
#' varUtility <- cbind.data.frame(varNames, x0, y0, xmin, xmax, weights)
#' signatures <- t(replicate(15, runif(4, varUtility$xmin, varUtility$xmax)))
#' signatures <- data.frame(signatures)
#' colnames(signatures) <- varNames
#' signatures <- cbind(signatureID = gl(5, 3),  signatures)
#' 
#' # Now that we have generated our example data, we write the data to two files to
#' # emulate a real application of the \code{afUtility} function.
#' write.csv(signatures, file = "signatures.csv", quote = FALSE,
#'           row.names = FALSE)
#' write.csv(varUtility, file = "utilityAttributes.csv", quote = FALSE,
#'           row.names = FALSE)
#' 
#' # Next, we call the \code{afUtility} function to read in the generated data
#' # and calculate the utility results for each attribute.
#' utilityResults <- afUtility(signaturesFile = "signatures.csv",
#'                             utilityAttributesFile = "utilityAttributes.csv",
#'                             outputFilePrefix = "utilityResults")
#' 
#' # Notice that four additional CSV files have been created with the utility
#' # results. These have been created in your working directory and can be
#' # deleted. We have not deleted them automatically, so that you may view
#' # their output, if so desired. Notice that the returned object from the
#' # \code{afUtility} function matches the results in the outputted CSV files.
#' utilityResults
afUtility <- function(signaturesFile, utilityAttributesFile,
                      standardizeWeights = TRUE, outputFilePrefix = NULL,
                      digits = NULL) {
  # Reads the signature events from the specified input files that will be used
  # to calculate the utility for each signature.
  utilityFileInput <- readUtility(signaturesFile, utilityAttributesFile)
  
  # For each signature, we calculate the aggregated utility score. The results
  # are stored in a the data.frame, 'utilityResults'.
  utilityResults <- with(utilityFileInput,
                         calcUtility(signatures = signatures,
                                     varUtility = varUtility,
                                     standardizeWeights = standardizeWeights
                                     )
                         )

  # If a file name is specified for output, we write the results to the file and
  # return the results invisibly. Otherwise, we return the results visibly.
  if (!is.null(outputFilePrefix)) {
      writeUtility(utilityResults = utilityResults,
                   outputFilePrefix = outputFilePrefix, digits = digits)
      invisible(utilityResults)
  } else {
    utilityResults
  }

}
