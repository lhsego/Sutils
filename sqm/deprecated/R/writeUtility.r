#' Writes the utility results for each signature to files.
#'
#' The utility results for a set of signatures is written to four files.
#'
#' The first file contains the utility scores for each observation (i.e. each row
#' of the inputted signatures) by feature; the filename begins with the string
#' provided in \code{outputFilePrefix} and is appended with "_obsUtility.csv".
#'
#' The second file contains the aggregated utility scores for each observation
#' (i.e. each row of the inputted signatures); the filename begins with the
#' string provided in \code{outputFilePrefix} and is appended with
#' "_obsUtilityAggregate.csv".
#' 
#' The third file contains the aggregated utility scores for each signature. This
#' file is the primary file necessary to compare the aggregate utility scores of
#' different signatures. Its filename begins with the string provided in
#' \code{outputFilePrefix} and is appended with "_aggregate.csv".
#'
#' The fourth file contains a summary of the fitted utility functions; the
#' filename begins with the string provided in \code{outputFilePrefix} and is
#' appended with "_utilityFitSummary.csv".
#'
#' The output file format is CSV (i.e. the file delimiter is a comma).
#'
#' @export
#' @param utilityResults the computed utility results for each signature. The
#' results are computed with the \code{calcUtility} function.
#' @param outputFilePrefix string of the output file where the utility results
#' will be stored
#' @param digits the number of digits after the decimal to use in outputted
#' results. If NULL, no rounding is performed.
#' @return nothing
writeUtility <- function(utilityResults, outputFilePrefix, digits = NULL) {
  obsUtilityFilename <- paste(outputFilePrefix, "obsUtility.csv", sep = "_")
  obsUtilityAggregateFilename <- paste(outputFilePrefix,
                                       "obsUtilityAggregate.csv", sep = "_")
  aggregateFilename <- paste(outputFilePrefix, "aggregate.csv", sep = "_")
  utilityFitSummaryFilename <- paste(outputFilePrefix,
                                     "utilityFitSummary.csv", sep = "_")

  # If specified, round the results to the specified number of 'digits'.
  if (!is.null(digits)) {
    utilityResults$obsUtility$utility <- round(utilityResults$obsUtility$utility,
                                               digits = digits)
    utilityResults$obsUtility$byVar <- round(utilityResults$obsUtility$byVar,
                                             digits = digits)
    utilityResults$aggregate <- round(utilityResults$aggregate, digits = digits)
    utilityResults$varUtilityFit <- round(utilityResults$varUtilityFit,
                                          digits = digits)
  }
  
  write.csv(utilityResults$obsUtility$utility, file = obsUtilityFilename,
            quote = FALSE, row.names = FALSE)
  write.csv(utilityResults$obsUtility$byVar, file = obsUtilityAggregateFilename,
            quote = FALSE, row.names = FALSE)
  write.csv(utilityResults$aggregate, file = aggregateFilename, quote = FALSE,
            row.names = FALSE)
  write.csv(utilityResults$varUtilityFit, file = utilityFitSummaryFilename,
            quote = FALSE, row.names = FALSE)
}

