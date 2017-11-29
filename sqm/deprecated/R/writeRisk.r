#' Writes the risk estimates for each signature to a CSV file.
#'
#' We write the calculated risks for each signature to a comma-separated value
#' (CSV) file.
#'
#' In the output file, each row corresponds to a signature's \code{signatureID}.
#' For each row, the first column provides the \code{signatureID} and the second
#' column provides the \code{risk} estimate.
#'
#' The output file format is CSV (i.e. the file delimiter is a comma).
#'
#' @export
#' @param riskResults the computed risks for each signature
#' @param outputFile string of the output file where the risk results will be
#' stored
#' @param digits the number of digits after the decimal to use in outputted
#' results. If NULL, no rounding is performed.
#' @return nothing
writeRisk <- function(riskResults, outputFile, digits = NULL) {
  riskResults <- as.character(riskResults)
  
  # If specified, round the results to the specified number of 'digits'.
  if (!is.null(digits)) {
    riskResults <- round(riskResults, digits = digits)
  }
  
  write.csv(riskResults, file = outputFile, quote = F, row.names = F)
}
