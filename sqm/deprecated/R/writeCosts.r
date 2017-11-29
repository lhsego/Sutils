#' Writes the calculated costs for each signature to a file.
#'
#' This function writes the calculated costs summary for a set of signatures to
#' a file. The ouptput file contains the Annualized Development and Deployment
#' Cost per Unit (DDCU), Intangible Costs per Unit (ICU), Labor Cost per Unit
#' (LCU), Variable Costs per Unit (VCU), Fixed Costs per Unit (FCU),
#' operational costs (LCU + VCU + FCU), and the total costs (DDCU + ICU + LCU +
#' VCU) for each signature.
#'
#' Each row of the outputted file corresponds to a signature and its calculated
#' costs.
#'
#' The output file format is CSV (i.e. the file delimiter is a comma).
#'
#' @export
#' @param accuracyResults the computed accuracy results for each signature
#' @param outputFilePrefix string of the output file where the accuracy results
#' will be stored
#' @param digits the number of digits after the decimal to use in outputted
#' results. If NULL, no rounding is performed.
#' @return nothing
writeCosts <- function(signatureCosts, outputFile, digits = NULL) {
  # If specified, round the results to the specified number of 'digits'.
  if (!is.null(digits)) {
    signatureCosts$DDCU <- round(signatureCosts$DDCU, digits = digits)
    signatureCosts$ICU <- round(signatureCosts$ICU, digits = digits)
    signatureCosts$LCU <- round(signatureCosts$LCU, digits = digits)
    signatureCosts$VCU <- round(signatureCosts$VCU, digits = digits)
    signatureCosts$FCU <- round(signatureCosts$FCU, digits = digits)
    signatureCosts$operationalCosts <- round(signatureCosts$operationalCosts,
                                             digits = digits)
    signatureCosts$totalCosts <- round(signatureCosts$totalCosts,
                                       digits = digits)
  }
  
  write.csv(signatureCosts, file = aggregateFilename, quote = F, row.names = F)
}

