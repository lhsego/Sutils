#' Calculates the Intangible Costs Per Unit (ICU).
#'
#' TODO: Add brief description of ICU.
#'
#' Intangible costs include intangibles such as goodwill (for signatures an
#' element that makes the signature more or less valuable, e.g., a patent.
#' Nuisance calls would be an additional cost.)
#'
#' The rows of the signatureIntangibleAssets data.frame index the intangible assets
#' involved in undertaking the signature.
#'
#' The signatureIntangibleAssets data.frame that is passed must have the
#' following four columns:
#'
#' \itemize{
#'   \item cost
#'   \item discountRate
#'   \item yearsLife
#'   \item qtyProcessRate (in years)
#' }
#'
#' @param signatureAssets TODO
#' @return TODO
#' @export
intangibleCostsPerUnit <- function(signatureIntangibleAssets) {
  costPerIntangibleAsset <- with(signatureIntangibleAssets,
                       calculateIntangibleCost(I = cost, d = discountRate,
                                              n = yearsLife,q = qtyProcessRate)
                      )
  sum(costPerIntangibleAsset)
}

#' The formula to calculate the Intangible Costs per Unit (ICU).
#'
#' TODO
#'
#' @param I cost
#' @param d discount rate
#' @param n number of years of life
#' @param q quantity of signatures that can be processed during a year
#' @return the annualized development and deployment cost per unit for a given
#' asset
calculateIntangibleCosts <- function(I, d, n, q) {
  (I * d * (1 - d)^n) / ((1 - d)^n - 1) / q
}
