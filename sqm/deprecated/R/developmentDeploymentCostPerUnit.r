#' Calculates the Annualized Development and Deployment Cost per Unit (DDCU).
#'
#' TODO: Add brief description of DDCU.
#'
#' Typical installed capital asset costs include:
#'
#' \itemize{
#'   \item Research and Development
#'   \item Implementation Cost
#'   \item Startup costs (e.g. training)
#'         (sometimes six months to year of operations)
#'   \item Design
#'   \item Permitting
#'   \item Construction
#'   \item Buildings
#'   \item Equipment
#'   \item Others
#' }
#'
#' The rows of the signatureAssets data.frame index the assets involved in
#' undertaking the signature.
#'
#' The signatureAssets data.frame that is passed must have the following four
#' columns:
#'
#' \itemize{
#'   \item capital
#'   \item discountRate
#'   \item yearsLife
#'   \item qtyProcessRate (in years)
#' }
#'
#' @param signatureAssets TODO
#' @return TODO
#' @export
developmentDeploymentCostPerUnit <- function(signatureAssets) {
  costPerAsset <- with(signatureAssets,
                       calculateDevDeployCost(K = capital, d = discountRate,
                                              n = yearsLife,q = qtyProcessRate)
                      )
  sum(costPerAsset)
}

#' The formula to calculate the Annualized Development and Deployment Cost per
#' Unit (DDCU).
#'
#' TODO
#'
#' @param K capital cost
#' @param d discount rate
#' @param n number of years of life
#' @param q quantity of signatures that can be processed during a year
#' @return the annualized development and deployment cost per unit for a given
#' asset
calculateDevDeployCost <- function(K, d, n, q) {
  (K * d * (1 - d)^n) / ((1 - d)^n - 1) / q
}
