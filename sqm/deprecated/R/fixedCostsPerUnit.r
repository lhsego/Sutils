#' Calculates the Fixed Costs per Unit (FCU).
#'
#' TODO
#'
#' Fixed costs are any lump sum cost that does not change as the number of
#' signatures change. Examples of fixed costs include overhead charges, an
#' annual calibration cost or annual maintenance costs.
#'
#' The rows of the signatureFixedCosts data.frame index the fixed costs used in the
#' processes to undertake the signature.
#'
#' The signatureFixedCosts data.frame that is passed must have the following 3 columns:
#' \itemize{
#'   \item fixedCost
#'   \item qtyProcessRate
#' }
#'
#' @param signatureFixedCosts TODO
#' @return TODO
#' @export
fixedCostsPerUnit <- function(signatureFixedCosts) {
  costPerFixedCosts <- with(signatureFixedCosts,
                         calculateFixedCosts(F = fixedCosts, q = qtyProcessRate)
                         )
  sum(costPerFixedCosts)
}


#' The formula to calculate the Fixed Costs per Unit (FCU).
#'
#' TODO
#'
#' @param F fixed costs during year
#' @param q quantity of signatures processed during hours required
#' @return the fixed costs per unit for a given fixed cost
calculateFixedCosts <- function(F, q) {
  F / q
}
