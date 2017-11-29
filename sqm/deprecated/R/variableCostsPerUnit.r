#' Calculates the Variable Costs per Unit (VCU)
#'
#' TODO
#'
#' The rows of the signatureConsumables data.frame index the consumables used in the
#' processes to undertake the signature.
#'
#' The signatureConsumables data.frame that is passed must have the following 2 columns:
#' \itemize{
#'   \item price
#'   \item quantity
#' }
#'
#' @param signatureConsumables TODO
#' @return TODO
#' @export
variableCostsPerUnit <- function(signatureConsumables) {
  costPerConsumable <- with(signatureConsumables,
                            calculateVariableCosts(p = price, x = quantity)
                            )
  sum(costPerConsumable)
}


#' The formula to calculate Variable Costs per Unit (VCU).
#'
#' TODO
#'
#' @param p price of consumable
#' @param x quantity of consumable
#' @return the variable costs per unit for a given consumable
calculateVariableCosts <- function(p, x) {
  p * x
}
