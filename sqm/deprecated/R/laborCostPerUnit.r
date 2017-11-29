#' Calculates the Labor Cost per Unit (LCU)
#'
#' TODO 
#'
#' The rows of the signatureLaborers data.frame index the laborers employed in
#' undertaking the signature.
#'
#' The signatureLaborers data.frame that is passed must have the following 3 columns:
#' \itemize{
#'   \item hoursRecorded
#'   \item burdenedLaborRate
#'   \item qtyProcessRate
#' }
#'
#' @param signatureLaborers TODO
#' @return TODO
#' @export
laborCostPerUnit <- function(signatureLaborers) {
  costPerLaborer <- with(signatureLaborers,
                         calculateLaborCost(h = capital, w = burdenedLaborRate,
                                            q = qtyProcessRate)
                         )
  sum(costPerLaborer)
}


#' The formula to calculate Labor Cost per Unit (LCU).
#'
#' TODO
#'
#' @param h hours recorded
#' @param w burdened labor rate
#' @param q quantity of signatures processed during hours required
#' @return the labor cost per unit for a given laborer
calculateLaborCost <- function(h, w, q) {
  h * w / q
}
