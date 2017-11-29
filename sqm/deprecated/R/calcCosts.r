#' Calculates the costs for a given set of assets, intangible assets, laborers,
#' consumables, and fixed costs for a set of signatures.
#'
#' Computes the Annualized Development and Deployment Cost per Unit (DDCU),
#' Intangible Costs per Unit (ICU), Labor Cost per Unit (LCU), Variable
#' Costs per Unit (VCU), and Fixed Costs per Unit (FCU) for each signature. Also,
#' calculates operational costs and the total costs for each signature.
#'
#' Each data.frame argument should have a signatureID column, which specifies the
#' costs for each signature. For the appropriate signature data.frame setup, see
#' \code{\link{developmentDeploymentCostPerUnit}},
#' \code{\link{intangibleCostsPerUnit}}, \code{\link{laborCostPerUnit}},
#' \code{\link{variableCostsPerUnit}}, and \code{\link{fixedCostsPerUnit}}.
#'
#' A data.frame is returned containing the operational costs and total costs per
#' signature. The operational costs are defined as LCU + VCU + FCU. The total
#' costs are defined as DDCU + ICU + LCU + VCU.
#'
#' TODO: Add more description.
#'
#' @export
#' @param signatureAssets data.frame containing the signaturedID, capital cost,
#' discount rate, years of life, and process rate (in years) for each signature.
#' @param signatureIntangibleAssets data.frame containing the signaturedID, cost,
#' discount rate, years of life, and process rate (in years) for each signature.
#' @param signatureLaborers data.frame containing the signatureID, hours
#' recorded, burdened labor rate, and quantity process rate for each signature.
#' @param signatureConsumables data.frame containing the signatureID, price, and
#' quantity for each signature.
#' @param signatureFixedCosts data.frame containing the signatureID, fixedCosts,
#' and quantity process rate for each signature.
#' @return data.frame with the DDCCU, LCU, VCU, FCU, operational costs, and total
#' costs for each signature.
calcCosts <- function(signatureAssets, signatureIntangibleAssets,
                      signatureLaborers, signatureConsumables,
                      signatureFixedCosts) {
  sigClasses <- factor(signaturesAssets$signatureID)

  DDCU <- tapply(seq_along(sigClasses), sigClasses, function(sig) {
    whichSig <- which(sig == signatureAssets$signatureID)
    developmentDeploymentCostPerUnit(signatureAssets = signatureAssets[whichSig, ])
  })
  ICU <- tapply(seq_along(sigClasses), sigClasses, function(sig) {
    whichSig <- which(sig == signatureIntangibleAssets$signatureID)
    intangibleCostsPerUnit(signatureIntangibleAssets =
                           signatureIntangibleAssets[whichSig, ])
  })
  LCU <- tapply(seq_along(sigClasses), sigClasses, function(sig) {
    whichSig <- which(sig == signatureAssets$signatureID)
    laborCostPerUnit(signatureAssets = signatureAssets[whichSig, ])
  })
  VCU <- tapply(seq_along(sigClasses), sigClasses, function(sig) {
    whichSig <- which(sig == signatureAssets$signatureID)
    variableCostsPerUnit(signatureAssets = signatureAssets[whichSig, ])
  })
  FCU <- tapply(seq_along(sigClasses), sigClasses, function(sig) {
    whichSig <- which(sig == signatureFixedCosts$signatureID)
    fixedCostsPerUnit(signatureFixedCosts = signatureFixedCosts[whichSig, ])
  })  

  operationalCosts <- LCU + VCU + FCU
  totalCosts <- DDCU + ICU + LCU + VCU
  
  cbind.data.frame(signatureID = sigClasses, DDCU, ICU, LCU, VCU, FCU,
                   operationalCosts = operationalCosts, totalCosts = totalCosts)
}
