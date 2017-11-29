#' Calculates the risk for a set of signatures.
#'
#' Computes the risk of each signature and its variables.
#' TODO: Add more description.
#'
#' The \code{signatureEvents} data.frame should contain the values for each
#' variable of a signature. The signature is designated by a \code{signatureID}
#' column.
#'
#' A signature's risk is calculated with the following events:
#'
#' \itemize{
#'   \item \eqn{X_i}: Context Scenario
#'   \item \eqn{S_j}: True phenomenon of interest (i.e. known label)
#'   \item \eqn{A_k}: Signature Outcome/Prediction
#'   \item \eqn{T_l}: Terminating/Resultant Scenario
#' }
#'
#' For signature \eqn{m}, the calculated risk is given by
#' \deqn{R_m = \sum_{i,j,k,l} P(T_l | A_k, S_j, X_i) P(A_k | S_j, X_i) P(S_j | X_i) P(X_i) C(T_l)},
#' where \eqn{P} denotes the probability of an event and \eqn{C(T_l)} denotes the
#' consequence of the terminating scenario, \eqn{T_l}. Notice that there is a
#' many-to-many relationship between each type of event listed above.
#'
#' @export
#' @param signatureEvents data.frame with each row corresponding to a signature,
#' a set of events, and the corresponding probabilities as given in the above
#' risk definition. The signature is determined by a column denoted by
#' \code{signaturedID}.
#' @return a data.frame with the calculated risk for each signature.
calcRisk <- function(signatureEvents) {
  signatures <- factor(signatureEvents$signatureID)

  # For each signature, we compute the risk as defined above:
  # 1. Subset the probabilities and the consequence for each row correponding to
  #    the current signature
  # 2. For each row, we compute the product of the remaining columns.
  # 3. We return the sum of the products.
  sigRisks <- tapply(seq_along(signatures), signatures, function(m) {
    sigProbs <- subset(signatureEvents[m, ],
                       select = c(probContextScenario, probTruth, probPrediction,
                         probTerminatingScenario, consequence))
    sum(apply(sigProbs, 1, prod))
  })

  # sigRisks is a list. First, we transform the list into a vector of
  # risks. Then we 'cbind' this vector of risks along with the corresponding
  # 'signatureID' to form a data.frame.
  sigRisks <- do.call(c, sigRisks)
  sigRisks <- cbind.data.frame(signatureID = levels(signatures), risk = sigRisks)

  sigRisks
}

