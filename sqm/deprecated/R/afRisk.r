#' Signature Risk Calculation for the Analytic Framework
#'
#' A key goal of the Analytic Framework is to compare the efficacy of a variety
#' of signatures. One important comparison metric is risk based on each
#' signature. This function computes the risk for each signature from a set of
#' input files.
#'
#' The first column of each file should contain an identifier to distinguish each
#' signature. The signature identifier should be unique. We refer to this
#' signature identifies as \code{signatureID}.
#'
#' TODO: Provide the details of the other columns for each input file.
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
#' The object returned from \code{afRisk} will be the same whether or not the
#' results are exported to files. However, if an output file is specified,
#' then the results will be \code{invisible}. See \code{\link{invisible}} for
#' details.
#'
#' The input file should be in the CSV format. Likewise, the output files will be
#' in the CSV format.
#'
#' @export
#' @param contextsFile string with the file name corresponding to the contexts
#' and their probabilties for each signature.
#' @param truthsFile string with the file name corresponding to the true
#' phenomenon of interest (i.e. known label). For each true phenomenon of
#' interest, the probability of the true phenomenon of interest should be
#' provided conditional on each context scenario given in \code{contextsFile}.
#' @param predictionsFile string with the file name corresponding to the
#' signature outcome/prediction. For each prediction, the probability of the
#' prediction should be provided conditional on each true phenomenon of interest
#' given in \code{truthsFile} and on each context scenario given in
#' \code{contextsFile}.
#' @param terminantingScenarioFile string with the file name corresponding to the
#' terminating scenario for each signature. For each terminating scenario, the
#' probability of the terminating scenario should be provided conditional on each
#' signature outcome/prediciton given in \code{predictionsFile}, on each true
#' phenomenon of interest given in \code{truthsFile} and on each context scenario
#' given in \code{contextsFile}.
#' @param consequencesFile string with the file name corresponding to the
#' consequences of each terminating scenario for each signature.
#' @param outputFile string with the file name for the output file where the risk
#' results will be stored. If NULL (default), then the results are not exported
#' to a file. See details.
#' @param digits the number of digits after the decimal to use in the results
#' that are outputted to files. If NULL, no rounding is performed.
#' @return a data.frame that contains the risk results for each each signature.
afRisk <- function(contextsFile, truthsFile, predictionsFile,
                     terminatingScenarioFile, consequencesFile,
                     outputFile = NULL, digits = NULL) {
  # Reads the signature events from the specified input files that will be used
  # to calculate the risk for each signature.
  riskEvents <- readRisk(contextsFile = contextsFile, truthsFile = truthsFile,
                         predictionsFile = predictionsFile,
                         terminatingScenarioFile = terminatingScenarioFile,
                         consequencesFile = consequencesFile)

  # We formulate the 'riskEvents' list into a single data.frame that will be
  # passed to the 'calcRisk' function to perform the actual calculation of risks.
  signatureEvents <- merge(riskEvents$contexts, riskEvents$truths,
                           by = c("signatureID", "contextScenario"))
  signatureEvents <- merge(signatureEvents, riskEvents$predictions,
                           by = c("signatureID", "contextScenario", "truth"))
  signatureEvents <- merge(signatureEvents, riskEvents$terminatingScenarios,
                           by = c("signatureID", "contextScenario", "truth",
                             "prediction"))
  signatureEvents <- merge(signatureEvents, riskEvents$consequences,
                           by = c("signatureID", "terminatingScenario"))  
    
  # For each signature, we calculate the risk. The results are stored in a
  # the data.frame, 'accuracyResults'.
  riskResults <- calcRisk(signatureEvents = signatureEvents)

  # If a file name is specified for output, we write 
  if (!is.null(outputFilePrefix)) {
      writeRisk(riskResults = riskResults, outputFile = outputFile,
                    digits = digits)
      invisible(riskResults)
  } else {
    riskResults
  }

}
