#' Reads the signature events to compute signature risks from given files.
#'
#' For a given file, this function reads in the signature events along with
#' their respective probabilties to  calculate the corresponding risks for each
#' signature.
#'
#' The first column should contain the signature ID.
#'
#' We require that the input file is in the CSV file format (i.e. the file
#' delimiter is a comma).
#'
#' TODO: Discuss the conditionality of the probabilities.
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
#' @return a list with six data.frame corresponding to the inputted events and
#' probabilities for each signature.
readRisk <- function(contextsFile, truthsFile, predictionsFile,
                     terminatingScenarioFile, consequencesFile) {
  contextsColNames <- c("signatureID", "contextScenario", "probContextScenario")
  contexts <- read.csv(contextsFile, header = FALSE,
                              col.names = contextsColNames,
                              stringsAsFactors = FALSE)
  truthsColNames <- c("signatureID", "truth", "contextScenario", "probTruth")
  truths <- read.csv(truthsFile, header = FALSE,
                              col.names = truthsColNames,
                              stringsAsFactors = FALSE)

  predictionsColNames <- c("signatureID", "prediction", "truth",
                           "contextScenario", "probPrediction")
  predictions <- read.csv(predictionsFile, header = FALSE,
                              col.names = predictionsColNames,
                              stringsAsFactors = FALSE)

  terminatingScenariosColNames <- c("signatureID", "terminatingScenario",
                                    "prediction", "truth", "contextScenario",
                                    "probTerminatingScenario")
  terminatingScenarios <- read.csv(predictionsFile, header = FALSE,
                              col.names = terminatingScenariosColNames,
                              stringsAsFactors = FALSE)
  
  consequencesColNames <- c("signatureID", "terminatingScenario", "consequence")
  consequences <- read.csv(consequencesFile, header = FALSE,
                              col.names = consequencesColNames,
                              stringsAsFactors = FALSE)
  
  list(contexts = contexts,
       truths = truths,
       predictions = predictions,
       terminatingScenarios = terminatingScenarios,
       consequences = consequences
      )
}
