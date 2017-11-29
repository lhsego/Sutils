#' Calculates the utility for a set of signatures.
#'
#' Computes the utility of each signature and its variables.
#' TODO: Add more description.
#'
#' The \code{signatures} data.frame should contain the values for each variable
#' of a signature. The signature is designated by a \code{signatureID} column.
#'
#' For each attribute, a univariate utility function must be determined. To
#' determine the fit, a variable's (practical) minimum and maximum values must
#' be specified as well as values that must be fit. Only one row should
#' correspond to a given variable. To fit these functions, the data.frame
#' \code{varUtility} must supply the following columns:
#'
#' \itemize{
#'   \item varName - Name of the variable
#'   \item xmin - Variable's minimum value.
#'   \item xmax - Variable's maximum value.
#'   \item x0 - Value of the variable with specified utility value in c0.
#'   \item y0 - Specified utility value for the variable value, x0.
#'   \item weights - Coefficient that will be used to aggregate the utility
#'   scores
#'   \item increasing - TRUE/FALSE. Should the utility function be increasing?
#' }
#'
#' The \code{weights} are coefficients used to calculate a linear combination of
#' the utility functions for a set of variables. By default, the weights are
#' standardized so that they sum to one. This can be disabled by setting
#' \code{standardizeWeights} to FALSE.
#'
#' A list is returned containing three elements:
#'
#' \enumerate{
#'   \item obsUtility - list
#'   \item aggregate - data.frame
#'   \item varUtilityFit - data.frame
#' }
#'
#' The list in \code{obsUtility} stores in \code{byVar} the utility values for
#' each observation's variables. The linear combination of these utility values
#' for each variable is given in \code{utility}. In other words, the fitted
#' utility functions are applied to the data given in \code{signatures} and
#' returned in a matrix, \code{byVar}. The linear combination of each row of
#' \code{byVar} is stored in \code{utility} with weights given in \code{weights}.
#'
#' The returned data.frame \code{aggregate} provides the aggregation of the
#' utility scores for each signature. More specifically, we calculate the
#' arithmetic mean of the utility values for the observations within each signature.
#'
#' The returned data.frame \code{varUtilityFit} contains the fitted arguments for
#' the utility function for each variable.
#'
#' @export
#' @param signatures data.frame with each row corresponding to a signature and a
#' vector of variable values. The signature is determined by a column denoted by
#' \code{signaturedID}.
#' @param varUtility data.frame with each row corresponding to a variable and
#' the required arguments to determine the utility function that will be used.
#' The columns should be 'varName', 'x0', 'y0', 'xmin', and 'xmax'. See Details.
#' @return list containing a data.frame with the aggregate utility values for
#' each signature and a list with the utility values for each observation.
#' See Details.
#' aggregate utility value for each signature.
#' @examples
#' # For this example, we construct a set of signatures with arbitrary names
#' # and values. Also, we provide the varUtility data.frame for these values.
#' # This example include 3 observations from each of 5 signatures. Each
#' # observation has 4 measured variables.
#' set.seed(42)
#' varNames <- c("var1", "var2", "var3", "var4")
#' x0 <- c(2, 20,  50,  100)
#' y0 <- c(0.25, 0.4, 0.6, 0.8)
#' xmin <- c(1, 10, 30, 80)
#' xmax <- c(5, 25, 60,  105)
#' weights <- seq_along(varNames)
#' varUtility <- cbind.data.frame(varNames,  x0,  y0, xmin, xmax, weights)
#' signatures <- t(replicate(15, runif(4, varUtility$xmin,  varUtility$xmax)))
#' signatures <- data.frame(signatures)
#' colnames(signatures) <- varNames
#' signatures <- cbind(signatureID = gl(5, 3),  signatures)
#'
#' calcUtility(signatures, varUtility)
#' calcUtility(signatures, varUtility, standardizeWeights = FALSE)
calcUtility <- function(signatures, varUtility,
                        standardizeWeights = TRUE) {
  weights <- varUtility$weights
  varNames <- varUtility$varName
  
  if (standardizeWeights) {
    weights <- weights / sum(weights)
  }
  sigClasses <- factor(signatures$signatureID)


  # For each variable, we fit the utility function using the arguments given in
  # the data.frame varUtility. The call to 'utility' for each variable yields a
  # value of 'k', 'xmin', and 'xmax'.
  varUtilityFit <- tapply(seq_along(varNames), varNames, function(j) {
    with(varUtility,
         utility(x0 = x0[j], y0 = y0[j], xmin = xmin[j], xmax = xmax[j], increasing = increasing[j])
    )
  })

  # varUtilityFit is a list of lists. We transform it to a data.frame.
  varUtilityFit <- do.call(rbind, lapply(varUtilityFit,  data.frame))

  # Notice that the row names for varUtilityFit are sorted. So, we obtain the
  # varNames once again.
  varNames <- rownames(varUtilityFit)

  # For each variable in 'signatures', we compute the utility values
  # corresponding to the selected k, xmin, and xmax.
  obsUtility <- sapply(seq_along(varNames), function(i) {
    with(varUtilityFit,
         exponential_utility(x = signatures[[varNames[i]]], k = k[i],
                             xmin = xmin[i], xmax = xmax[i], increasing = increasing[i])
    )
  })

  obsUtility <- list(utility = as.vector(obsUtility %*% weights),
                     byVar = obsUtility)

  utilAggregate <- aggregate(obsUtility$utility,
                            by = list(signatureID = signatures$signatureID),
                            FUN = mean)
  colnames(utilAggregate) <- c("signatureID", "utility")
  
  list(obsUtility = obsUtility, aggregate = utilAggregate,
       varUtilityFit = varUtilityFit)
}

