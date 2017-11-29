#' Fits a utility function to the user-specified utility values for a given
#' variable.
#'
#' TODO: Add description.
#'
#' @export
#' @param x0 numeric value of the considered variable for which a utility value
#' is given
#' @param y0 numeric utility value specified for x0
#' @param xmin The minimum value for the considered variable.
#' @param xmax The maximum value for the considered variable.
#' @param k_range The range of values for the exponential coefficient, 'k', to be
#' fitted via the R function \code{uniroot}.
#' @return list containing the necessary parameters to calculate the exponential
#' utility function for observations of this variable.
utility <- function(x0, y0, xmin, xmax, k_range = c(-100, 100), increasing = TRUE) {
  k <- try(uniroot(f = exponential_utility_fit, interval = k_range,
                   x0 = x0, y0 = y0, xmin = xmin, xmax = xmax, increasing = increasing)$root,
           silent = TRUE)
  if (inherits(k, "try-error")) {
    print(k)
    stop("There was an error in calculating the value of 'k' in 'utility.")
  }
  list(k = k, xmin = xmin, xmax = xmax, increasing = increasing)
}

#' The exponential utility function.
#'
#' TODO: Add description
#'
#' @export
#' @param x numeric value of the considered variable for a which a utility value
#' is requested.
#' @param k The fitted value for the coefficient in the exponential utility
#' function.
#' @param xmin The minimum value for the considered variable.
#' @param xmax The maximum value for the considered variable.
#' @return The variable's  utility value for the given value, 'x'
exponential_utility <- function(x, k, xmin, xmax, increasing = TRUE) {
  if (increasing) {
    (exp(-k * (x - xmin) / (xmax - xmin)) - 1) / (exp(-k) - 1)
  } else {
    1 - (exp(-k * (x - xmin) / (xmax - xmin)) - 1) / (exp(-k) - 1)
  }
} 

#' A helper function to fit the optimal value of 'k' for the exponential utility
#' function.
#'
#' TODO: Add description
#'
#' @param k The candidate value for the coefficient in the exponential utility
#' function.
#' @param x0 numeric value of the considered variable for which a utility value
#' is given
#' @param y0 numeric utility value specified for x0
#' @param xmin The minimum value for the considered variable.
#' @param xmax The maximum value for the considered variable.
#' @param k_range The range of values for the exponential coefficient, 'k', to be
#' fitted via the R function \code{uniroot}.
exponential_utility_fit <- function(k, x0, y0, xmin, xmax, increasing = TRUE) {
  if (increasing) {
    (exp(-k * (x0 - xmin) / (xmax - xmin)) - 1) / (exp(-k) - 1) - y0
  } else {
    1 - (exp(-k * (x0 - xmin) / (xmax - xmin)) - 1) / (exp(-k) - 1) - y0
  }
} 
