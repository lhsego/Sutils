#' Plots the specified utility function.
#'
#' TODO: Description
#'
#' The plot's filename must have one of the following extensions:
#' \itemize{
#'   \item ps
#'   \item pdf
#'   \item jpg
#'   \item tif
#'   \item png
#'   \item bmp
#' }
#'
#' @export
#' @param utilityResults list of utility results for the considered attributes.
#' This list is returned from both the 'calcUtility' and 'afUtility' functions.
#' @param length TODO
#' @param savePlot If TRUE, the plot will be saved to the file given in 'file'.
#' @param file The name of the saved plot. Ignored if savePlot is FALSE. See
#' details for valid file extensions.
#' @param numColsPlot The number of columns that will be used in the generated
#' plot. By default, there will be 2 columns per plot. For example, if there
#' are 6 considered attributes, a plot with 3 x 2 frames will be generated. Note
#' the same number of frames would be generated for 5 considered attributes.
#' @return Invisibly returns the filename of the plot. If the plot is not saved
#' to a file, then NULL is returned invisibly.
#' @examples
#' plotUtility(k = -2, xmin = 10, xmax = 20)
#' plotUtility(k = -4, xmin = 10, xmax = 20, xlim = "Variable Name", ylim = "Utility")
#' plotUtility(k = 2, xmin = 10, xmax = 30, savePlot = TRUE, file = "plot.png")
plotUtility <- function(utilityResults, length = 1000, savePlot = FALSE,
                        file = NULL, numColsPlot = 2) {

  utilityFit <- utilityResults$varUtilityFit
  numAttributes <- nrow(utilityFit)

  # If the user incorrectly passes a list or data.frame that was not returned
  # from either the 'afUtility' or 'calcUtility' functions, we throw an error.
  # We determine this based on the presence/absence of the 'varUtilityFit'
  # element in the returned list.
  # A more robust approach is to return an object from the utility functions as
  # an object from a specified class, say 'utilityResults'. Then, we would check
  # that the object passed in to the 'plotUtility' function is an object of the
  # 'utilityResults' class. This is an ideal approach and should be considered
  # later as we tighten up error handling.
  if (is.null(utilityFit)) {
    stop("The 'utilityResults' does not contain the fitted utility functions.")
  }

  fileReturned <- NULL
  if (savePlot) {

    if (is.null(file)) {
      stop("To save a plot, a filename must be provided.")
    } else {
      require('pnlStat')
      fileReturned <- pnlStat:::openDevice(fileName = file)
    }
  }

  # This function sets up the plot with the proper number of frames on the plot.
  # We use the 'ceiling' function to round up the number of rows. So, as we state
  # in our example in the @param statement, we will create a 3 x 2 plot if
  # we have either 5 or 6 attributes (assuming the default number of plot
  # columns).
  par(mfrow = c(ceiling(numAttributes / numColsPlot), numColsPlot))

  for (i in seq_len(numAttributes)) {
    k <- utilityFit$k[i]
    xmin <- utilityFit$xmin[i]
    xmax <- utilityFit$xmax[i]
    increasing <- utilityFit$increasing[i]
    varName <- rownames(utilityFit)[i]
  
    # Although the given value of k can be positive or negative, we set it to its
    # absolute value, so that the given value corresponds to "risk averse".
    k <- abs(k)

    x <- seq(from = xmin, to = xmax, length = length)
    riskAverse <- exponential_utility(x = x, k = k, xmin = xmin, xmax = xmax,
                                      increasing = increasing)
    riskProne <- exponential_utility(x = x, k = -k, xmin = xmin, xmax = xmax,
                                     increasing = increasing)
    if (increasing) {
      riskNeutral <- (x - xmin) / (xmax - xmin)
    } else {
      riskNeutral <- 1 - (x - xmin) / (xmax - xmin)
    }

    plot(x, riskAverse, type = "l", xlab = varName, ylab = "utils")
    lines(x, riskProne, type = "l")
    lines(x, riskNeutral, type = "l")
  }

  if (savePlot) {
    dev.off()
  }
  invisible(fileReturned)
}
