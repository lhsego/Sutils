##' Identifies the pareto frontier 
##'
##' Identifies the pareto frontier for a set of summarized signatures
##'
##' Identify the pareto frontier by 
##' the attributes. A \code{signatureID} dominates another if it is better than the
##' other \code{signatureID} for at least one attribute,
##' and, at a minumum, equivalent for all the other attributes.
##' 
##' The new varible \code{frontier} is of type character, each element of which can
##' take one of four possible values:
## TODO fix this
## \itemize{
## \item{"yes" }{ indicates the \code{signatureID} is on the frontier}
## \item{"" }{ A blank value, indicating the \code{signatureID} is not on the frontier}
# Cannot achieve 'tie' response so removing line below from help file:
# \item{"tie" }{ indicates \code{signatureID} is tied with another \code{signatureID}}
# Cannot achieve 'unknown' in response so removing line below from help file:
# \item{"unknown" }{ indicates the algorithm could not ascertain whether the \code{signatureID}
## was on the frontier. This state shouldn't happen--and a warning is produced if it does.}
## }
##' 
##' @export
##' @param X A data frame containing the attributes on the columns and one row
##' for each value of \code{signatureID}, where the required column
##' \code{signatureID} indicates the diferent signature systems under
##' comparison.
##'
##' @param orientation A named vector of 1's and -1's, where the names identify the columns
##' in \code{X} that will be used in the pareto analysis. A value of \code{1} indicates the
##' utility of the variable is increasing, i.e. larger values of the variable are preferred.
##' A value of \code{-1} indicates the utility of the variable is decreasing, i.e., smaller values
##' of the variable are preferred to larger ones.
##'
##' @param tol the tolerance for determining if two instances are tied.
##' If the absolute difference of a particular attribute exceeds \code{tol},
##' they are considered to be different (not tied).
##'
##' @return A data frame identical to \code{X}, except with a new column labeled \code{frontier} 
##' indicating if the particular \code{signatureID} is on the frontier.
##'
##' @author Landon Sego
## TODO: @references insert method for selecting the frontier
## TODO: 2d, 3d? plot methods for object of class pareto
## TODO: validate the algorithm--I think it's good, but I haven't rigorously checked it
##'
##' @examples
##' data(bacillusAttributes)
##' p <- idParetoFrontier(bacillusAttributes, c(risk = -1, ss = -1, cost = -1))
##' print(p)
##' plot(p)

idParetoFrontier <- function(X, orientation, tol = 1e-10) {

  # Check arguments
  stopifnot(is.data.frame(X),
            "signatureID" %in% colnames(X),
            is.vector(orientation),
            all(names(orientation) %in% colnames(X)),
            !("signatureID" %in% names(orientation)),
            all(abs(as.integer(orientation)) == 1),
            !("frontier" %in% colnames(X)),
            !("frontier" %in% names(orientation)))

  # The trivial case
  if (NROW(X) == 1) {
    X$frontier <- "yes"
    return(X)
  }
            
  # Verify that each signatureID has only one row
  if (!all(table(X$signatureID) == 1))
    stop("Each 'signatureID' must have only 1 row")

  # Create the matrix on which we'll operate
  Y <- as.matrix(X[, names(orientation)])

  if (!is.numeric(Y))
    stop("All the columns in 'X' selected by the names of 'orientation' must be numeric")

  # Number of rows and columns
  nR <- NROW(Y)
  nC <- NCOL(Y)
  
  # A matrix to make all positive comparisons
  orien.mat <- matrix(orientation, nrow = nR, ncol = nC, byrow = TRUE)

  # Multiply Y by orien.vec to orient them all to make positive comparisons
  # i.e. the ordering of the attributes of negative comparisons is reversed using -1
  Yorien <- Y * orien.mat

  # Function to compare one instance to another
  idInferior <- function(inst1, inst2) {

    d <- inst1 - inst2

    # If they are the same (to within tol)
    if (all(abs(d) < tol))
      out <- "tie"
    # if inst1 dominates, and inst2 in inferior
    else if (all(d >= 0))
      out <- "inst2"
    # if inst2 dominates, and inst1 is inferior
    else if (all(d <= 0))
      out <- "inst1"
    # if none dominate: this shouldn't happen
    else
      out <- "unknown"

    return(out)
    
  } # idInferior
  
  # Intialize output vec, which will determine which instances are inferior
  infOut <- rep("yes", nR)

  # Loop over lower triangle
  for (i in 2:nR) {

    i1 <- Yorien[i,]

    for (j in 1:(nR - 1)) {

      i2 <- Yorien[j,]
      
      compare <- idInferior(i1, i2)

      if (compare == "inst1")
        infOut[i] <- ""
      else if (compare == "inst2")
        infOut[j] <- ""

    } # loop over columns

  } # loop over rows
  
  # Add the vector into X

  if (any(infOut == "unknown"))
    warning("Unexpected result: For one or or more of the signatureIDs, it is\n",
            "unknown whether they are on the frontier or not")
  
  X$frontier <- infOut
  X <- X[,c("signatureID", names(orientation), "frontier")]
  class(X) <- c("pareto", class(X))

  return(X)

} # idParetoFrontier

##' @S3method plot pareto
##'

plot.pareto <- function(X, ...) {

  # Identify number of attributes
  attribs <- setdiff(colnames(X), c("signatureID", "frontier"))
  n.attribs <- length(attribs)
  
  if (n.attribs > 3)
    stop("plot method for class 'pareto' can only plot 2 or 3 dimensions")

  frontierInd <- X$frontier == "yes"

  # Set graphing parameters  
  inputParms <- list(...)

  frontierColor <- ifelse(is.null(inputParms$col), "red", inputParms$col)
  
  # Default plotting parameters which can be overriden by arguments to ...
  defaultPlotParms <- list(main = paste("Pareto plot\n",
                                        paste(toupper(substr(frontierColor, 1, 1)),
                                              tolower(substr(frontierColor, 2, 100)), sep = ""),
                                        "points indicate the frontier"),
                           xlab = attribs[1],
                           ylab = attribs[2],
                           zlab = attribs[3],
                           pch = 19)
  
  plotParms <- defaultPlotParms[setdiff(names(defaultPlotParms), names(inputParms))]
  plotParms <- c(plotParms, inputParms)

  # Set color for frontier
  plotParms$col <- rep("black", NROW(X))
  plotParms$col[frontierInd] <- frontierColor

  # Now for two dimensional plot
  if (n.attribs == 2) {

    # Color and zlab will be removed from these parms
    if (length(ind <- which(names(plotParms) == "zlab")))
      plotParms <- plotParms[-ind]

    do.call("plot", c(list(x = X[,attribs[1]], y = X[,attribs[2]]), plotParms))

  }

  # And the 3d plot
  else {

    # I didn't put this in the 'depends' DESCRIPTION because I couldn't get rgl to install on martingale
    stopifnot(require(rgl))

    do.call("plot3d", c(list(x = X[,attribs[1]], y = X[,attribs[2]], z = X[,attribs[3]]), plotParms))

  }

  invisible(NULL)
     
} # plot.pareto
