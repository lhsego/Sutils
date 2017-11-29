##' Generates or evaluates a multiattribute utility function 
##'
##' Generate or evaluates a multiattribute utility function
##'
##' Method functions must be constructed to operate on \code{Z}. Note that
##' 
##' @export
##' 
##' @rdname maUtil
##' 
##' @param maUtilMethod A single character string (or unique abbreviation) indicating the multiattribute
##' utility function method. Acceptable methods are given by \code{\link{maUtilMethods}}.
##' 
##' @param Z A dataframe or matrix containing the values of the attributes, where the attributes
##' are on the columns and the observations are on the rows.
##' 
##' @param dots Optional named arguments to \code{\link{maUtilMethods}} functions.
##'
##' @param x An object returned by \code{maUtilMethod}, which will be of class \code{maUtilCall} or \code{maUtilFun}
##' 
##' @param dots Additional arguments to \code{\link{print.default}} or \code{\link{plot.default}} 
##'
##' @author Landon Sego
##' 
##' @return
##' \item{1}{If \code{Z = NULL}, the multiattribute utility function with fixed parameters is returned
##' and can be called later. This function has a single argument, \code{Z}, that matrix or dataframe
##' containing the measured
##' values of the attributes of interest. The returned object is of class \code{maUtilFun}.}
##' \item{2}{If a matrix or dataframe is provided for the argument \code{Z},
##' the multiattribute utility method is evaluated using \code{Z} for the given parameters \code{...} and the
##' calculated utility values are returned. The returned object is of class \code{maUtilCall}.}
##' 
##' @examples
##' # Generate single attribute utility functions
##' sa.funs <- list(a1 = saUtil(theta = 2, zrange = c(-2, 4), urange = c(1, 0)),
##'                 a2 = saUtil(zrange = c(0.4, 0.9), urange = c(1,0), theta = -1.7))
##' 
##' # The weights
##' alphas <- c(a2 = 0.2, a1 = 0.8)
##' 
##' # Cook up some data
##' someData <- data.frame(a1 = runif(10, -2, 4),
##'                        a2 = rpois(10, 3), row.names = letters[1:10])
##' 
##' # Calculate the utility
##' u <- maUtil(alpha = alphas, saUtilFun = sa.funs, Z = someData)
##' print(u)
##' 
##' # Create a function for calculating the utility
##' uf <- maUtil(alpha = alphas, saUtilFun = sa.funs)
##' uf(someData)
##' 
##' # Plot the single attribute utility functions side by side:  plot the uf object
##' par(mfrow = c(1, 2), pty = "s")
##' plot(uf)
##' 
##' # Notice how we can input graphing parameters to control the look of the chart
##' # The length of each graphic parameter should either be 1 (which will be applied to
##' # each plot), or equal to the number of attributes.
##' plot(uf, ylab = c(expression(u[1]), expression(u[2])), main = c("Attribute 1", "Attribute 2"), lwd = 2,
##'      col = "Blue", las = 1)
##' 
##' # Two ways to calculte the special case with only a single attribute utility function
##' s1 <- maUtil(alpha = c(a1 = 1), saUtilFun = list(a1 = sa.funs[[1]]), Z = someData)
##' s2 <- maUtil(alpha = c(a1 = 1, a2 = 0), saUtilFun = sa.funs, Z = someData)
##' max(abs(s1 - s2))

maUtil <- function(maUtilMethod = "addMultUtil", Z = NULL, ...) {

  # Match the maUtilMethod argument
  maUtilMethod <- match.arg(maUtilMethod, maUtilMethods())

  # Grabbing the arguments to the maUtilMethod
  inputParms <- list(...)

  # Check that arguments provided in parms match the utility method
  validArgs <- formals(maUtilMethod)
  
  if (!all(names(inputParms) %in% names(validArgs)))
    stop("'", paste(bad <- setdiff(names(inputParms), names(validArgs)), collapse = "', '"), "' ",
         ifelse(length(bad) == 1, "is not a valid argument", "are not valid arguments"),
         " to '", maUtilMethod, "'")

  # Create the list of parms. 
  # Get setof parms that were not provided in the ...
  parms <- validArgs[setdiff(names(validArgs), names(inputParms))]
  
  # Add in the parms that were provided in the ...
  parms <- c(parms, inputParms)
  
  # Now remove the data object Z
  parms <- parms[-which(names(parms) == "Z")]

  # If Z is null, return the function
  if (is.null(Z)) {

    # Define the single argument utility function
    utilFun <- function(Z) {
  
      do.call(maUtilMethod, c(list(Z = Z), parms))
  
    } # utilFun

    class(utilFun) <- unique(c("maUtilFun", class(utilFun)))
    
  }
  # If Z is provided, call the function and get utilities
  else {
    
    utilFun <- do.call(maUtilMethod, c(list(Z = Z), parms))
    parms <- attributes(utilFun)$parms

    class(utilFun) <- unique(c("maUtilCall", class(utilFun)))    
      
  }
  
  # Add in attributes that make it clear what the parameters are set to
  attributes(utilFun) <- c(attributes(utilFun),
                           list(maUtilMethod = maUtilMethod,
                                parms = parms))

  return(utilFun)

} # maUtil


################################################################################
### Printing and plotting methods
################################################################################


##' @rdname maUtil
##' @method print maUtilCall
##' @S3method print maUtilCall

# A method for printing that doesn't show the attributes
print.maUtilCall <- function(x, ...) {

  class(x) <- "data.frame"
  print(x, ...)
  
} # print.maUtilCall


##' @rdname maUtil
##' @method print maUtilFun
##' @S3method print maUtilFun

# A method for printing that doesn't show the attributes
print.maUtilFun <- function(x, ...) {

  y <- x
  attributes(y) <- list(names = names(x))
  print(y, ...)
  
  maUtilMethod <- attributes(x)$maUtilMethod
  parms <- names(attributes(x)$parms)
  cat("\n")
  pvar(maUtilMethod, parms)

  obj <- deparse(substitute(x))
  cat("\nUse 'str(", ifelse(length(obj) > 1, "", obj), ")' for more details\n", sep = "")

} # print.maUtilFun

##' @rdname maUtil
##' @method plot maUtilFun
##' @S3method plot maUtilFun

# Plot methods
plot.maUtilFun <- function(x, ...) {

  # Extract the SA utility functions
  saFuns <- attributes(x)$parms$saUtilFun

  # Number of attributes
  numAttributes <- length(saFuns)

  # Get plotting parameters
  plotParms <- list(...)

  # If xlab not provided, then use the names of saFuns...
  if (!("xlab" %in% names(plotParms)))
    plotParms$xlab <- names(saFuns)

  # Check lengths of graphic arguments--they need to match the number of saUtil functions
  # or be of length 1
  lapply(plotParms, function(x) stopifnot(length(x) == numAttributes | length(x) == 1))

  # If they're length 1, repeat them
  plotParms <- lapply(plotParms, function(x) {if (length(x) == 1) x <- rep(x, numAttributes); return(x)})

  # Build the list for plotting
  pList <- vector(mode = "list", length = length(saFuns))

  for (i in 1:length(pList))
    pList[[i]] <- c(list(x = saFuns[[i]]), lapply(plotParms, function(x) x[i]))

  # Plot them one by one...
  out <- lapply(pList, function(x) do.call("plot", x))

  return(invisible(NULL))

} # plot.maUtilFun


