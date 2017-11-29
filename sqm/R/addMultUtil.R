
##' An additive or multiplicative utility function
##'
##' An additive or multiplicative utility function
##'
##' The additive function is used if \code{sum(alpha) == 1} and the
##' multiplicative function is used otherwise.
##'
##' Regarding \code{saUtilFun}:  The names of \code{saUtilFun} must be a subset of the names
##' in \code{Z}, so that each single attribute utility function is matched to its corresponding
##' attribute in \code{Z}. Each element of \code{saUtilFun} must be of class \code{saUtilFun}.
##'
##' Functions of this \code{saUtilFun} class take a vector of attribute values as their principal argument and
##' return a corresponding vector of utility values. They may have additional named agruments that control the
##' parameters of the single attribute utility function.  They also have the following attributes:
##' \itemize{
##' \item{saUtilMethod}{Character string indicating the name of the single attribute utility method}
##' \item{parms}{A named list of all the agruments and their values that were provided to the call of the
##' to the single attribute utility function.}
##' }
##' See \code{\link{logUtil}} for an example.
##'
##' @export
##'
##' @param alpha A named list or named vector containing the weights for the multiattribute
##' utility function. The names should be the same as \code{saUtilFun}
##' 
##' @param saUtilFun A named list where each element of which contains its corresponding
##' single attribute utility function. See Details.
##' 
##' @param Z A dataframe or matrix containing the values of the attributes, where the attributes
##' are on the columns and the observations are on the rows.
##' 
##' @param beta A numeric value for the normalizing constant. If not provided, a solution for
##' \code{beta} is calculated from the \code{alpha} vector.
##'
##' @param utilLabel Character string indicating the column name that will be used for the utility
##' 
##' @return If \code{Z = NULL}, the normalizing constant, beta, is returned.  Otherwise, a data frame
##' of \code{Z} is returned with an additional column called \code{utilLabel} which contains
##' the additive or multiplicative multiattribute utility, is calculated for each row of
##' \code{Z}.
##'
##' @author Landon Sego
##' 
##' @references
##' TODO:  finish these references
##' Keeney & Raifa. Decision... 1976.  Holmes A., Sego L.H., etc. PNNL Technical report 2013
##'
##' @examples
##' # Generate single attribute utility functions
##' a1Fun <- saUtil(theta = 2, zrange = c(-2, 4), urange = c(1, 0))
##' plot(a1Fun)
##' 
##' a2Fun <- saUtil(urange = c(1,0), theta = -1.7)
##' plot(a2Fun)
##' 
##' # Put them in a single list
##' sa.funs <- list(attribute1 = a1Fun, attribute2 = a2Fun)
##' 
##' # The weights
##' alphas <- c(attribute2 = 0.2, attribute1 = 0.8)
##' 
##' # Cook up some data
##' someData <- data.frame(attribute1 = runif(10, -2, 4),
##'                        attribute2 = rpois(10, 3), row.names = letters[1:10])
##' 
##' # Calculate the utility
##' u <- addMultUtil(alphas, saUtilFun = sa.funs, Z = someData)
##' print(u)
##' 
##' # Manually calculate to check ...
##' u1 <- saUtil(z = someData$attribute1, theta = 2, zrange = c(-2, 4), urange = c(1, 0))
##' u2 <- saUtil(z = someData$attribute2, urange = c(1, 0), theta = -1.7)
##' 
##' u.check <- alphas["attribute1"] * u1 + alphas["attribute2"] * u2
##' 
##' # Should be 0
##' max(abs(u$utility - u.check))
##' 
##' # Now try a check of the multiplicative version
##' alphas <- c(attribute1 = 0.2, attribute2 = 0.9)
##' u <- addMultUtil(alphas, saUtilFun = sa.funs, Z = someData)
##' u.check <- alphas[1] * u1 + alphas[2] * u2 +
##'            attributes(u)$parms$beta * alphas[1] * alphas[2] * u1 * u2
##' 
##' # Should be 0 (or very close)
##' max(abs(u$utility - u.check))


## TODO:  This function needs to be tested, validated

addMultUtil <- function(alpha, saUtilFun = NULL, Z = NULL,
                        beta = NULL, utilLabel = "utility") {

  # Check alphas
  # Convert alpha to a named vector if it's a list
  if (is.list(alpha))
    alpha <- unlist(alpha)

  # Check arguments
  stopifnot(is.vector(alpha),
            is.numeric(alpha),
            all(alpha >= 0),
            all(alpha <= 1),
            is.vector(utilLabel),
            is.character(utilLabel))  
  
  # Check other variables
  if (!is.null(saUtilFun)) {

    names.saUtilFun <- names(saUtilFun)

    # More checks
    stopifnot(length(alpha) == length(saUtilFun),
              is.list(saUtilFun),
              setequal(names(alpha), names(saUtilFun)))
    
    if (!all(unlist(lapply(saUtilFun, function(x) {"saUtilFun" %in% class(x)}))))
      stop("Not all the elements of 'saUtilFun' are of class 'saUtilFun'")
    
    # Make sure ordering of alpha matches ordering of saUtilFun    
    alpha <- alpha[names.saUtilFun]
    
  }

  # Solve for beta if needed
  solve.beta <- TRUE

  # Function to solve for beta
  fbeta <- function(beta) {prod(1 + beta * alpha) - 1 - beta}

  # Validate beta  
  if (!is.null(beta)) {

    stopifnot(is.numeric(beta),
              length(beta) == 1)
    
    # Beta must be >= -1
    if (beta < -1) 
      warning("'beta' must be greater than -1. Solving for a viable value of beta.")
    
    # Beta must also match the alphas that were provided
    else if (abs(fbeta(beta)) > 1e-10) 
      warning("Value of 'beta' provided does not match the 'alpha' values.",
              "\nSolving for a viable value of beta.")
    
    # Conditions are satisfied, use the beta provided by the user
    else {
      betaToUse <- beta
      solve.beta <- FALSE
    }

  }
  
  # Solve for beta if it wasn't provided (or provided correctly)
  if (solve.beta) {

    s.alpha <- sum(alpha)
  
    # If the weights sum to 1  
    if (abs(s.alpha - 1) < 1e-10) 
      betaToUse <- 0

    else {
      
      if (s.alpha > 1)
        uint <- c(-1, -1e-10)
      else
        uint <- c(1e-10, 500)

      # Solve for beta
      betaToUse <- uniroot(fbeta, uint, tol = 1e-12)$root
      
    }
  }

  # If data was not provided, return the beta value only
  if (is.null(Z))
    return(betaToUse)

  # Create the utility function
  if (abs(betaToUse) < 1e-10) {

    # Additive
    aggregationFun <- function(aRow) {
      return(t(alpha) %*% aRow)
    } 
    
  }
  else {

    # Multiplicative
    aggregationFun <- function(aRow) {
      return((prod(1 + betaToUse * alpha * aRow) - 1) / betaToUse)
    }
    
  }

  ################################################################################
  # Now calculate the multiattribute utility
  ################################################################################
  
  # Checks on Z
  stopifnot(is.matrix(Z) | is.data.frame(Z),
            all(names.saUtilFun %in% colnames(Z)),
            !(utilLabel %in% colnames(Z)))

  # Create subset of Z that matches the elements in saUtilFun
  origRowNamesZ <- rownames(Z)

  # Assign working rownames to Z
  rownames(Z) <- rnZ <- 1:NROW(Z)

  # Grab only the columns we need for the utility
  Zsub <- Z[,names.saUtilFun]

  # If it's no longer a matrix, (because there is only one attribute)
  # then make it a column matrix
  if (is.vector(Zsub) & (length(names.saUtilFun) == 1))
    Zsub <- matrix(Zsub, ncol = 1, dimnames = list(rnZ, names.saUtilFun))
  
  # Convert data frame to a matrix and verify it's numeric    
  if (is.data.frame(Zsub))
    Zsub <- as.matrix(Zsub)

  if (!is.numeric(Zsub))
    stop("All columns of 'Z' that will be used for utility must be numeric")

  # Calculated single attribute utility functions for each attribute
  Z.saUtil <- lapply(as.list(names.saUtilFun),
                     function(attribName) saUtilFun[[attribName]](Zsub[,attribName]))
  
  # Turn results into a matrix
  Z.saUtil <- matrix(unlist(Z.saUtil), ncol = length(Z.saUtil),
                     dimnames = list(rnZ, names.saUtilFun))
  
  # Calculate the utility for each row
  util <- data.frame(util = apply(Z.saUtil, 1, aggregationFun))
  colnames(util) <- utilLabel

  # Add it into Z
  u <- merge(as.data.frame(Z), util, by = "row.names", sort = FALSE)

  # Sanity check--make sure Row.names matches rnZ
  if (!all(u$Row.names == rnZ))
    stop("Unexpected results with merge in addMultUtil(). Rownames seem mismatched")

  # Drop the rownames column and assign back the original rownames
  u <- u[,-which(colnames(u) == "Row.names")]

  # Restore original rownames
  rownames(u) <- origRowNamesZ

  # The attributes keep a record of all the parameters and function forms
  # in the utility function
  attributes(u) <- c(attributes(u),
                     list(maUtilMethod = "addMultUtil",
                          parms = list(alpha = alpha,
                                       saUtilFun = saUtilFun,
                                       beta = betaToUse,
                                       aggregationFun = aggregationFun)))
  
  class(u) <- c("maUtilCall", class(u))
  
  # Return the multiattribute multiplicative/additive utility function
  return(u)

} # addMultUtil

