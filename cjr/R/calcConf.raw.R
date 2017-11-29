# Raw function (doesn't check the inputs) to calculate the confidence,
# the sign of the first partial derivative of the confidence wrt N and

# the 'output' argument is there to avoid calculating uncessary quantities

# The mathematics in this function were derived by Landon Sego and then verified by Scott Morris on 5/28/2009.

# All mathematics re-verified by Landon Sego on 9/9/2009

##'Calculate the Bayesian confidence and the sign of the first derivative
##'
##'Calculate the Bayesian confidence and the sign of the first derivative of
##'the confidence function wrt N at the value N.
##'
##'
##'@param N Population size
##'@param n1 Number of judgmental samples
##'@param n2 Number of random samples
##'@param r A high risk judgment sample location is expected to be \code{r}
##'times more likely to be contaminated than a low risk random sample location
##'@param beta The second shape parameter of the prior distribution (which is a
##'Beta distribution).
##'@param lambda The fraction of percentage clean in (0,1].
##'@param maxN A large integer indicating the transition from finite population
##'to infinite population formulas.
##'@param output Character vector indicating which quanity to calculate: the
##'confidence, the first derivative, the sign of the first derivative, or any
##'combination thereof.
##'@return List with the following elements: \item{confidence}{The Bayesian
##'confidence} \item{first.sign}{The sign of the partial derivative of the
##'confidence function wrt N evaluated at \code{N}}
##'@author Landon Sego
##'@seealso \code{\link{calcConf}}, \code{\link{calcConf.discrete}}
##'@keywords misc
##'@examples
##'
##'calcConf.raw(10^4, 10, 80, 2, 23, 0.98, 10^6, output="confidence")
##'calcConf.raw(10^4, 10, 95, 2, 23, 0.98, 10^6)
##'
calcConf.raw <- function(N, n1, n2, r, beta, lambda, maxN, output=c("confidence", "first.sign")) {

  if (!all(output %in% c("confidence", "first.sign")))
    stop("'output' should be a subset of c('confidence','first.sign')\n")

  # N can be a vector, but the rest of the arguments need to have length 1
  if ((length(n1) > 1) |
      (length(n2) > 1) |
      (length(r) > 1) |
      (length(beta) > 1) |
      (length(lambda) > 1) |
      (length(maxN) > 1))
    stop("'n1', 'n2', 'r', 'beta', 'lambda', and 'maxN' must all be singletons\n")
     
  # Initialize outputs
  confidence <- first.sign <- rep(NA, length(N)) 

  # Boolean indicators for finite and infinite population calculations
  infSet <- N > maxN
  finiteSet <- !infSet

  # Calculate necessary quantities if there are finite N's
  if (any(finiteSet)) {
    
    k1 <- -1 * (n1 + n2)
    k2 <- n1 * (r - 1) + r * (beta + 1)
    A <- N + k1 + 1
    B <- lambda * N + k2 - 1
    C <- lambda * N + k1
    D <- N + k2
    
  }

  # Now calculate the confidence
  if ("confidence" %in% output) {

    # Always use the direct solution if lambda = 1, regardless of N (note that this matches Wright Grieve)
    if (lambda >= 1) 
      confidence <- 1 - (N - n1 - n2) / (N - n1 + r * (n1 + beta + 1) - 1)

    else {

      # Use formula for infinite N calcs
      if (any(infSet))
        confidence[infSet] <- 1 - lambda ^ (n2 + r * (n1 + beta + 1) - 1)

      # Finite N formula
      if (any(finiteSet)) {
        tmp1 <- 1 - exp(lgamma(A) + lgamma(B) - lgamma(C) - lgamma(D))
        confidence[finiteSet] <- tmp1[finiteSet]
      }
        
    }

    # When more than lambda% of the area has been sampled, the confidence is 1
    confidence[n1 + n2 >= lambda * N] <- 1

    # Make sure the confidence is not negative
    if (any(confidence < 0)) {
        
      cat("Confidence values:\n")
      print(confidence)
        
      stop("One or more confidence values < 0 for ",
        pvar(N, n1, n2, r, beta, lambda, maxN, verbose=FALSE), "\n")
    }
      
  }
      

  # Calculate the first derivative
  if ("first.sign" %in% output) {

    # For infinite N, the derivative is 0
    if (any(infSet))
      first.sign[infSet] <- 0

    if (any(finiteSet)) {

      # Derivative is always negative if lambda = 1
      if (lambda >= 1)
        first.sign[finiteSet] <- -1
      
      else {
        tmp2 <- -1 * sign(digamma(A) + lambda * digamma(B) - lambda * digamma(C) - digamma(D))
        first.sign[finiteSet] <- tmp2[finiteSet]
      }


    }

  }
      

  return(list(confidence=confidence, first.sign=as.integer(first.sign)))
  
} # calcConf.raw

