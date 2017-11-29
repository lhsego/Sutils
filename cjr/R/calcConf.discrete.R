# This function gives P(Z <= (1-pct.clean)*N | X=0 & Y=0)
# for new CJR model, should be the same as calcConf, except it doesn't use the
# continuous binomial approximation for the distribution of Z.

# This function is not completely tested...

##'Calculate the Bayesian confidence using the discrete Binomial cdf
##'
##'Calculate the Bayesian confidence using the discrete Binomial cdf, i.e.
##'requiring the number of unacceptable items to be a whole number
##'
##'
##' @export
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
##'@return The Bayesian confidence
##'@author Landon Sego
##'@seealso \code{\link{calcConf.raw}}, \code{\link{calcConf}}
##'@keywords misc
##'@examples
##'
##'calcConf.discrete(1000, 10, 80, 2, 23, 0.99, 10^6)
##'calcConf.discrete(999, 10, 80, 2, 23, 0.99, 10^6)
##'
calcConf.discrete <- function(N, n1, n2, r, beta, lambda, maxN) {
  
  # N can be a vector, but the rest of the arguments need to have length 1
  if ((length(n1) > 1) |
      (length(n2) > 1) |
      (length(r) > 1) |
      (length(beta) > 1) |
      (length(lambda) > 1) |
      (length(maxN) > 1))
    stop("'n1', 'n2', 'r', 'beta', 'lambda', and 'maxN' must all be singletons\n")
     
  # Initialize outputs
  confidence <- rep(NA, length(N)) 

  # Boolean indicators for finite and infinite population calculations
  infSet <- N > maxN
  finiteSet <- !infSet

  # Calculate necessary quantities if there are finite N's
  if (any(finiteSet)) {
    
    k1 <- -1 * (n1 + n2)
    k2 <- n1 * (r - 1) + r * (beta + 1)
    A <- N + k1 + 1
    B <- ceiling(round(lambda * N, 14)) + k2 - 1
    C <- ceiling(round(lambda * N, 14)) + k1
    D <- N + k2
    
  }

  # Now calculate the confidence
  
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
      
 return(confidence)
  
} # calcConf.discrete

