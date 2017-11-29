# A wrapper for calcConf.raw, checks inputs

# This function gives P(Z <= (1-pct.clean)*N | X=0 & Y=0).
# It's the predictive posterior
# distribution of the number of contaminated cells that remain.

# Uses new model, where:
#  prob of judgmental   sample contaminated is distributed Beta(1, beta)
#  prob of random       sample contaminated is distributed Beta(1, r*(beta + 1) - 1)
#  posterior for random sample contaminated is             Beta(1, n2 +  r*n1 + r*(beta + 1) - 1)


##'Calculates the Bayesian posterior probability of the number of contaminated
##'cells being less than a threshold
##'
##'Using the posterior predictive distribution, calculates the probability that
##'the number of contaminated cells is less than a determined threshold, given
##'that none of the judgmental nor random samples detected the presence of
##'contamination.
##'
##'This serves as a wrapper for \code{\link{calcConf.raw}}.
##'
##' @export
##' 
##'@param n2 The number of random samples
##'@param maxN A large integer indicating the transition from finite population
##'to infinite population formulas.
##'@param \dots Arguments to \code{\link{cjrParms}}
##'@param arg.check \code{=TRUE} checks the arguments supplied in \code{...}
##'using \code{\link{cjrParms}}.  I would not recommend setting this to
##'\code{FALSE}.
##'@param discrete \code{=TRUE} uses \code{\link{calcConf.discrete}} to
##'calculate the confidence using the Binomial cdf (rather than the continuous
##'approximation of the Bionomial cdf)
##'@return The value of the Bayesian confidence
##'@author Landon Sego
##'@seealso \code{\link{calcConf.raw}}, \code{\link{calcConf.discrete}}
##'@keywords misc
##'@examples
##'
##'calcConf(239, 10^6, n1=10, N=25000, pct.clean=0.99, r=2, prior.prob=0.05)
##'calcConf(239, 10^6, n1=10, N=25000, pct.clean=0.99, r=2, beta=19)
##'
calcConf <- function(n2, maxN, ..., arg.check = TRUE, discrete=FALSE) {

  #  ... :  these are the required arguments to cjrParms:  'n1', 'N', 'pct.clean', 'r', and either 'prior.prob' or 'beta'
  
  # Check that n2 is present
  if (missing(n2))
    stop("'n2', the first (and unnamed) argument, must be specified\n")

  # n2 must be >= 0
  if (n2 < 0) {
    warning("'n2 = ", n2, "' was set to 'n2 = 0'\n")
    n2 <- 0
  }

  # Place argument objects into the scope of the function (and check them, if requested) 
  cjrParms(..., arg.check = arg.check)
  
  # The following objects are now available for use within the 'calcConf' function:
  # n1
  # N
  # pct.clean
  # prior.prob
  # beta
  # r
  # thresh

  # n1 and n2 must not exceed N
  if (n1 + n2 > N)
    stop("n1 + n2 = ", n1 + n2, " > N = ", N, "\n")
  
  # Calculate the confidence
  if (!discrete)
    return(calcConf.raw(N, n1, n2, r, beta, pct.clean, maxN, output="confidence")$confidence)
  else
    return(calcConf.discrete(N, n1, n2, r, beta, pct.clean, maxN))
  
} # calcConf
  


