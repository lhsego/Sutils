# Identify whether the confidence function is a decreasing function of N using the derivative
# Warning:  Does not protect for incorrect values of N, n1, n2, lambda, r, or beta


##'Validates that the confidence function is deceasing for "all" N
##'
##'A helper function for \code{\link{is.viable.lambda}} which determines
##'whether the confidence function is deceasing for a sequence of \code{N}.
##'
##'@param lambda The percentage clean.
##'@param n1 The number of judgmental samples
##'@param n2 The number of random samples
##'@param r Ratio of prior expecation between judgmental and random samples
##'@param beta Second shape pararments of the beta prior of the judgmental
##'samples
##'@param maxN A large integer indicating the transition from finite population
##'to infinite population formulas.
##'@return A logical (boolean) indicating whether the confidence function is
##'decreasing for "all" \code{N} in \code{[Nmin, maxN]} where \code{Nmin} is
##'the smallest \code{N} can be given \code{n1}, \code{n2}, and \code{lambda}.
##'
##'This sequence of \code{N} is linear logarithmic: it is more dense for
##'smaller \code{N} and less dense for larger \code{N}, i.e., the natural
##'logarithm of the sequence has values that are evenly spaced.
##'@author Landon Sego
##'@seealso Called by \code{\link{is.viable.lambda}}.
##'@keywords misc
##'@examples
##'
##'decreasingConf(0.99, 10, 30, 2, 200, 10^6)
##'decreasingConf(0.995, 10, 30, 2, 200, 10^6)
##'
decreasingConf <- function(lambda, n1, n2, r, beta, maxN) {

  # lambda is the pct.clean
  # beta is the 2nd shape parameter of the Beta prior which describes the distribution of the
  #  probability of a judgmental sample being contaminated (before any samples are taken)

  # If lambda is 1, then we're guaranteed that the confidence function is a decreasing function of N
  if (lambda >= 1)
    decreasing <- TRUE

  else {
  
    # Check the sign at maxN first.  If negative, the check the sign across the sequence
    first.deriv.sign.maxN <- try(calcConf.raw(maxN, n1, n2, r, beta, lambda, maxN, output="first.sign")$first.sign, silent=TRUE)

    if ((class(first.deriv.sign.maxN) == "try-error") | is.na(first.deriv.sign.maxN)) {
      cat("Failure in decreasingConf(", pvar(lambda, n1, n2, r, beta, maxN, verbose=FALSE, digits=5), ") ",
          "when calling \n",
          "   calcConf.raw(", pvar(maxN, n1, n2, r, beta, lambda, maxN, verbose=FALSE, digits=5), ", output='first.sign')\n",
          sep="")
      stop(first.deriv.sign.maxN)
    }


    # If the deriv is neg at maxN, then check the sequence
    if (first.deriv.sign.maxN  < 0) {

      Nmin <- (n1 + n2) / lambda + 0.1
      Nseq <- exp(seq(log(Nmin), log(maxN - 1), length=10^4))

      first.deriv.sign.seq <- calcConf.raw(Nseq, n1, n2, r, beta, lambda, maxN, output="first.sign")$first.sign

      decreasing <- all(first.deriv.sign.seq < 0)


    }
    else
      decreasing <- FALSE
    
  } # else

  return(decreasing)

} # decreasingConf  

