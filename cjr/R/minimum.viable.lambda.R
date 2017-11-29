# Are "all" lambda > viable.lambda also viable?

##'Determine whether "all" lambda greater than a known viable lambda are also
##'viable
##'
##'A validation function to determine whether "all" \code{lambda} greater than
##'a known viable \code{lambda} are also viable
##'
##'Checks the viability of a sequence of \code{lambda} values in the interval
##'[\code{viable.lambda}, 1].
##'
##' @export
##' 
##'@param conf The value of the Bayesian confidence
##'@param n1 The number of judgmental samples
##'@param n2 The number of random samples
##'@param r Ratio of prior expecation between judgmental and random samples
##'@param beta Second shape pararments of the beta prior of the judgmental
##'samples
##'@param viable.lambda A known viable \code{lambda}, likely obtained from
##'\code{\link{find.viable.lambda}}, or verifed to be viable using
##'\code{\link{is.viable.lambda}}.
##'@param maxN A large integer indicating the transition from finite population
##'to infinite population formulas.
##'@param max.seq.length The maximum number of \code{lambda} values in
##'[\code{viable.lambda}, 1] to test for viability
##'@param verbose \code{=TRUE} prints intermediate results
##'@return A list with the following components \item{all.viable}{A logical
##'indicating whether all the \code{lambda} in [\code{viable.lambda}, 1] were
##'viable} \item{num.not.viable}{The count of the number of times where the
##'\code{lambda} was not viable} \item{seq.length}{The length of the sequence
##'of \code{lambds} that was tested.} \item{pct.viable}{The percentage of the
##'tested \code{lambda} values that were viable} \item{results}{A data frame
##'showing the detailed results for the testing of each \code{lambda}.}
##'@author Landon Sego
##'@seealso \code{minimum.viable.lambda} is called by
##'\code{\link{n2.vs.viable.lambda}}.
##'@keywords misc
##'@examples
##'
##'v.lambda <- find.viable.lambda(0.8, 0.95, 10, 72, 2, 392, 10^6)
##'pvar(v.lambda)
##'minimum.viable.lambda(0.95, 10, 72, 2, 392, v.lambda, 10^6)
##'
minimum.viable.lambda <- function(conf, n1, n2, r, beta, viable.lambda, maxN, max.seq.length=50, verbose=FALSE) {

  # Now generate a sequence of viable lambda's
  lambda.seq <- seq(viable.lambda, 1, by = 5e-07)

  # If it's too long, then take an evenly spaced sample
  if (length(lambda.seq) > max.seq.length)
    lambda.seq <- unique(lambda.seq[seq(1, length(lambda.seq), length=max.seq.length)])

  seq.length <- length(lambda.seq)
  
  viable.seq <- rep(NA, seq.length)

  for (i in 1:seq.length) {
    
    vs <- is.viable.lambda(lambda.seq[i], n1, n2, r, beta, maxN, adjust=1e-14)
    viable.seq[i] <- vs$viable
   
    if (verbose) {
      cat("Call in minimum.viable.lambda:  is.viable.lambda(", pvar(lambda.seq[i], n1, n2, r, beta, maxN, verbose=FALSE), ")\n", sep="")
      print(vs)
    }
  }

  return(list(all.viable = all(viable.seq),
              num.not.viable = sum(!viable.seq),
              seq.length = seq.length,
              pct.viable = sum(viable.seq) / seq.length,
              results = data.frame(lambda = lambda.seq, viable = viable.seq)))

} # minimum.viable.lambda
