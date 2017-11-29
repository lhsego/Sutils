##' Calculate the CJR confidence for non-zero false negative rates
##' 
##' Calculates the Bayesian posterior probability of the number of contaminated
##' cells being less than a threshold, when the false negative rate is greater than zero
##'
##' Using the posterior predictive distribution, calculates the probability that
##' the number of contaminated cells is less than a determined threshold, given
##' that none of the judgmental nor random samples detected the presence of
##' contamination, and accounting for the false negative error rate.
##'
##' If \code{nProcesses = 1} an 
##' unparallized version that uses a slightly different algorithm is used,
##' and this can serve as a check for the parallelized version.
##'
##' @export
##' 
##'@param n2 The number of random samples
##'
##'@param \dots Arguments to \code{\link{cjrParms}}
##' 
##'@param arg.check \code{=TRUE} checks the arguments supplied in \code{...}
##'using \code{\link{cjrParms}}.  I would not recommend setting this to
##'\code{FALSE}.
##'
##' @param nProcesses Integer value of the number of cores to use in calculating the confidence in
##' parallel.  Defaults to one less than the total number of cores on the machine.
##'
##' @param verbose \code{=TRUE} prints details from the calculation of the confidence
##' 
##' @return The value of the Bayesian confidence
##' 
##' @author Landon Sego
##' 
##' @seealso \code{\link{calcConf}}
##' 
##' @keywords misc
##' @examples
##'
##'calcConf.fnr(239, fnr = 0, n1 = 10, N = 2500, pct.clean = 0.99, r = 2, prior.prob = 0.05)
##'calcConf.fnr(239, fnr = 0.1, n1 = 10, N = 2500, pct.clean = 0.99, r = 2, prior.prob = 0.05) 
##'calcConf.fnr(239, fnr = 0.4, n1 = 10, N = 2500, pct.clean = 0.99, r = 2, prior.prob = 0.05) 
##'calcConf.fnr(239, fnr = 0.7, n1 = 10, N = 2500, pct.clean = 0.99, r = 2, prior.prob = 0.05, nProcesses = 1)
##'calcConf.fnr(239, fnr = 0.7, n1 = 10, N = 2500, pct.clean = 0.99, r = 2, prior.prob = 0.05, nProcesses = 2) 
##'

calcConf.fnr <- function(n2, ..., arg.check = TRUE, nProcesses = detectCores() - 1, verbose = FALSE) {

  #  ... :  these are the required arguments to cjrParms:  'n1', 'N', 'pct.clean' or 'thresh', 'r', 
  #         'prior.prob' or 'beta', and 'fnr'

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
  
  # The following objects are now available for use in the environment of this function
  # n1
  # N
  # pct.clean
  # prior.prob
  # beta
  # r
  # fnr
  # thresh

  # n1 and n2 must not exceed N
  if (n1 + n2 > N)
    stop("n1 + n2 = ", n1 + n2, " > N = ", N, "\n")

  # Map notation into the form that matches the writeup on page 67 of Landon's notebook
  n.h <- n1
  n.l <- n2  
  beta.h <- beta
  phi.h <- fnr
  phi.l <- fnr
  rho <- r

  # Check whether we should be using the regular CJR
  if (fnr == 0) 
    return(calcConf(n2, 10^6, ..., arg.check = arg.check))

  # Else if fnr > 0
  # Here's a non-parallelized version of the confidence function that can be used to check
  # the parallelized version
  if (nProcesses == 1) {

    # Calculate and return the confidence--this uses the lbeta method 
    return(.C("calcConfFNR",
               as.integer(N),
               as.integer(n.h),
               as.integer(n.l),
               as.double(beta.h),
               as.double(phi.h),
               as.double(phi.l),
               as.double(rho),
               as.integer(thresh),
               as.integer(verbose),
               conf = double(1))$conf)

  } # If nProcesses = 1

  # Calculate various values that will be needed later
  v <- .C("calcBetaLoFNR",
          as.integer(n.h),
          as.integer(n.l),
          as.double(beta.h),
          as.double(phi.h),
          as.double(phi.l),
          as.double(rho),
          as.integer(verbose),
          beta.l = double(1),
          x.h.summands.3 = double(n.h + 1),
          x.l.summands.1 = double(n.l + 1),
          conf.denom = double(1))

  # Total number of combinations of x.h and x.l (the '+ 1' thing is for the 0's)
  aSize <- round((n.h + 1) * (n.l + 1), 0)
    
  # Set up the indexes of the parallel jobs, by dividing the index set 0:(aSize - 1) into
  # evenly sized groups
  nProcesses <- min(nProcesses, aSize)
  pJobIndexes <- lapply(parseJob(aSize, nProcesses), function(x) range(x) - 1)
  
  # Wrapper for the calculating the confidence
  cConf <- function(index) {

    res <- try(.C("calcConfFNR_parallel",
                  as.integer(N),
                  as.integer(n.h),
                  as.integer(n.l),
                  as.integer(index[1]),
                  as.integer(index[2]),
                  as.double(beta.h),
                  as.double(v$beta.l),
                  as.double(phi.h),
                  as.double(phi.l),
                  as.double(rho),
                  as.double(v$x.h.summands.3),
                  as.double(v$x.l.summands.1),
                  as.integer(thresh),
                  as.integer(verbose),
                  num.partial = double(1))$num.partial)

    if (class(res) == "try-error") {
      cat("Failure in '", calcConfFNR, "' at ", pvar(index, verbose = FALSE), "\n", sep = "")
      return(NULL)
    }
    else
      return(res)
      
  } # cConf

  # Now calculate the partial sums of the numerator of the confidence in parallel if
  # we're not on a Windows machine
  if (Sys.info()['sysname'] == 'Windows') {
    numerator <- sum(unlist(lapply(pJobIndexes, cConf))) 
  } else {  
    numerator <- sum(unlist(mclapply(pJobIndexes, cConf, mc.cores = nProcesses)))
  }

  if (verbose)
    pvar(numerator)

  # Calculate the confidence
  return(numerator / v$conf.denom)
  
} # calcConf.fnr
