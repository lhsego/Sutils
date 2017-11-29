# Theorem based approach

# Many of these arguments are preserved for backward compatibility


##'Calculate the viable value of lambda that ensures a non-increasing
##'confidence function
##'
##'Calculate the viable value of \code{lambda} that ensure a non-increasing
##'confidence function.
##'
##'
##' @export
##' 
##'@param target.lambda The starting value of \code{lambda}
##'@param conf The desired confidence level.
##'@param n1 Number of judgmental samples
##'@param n2 Number of random samples
##'@param r Ratio of prior expecation between judgmental and random samples
##'@param beta Second shape parameter of the beta prior of the judgmental
##'samples
##'@param maxN A large integer indicating the transition from finite population
##'to infinite population formulas.  This argument is now deprecated (not used)
##'but I've included it for backwards compatability with earlier versions.
##'@param decimal.precision The precision of the solution.
##'@param verbose Prints details showing the search This argument is now
##'deprecated (not used) but I've included it for backwards compatability with
##'earlier versions.
##'@param show.plot \code{=TRUE} makes a plot which illustrates the
##'identification of the viable percentage clean value.  This argument is now
##'deprecated (not used) but I've included it for backwards compatability with
##'earlier versions.
##'@return
##'
##'The value of \code{lambda} which produces a decreasing confidence function
##'(wrt \code{N}).
##'
##'If the value of \code{target.lambda} already produces a decreasing
##'confidence function, then that value is simply returned.  Otherwise, it
##'calculates the smallest value of \code{lambda} that is viable, according to
##'the theorem in the CJR report.
##'@author Landon Sego
##'@seealso \code{\link{is.viable.lambda}}, \code{\link{n2.vs.viable.lambda}}
##'@references PNNL-19315
##'@keywords misc
##'@examples
##'
##'  find.viable.lambda(0.99, 0.98, 10, 0, 3, 150, 10^6, verbose=TRUE, show.plot=TRUE)
##'
find.viable.lambda <- function(target.lambda, conf, n1, n2, r, beta, maxN, decimal.precision=5,
                               verbose=FALSE, show.plot=FALSE) {

  if (is.viable.lambda(target.lambda, n1, n2, r, beta, maxN)$viable)
    vl <- target.lambda
  else 
    vl <- 1 - 1 / (0.5*(r*(n1+beta+1) - 2*n1 - n2))

  return(ceiling(vl * 10^decimal.precision) / 10^decimal.precision)
  
} # find.viable.lambda

# Find the value of pct.clean that will give a decreasing confidence function of N
# Only finds suitable pct.clean to the nearest ten-thousandth

# This function is best understood by calling it with 'verbose=TRUE' and 'show.plot=TRUE'

find.viable.lambda.old <- function(target.lambda, conf, n1, n2, r, beta, maxN, decimal.precision=5,
                                   verbose=FALSE, show.plot=FALSE) {

  # Record the object for error printing later
  original.target.lambda <- target.lambda

  target.lambda <- round(target.lambda, decimal.precision + 1)

  if (show.plot) {

    # Close any open graphics devices
#    if (length(dev.list()))
#      graphics.off()

    # Open a new graphics device
    X11(height=7, width=17)
    par(mfrow=c(1,3))

    # Plotting the confidence as a function of N
    show.plot.f <- function(n2, lambda, title=NULL) {
      
      Nmin <- (n1 + n2) / lambda + 0.1
      Nseq <- c(exp(seq(log(Nmin), log(maxN), length=10^4)), maxN + 1)
      conf.seq <- calcConf.raw(Nseq, n1, n2, r, beta, lambda, maxN, output="confidence")$confidence
      ylims <- range(c(conf.seq, conf))
      plot(log10(Nseq[-length(Nseq)]), conf.seq[-length(conf.seq)], ylim=ylims, xlab="log10(N)", ylab="Confidence", type="l",
           main=title, font.main=1)
      abline(h = conf.seq[length(conf.seq)], col="Blue")
      abline(h = conf, col="Green")
      legend(log10(maxN / 50), 1, c("Aymptotic Conf", "Desired Conf"), col=c("Blue", "Green"), lty=1, lwd=3)

    } # show.plot.f

    # Show the initial conditions
    show.plot.f(n2, target.lambda,
                title=paste("Initial Conditions: ",
                            pvar(n2, target.lambda, verbose=FALSE, digits=5)))    

  } # if show.plot

  
  # lambda's of 1 are always viable (we can prove this mathematically)
  if (target.lambda >= 1) {

    if (verbose)
      cat("A target.lambda of 1 is always viable\n")
    
    return(1)

  }


  # A convenient wrapper function for is.viable.lambda
  is.viable.lambda.W <- function(lambda.test, n2.test) {

    is.viable.lambda(lambda.test, n1, n2.test, r, beta, maxN, verbose=verbose)$viable

  } # is.viable.lambda.W

  
  # Is the original choice of lambda viable? 
  if (is.viable.lambda.W(target.lambda, n2)) {
    
    if (verbose)
      cat("Initial target.lambda =", target.lambda, " results in decreasing confidence as a function of N.\n")

    return(target.lambda)
    
  }

  
  if (verbose)
    cat("Inital target.lambda =", target.lambda, "\n")


  # A scanning function to move over each level
  scan.f <- function(prec, t.lambda) {

    # First move up by prec
    if (t.lambda < (1 - prec)) {

      # Vector to move over
      lambda.vec <- (1:(1/prec)) * prec
      lambda.vec <- round(lambda.vec[lambda.vec > t.lambda], decimal.precision + 1)

      if (verbose) {
        cat("Moving up by", prec, ":\n")
        pvar(t.lambda)
      }

      for (i in 1:length(lambda.vec)) {


        if (is.viable.lambda.W(lambda.vec[i], n2))
          break
        
        t.lambda <- lambda.vec[i]
        
      }
  
      
    }
    else {
      if (verbose)
        cat("Since target.lambda = ", t.lambda, ", skipping scanning by ", prec, "\n", sep="")
    }
      
    return(t.lambda)

  } # scan.f

  # Scan upwards, searching for the viable lambda by each of the increments
  increments <- round(10^(-(1:decimal.precision)), decimal.precision + 1)
  
  for (i in increments)
    target.lambda <- scan.f(i, target.lambda)
  
  # If we've met our target, then target.lambda + smallest.precision should satisfy the conditions and 
  # target.lambda should not
  last.increment <- increments[length(increments)]
  
  suitable.lambda <- round(min(1, target.lambda + last.increment), decimal.precision + 1)


  # Comparison plot:
  if (show.plot) {
    show.plot.f(n2, suitable.lambda,
                title=paste("Recommended lambda: ",
                            pvar(n2, suitable.lambda, verbose=FALSE, digits=5)))
  }

  # Sanity Checks
  if (verbose)
    cat("Final check conditions should not be satisfied: ", pvar(target.lambda, verbose=FALSE), "\n")

  # target.lambda should not be viable
  if (is.viable.lambda.W(target.lambda, n2))
    stop("Search algorithm for suitable target.lambda did not perform as expected: ", pvar(target.lambda, verbose=FALSE), ".\n",
         "Call:  find.viable.lambda(", pvar(original.target.lambda, conf, n1, n2, r, beta, maxN, verbose=FALSE), ")\n")

  if (verbose)
    cat("Final check, conditions should be satisifed: ", pvar(suitable.lambda, verbose=FALSE), "\n")
  
  # suitable.lambda should be viable
  if (!is.viable.lambda.W(suitable.lambda, n2))
    stop("Search for suitable.lambda = ", suitable.lambda,
         " did not achieve decreasing confidence as a function of N as expected and\n",
         "confidence greater than the asymptotic confidence.\n",
         "Call:  find.viable.lambda(", pvar(original.target.lambda, conf, n1, n2, r, beta, maxN, verbose=FALSE), ")\n")


  # Add 1 more increment in order to make it more robust for situations where users are
  # exploring a number of N's  (i.e., if I keep all the parameters the same, but only change N, this will help keep
  # the viable lambda more consistent, since it does depend on N)
  return(suitable.lambda) 

} # find.viable.lambda.old
