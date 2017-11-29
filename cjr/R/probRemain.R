# This function gives P(Z <= (1-pct.clean)*N | X=0 & Y=0).
# It's the predictive posterior
# distribution of the number of contaminated cells that remain.  

##'Calculates the Bayesian posterior probability of the number of contaminated
##'cells being less than a threshold, using initial CJR model
##'
##'Using the posterior predictive distribution, calculates the probability that
##'the number of contaminated cells is less than a determined threshold, given
##'that none of the judgmental nor random samples detected the presence of
##'contamination.  This follows the initial CJR model which uses theta and
##'theta/r.  Note, however, that the CJR implementation in VSP is based on
##'\code{\link{calcConf}}.
##'
##'When calling \code{probRemain} directly, \code{arg.check} should always be
##'\code{TRUE}.  It is set to false when it is called by \code{\link{find.n2}}.
##'
##'For the initial CJR model where P(unacceptable judgmental sample) = theta
##'and P(unacceptable random sample) = theta/r, the functions
##'\code{\link{find.n2.initial}} and \code{\link{probRemain}} can be used.  The
##'arguments \code{method}, \code{int.method}, and \code{simpson.length} are
##'only applicable to those two functions.
##'
##'When chosing \code{method = "floor"}, the number of tolerable contaminated
##'cells is chosen using \code{floor((1 - pct.clean) * N)}, and the standard,
##'discrete binomial distribution is used to calculate probabilities.  This
##'results in confidence statements like "95\% confident that at least 99\% of
##'the grid cells do not contain detectable contamination."
##'
##'If \code{method = "cont.binom"}, the number of tolerable contaminated cells
##'is simply \code{(1 - pct.clean) * N}, and a continuous version of the
##'binomial distribution is used to calculate probabilities.  Results in
##'confidence statements like "95\% confident that 99\% of the decision area
##'does not contain detectable contamination."
##'
##' @export
##' 
##'@param n2 The number of random samples
##'@param \dots Arguments to \code{\link{cjrParms}}
##'@param plot.integrands \code{=TRUE} produces a plot (using the X11 device)
##'of the integrands of the numerator and denominator of the Bayesian
##'confidence.  \code{="filename.pdf"} produces the same plot but writes it to
##'"filename.pdf".
##'@param method Only applies to the initial CJR model.  Method used to
##'calculate probabilities and sample sizes, see Details below.
##'@param int.method Only applies to the initial CJR model.  Indicates whether
##'numerical integrations are performed using Gaussian quadrature or Simpson's
##'method.  See Details below.
##'@param simpson.length Only applies to the initial CJR model.  An odd integer
##'indicating the number points at which the integrand function is evaluated
##'within the interval of integration when using Simpson's method. See Details
##'below.
##'@param simpson.check \code{=TRUE} checks the integration results from the
##'quadrature using Simpsons' method.  If the results differ, a detailed
##'message is printed.  If \code{int.method='simpson'} is specified in
##'\code{...}, then no check is performed.
##'@param arg.check \code{=TRUE} checks the arguments supplied in \code{...}
##'using \code{\link{cjrParms}}
##'@return The probability that the sampling area is \code{pct.clean} x 100\%
##'clean, given that none of the judgmental nor random samples detected the
##'presence of contamination.
##'@author Landon Sego
##'@seealso \code{probRemain} called by: \code{\link{find.n2}}, Calls:
##'\code{\link{cjrParms}}
##'@keywords misc
##'@examples
##'
##'# These produce the same result, since they use the same prior
##'# distribution, because beta = (1 - prior.prob) / prior.prob
##'probRemain(239, n1=10, N=25000, pct.clean=0.99, r=2, prior.prob=0.05)
##'probRemain(239, n1=10, N=25000, pct.clean=0.99, r=2, beta=19)
##'
probRemain <- function(n2, ...,
                       method = c("cont.binom", "floor"),        
                       int.method = c("quadrature", "simpson"),  
                       simpson.length = 3001,                    
                       plot.integrands = FALSE,
                       simpson.check = FALSE,
                       arg.check = TRUE) {

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

  # Match args
  method <- match.arg(method)
  int.method <- match.arg(int.method)

  # checks
  stopifnot(is.numeric(simpson.length),
            simpson.length > 1)
  
  # This reduces the number of probabilistic samples to the nearest
  # whole number, which tips P(Z=z | X=0 & Y=0) toward the conservative side.
  # (and n2 needs to be a whole number).
  if (method == "floor")
    n2 <- as.integer(floor(round(n2, 14)))

  # N - n1 - n2, the number remaining, must be at least 0
  if ((N - n1 - n2) < 0)
    stop("'n1 + n2' must be no greater than 'N'.\n")

  # Determine the number of tolerable defective cells
  num.defective <- (1 - pct.clean) * N
  if (method == "floor")
    num.defective <- as.integer(floor(round(num.defective, 14)))
  
  # This is the likelihood for the unsampled cells multiplied
  # by the kernel of the posterior for theta (numerator)
  # Note:  pcbinom comes from the 'pnlStat' library
  if (method == "cont.binom") {
    f1 <- function(theta) 
      pcbinom(num.defective, N-n1-n2, theta/r) * (1-theta/r)^n2 * theta^(alpha-1) * (1-theta)^(beta+n1-1)
  }
  else {
    f1 <- function(theta) 
      pbinom(num.defective, N-n1-n2, theta/r) * (1-theta/r)^n2 * theta^(alpha-1) * (1-theta)^(beta+n1-1)
  }
  
  # This is the kernel of the posterior of theta (denominator)
  f2 <- function(theta)
    (1-theta/r)^n2 * theta^(alpha-1) * (1-theta)^(beta+n1-1)


  # Alternative approach to the numerator.  This keeps the summand
  # outside the integral--every time I've tested it, it comes out equal
  #  f3 <- function(theta) {
  #    dbinom(z, N-n1-n2, theta/r) *
  #    (1-theta/r)^n2 * theta^(alpha-1) * (1-theta)^(beta+n1-1)
  #  }
  #
  #  num2 <- 0
  #  for (z in 0:floor((1-pct.clean)*N))
  #    num2 <- num2 + integrate(f3, 0, 1, subdivisions=1000)$value

  
  # Allows the 'plot.integrands' argument to be logical or a filename
  if (is.character(plot.integrands)) {
    gfile <- plot.integrands
    plot.integrands <- TRUE
  }
  else
    gfile <- "launch.X11"

  # Shows plot of numerator and denominator
  if (plot.integrands) {

    if (alpha != 1)
      warning("Integrands plot may not work for 'alpha != 1'\n")
    if (gfile == "launch.X11") {
      X11(height=5, width=7)
      devAskNewPage(ask = TRUE)
    }
    else
      pdf(file=gfile, height=5, width=7)
    
    par(las=1, cex.main=0.8, cex.lab=0.8, cex.axis=0.8)

    # Find where it meets 0
    cut.num1 <- uniroot(function(x) f2(x) - 1e-06, c(0,1))$root 
    cut.num2 <- uniroot(function(x) f1(x) - 1e-06, c(0,1))$root
    cut.num <- max(cut.num1, cut.num2)

    theta.1 <- seq(0, min(cut.num, 1), length=500)
    num.1 <- f1(theta.1)
    den.1 <- f2(theta.1)

    title <- paste(pvar(N,n1,n2,pct.clean,verbose=FALSE), "\n",
                   pvar(beta,r,alpha,verbose=FALSE), sep="")

    plot(theta.1, num.1, ylim=range(num.1,den.1), main=title,
         ylab="Integrand", xlab=expression(theta), type="l")
    lines(theta.1, den.1, col="Blue")

    legend(0.65*cut.num, 1, c("Numerator","Denominator"), lwd=3, col=c("Black","Blue"), lty=1, cex=0.9)

    # Second plot showing integrands for calc.int3() methodology
    theta.1 <- seq(0,1,length=500)
    num.1 <- f1(theta.1) + theta.1^2
    den.1 <- f2(theta.1) + theta.1^2

    title <- paste(pvar(N,n1,n2,pct.clean,verbose=FALSE), "\n",
                   pvar(beta,r,alpha,verbose=FALSE), sep="")

    plot(theta.1, num.1, ylim=range(num.1,den.1), main=title,
         ylab="Integrand + theta^2", xlab=expression(theta), type="l")
    lines(theta.1, den.1, col="Blue")
    legend(0.3, 1, c("Numerator","Denominator"), lwd=3, col=c("Black","Blue"), lty=1, cex=0.9)

    if (gfile != "launch.X11") {
      dev.off()
      cat("Plot written to '", gfile, "'\n", sep="")
    }

  } # if plot.integrands()

  # A simple integration function based on the idea
  # of adding the function f(x) = x^2 to the integrands so that they are no longer
  # constant as they go to 1 and then subtracting out 1/3 at the end.
  # Assumes that f1 + theta and f2 + theta have a unique minimum,
  # which implies that f1 and f2 are strictly decreasing, which will occur when
  # alpha = 1 and beta > 1.
  calc.int3 <- function() {

    f1.1 <- function(theta) {
      f1(theta) + theta^2
    }

    f2.1 <- function(theta) {
      f2(theta) + theta^2
    }

    # Find minimums of the functions and use for cutpoints
    min.1 <- optimize(f1.1, interval=c(0,1), tol=1e-12)
    min.2 <- optimize(f2.1, interval=c(0,1), tol=1e-12)

    # Protect the algorithm
    if (min.1$objective >= 1/1.05)
      stop("min.1$objective >= 1/1.05\n")
    if (min.2$objective >= 1/1.05)
      stop("min.2$objective >= 1/1.05\n")

    # Find cutpoints to the left (lt) and right (rt) of the minimum, to divide the domain into 3 intervals
    cut.1.lt <- uniroot(function(x) f1.1(x) - 1.05 * min.1$objective, c(0, min.1$minimum), tol=1e-07)$root
    cut.1.rt <- uniroot(function(x) f1.1(x) - 1.05 * min.1$objective, c(min.1$minimum, 1), tol=1e-07)$root
    cut.2.lt <- uniroot(function(x) f2.1(x) - 1.05 * min.2$objective, c(0, min.2$minimum), tol=1e-07)$root
    cut.2.rt <- uniroot(function(x) f2.1(x) - 1.05 * min.2$objective, c(min.2$minimum, 1), tol=1e-07)$root

    # Make sure cut points are ordered properly
    if ((cut.1.lt < 0) | (cut.1.lt > cut.1.rt) | (cut.1.rt > 1) |
        (cut.2.lt < 0) | (cut.2.lt > cut.2.rt) | (cut.2.rt > 1)) {
      pvar(cut.1.lt, cut.1.rt, cut.2.lt, cut.2.rt, digits=8)
      stop("Cut points in calc.int3 not ordered correctly\n")
    }

    # Perform the integrals over the 3 intervals
    if (int.method == "quadrature") {
      n1 <- try(integrate(f1.1, 0,        cut.1.lt, rel.tol=1e-14)$value)
      n2 <- try(integrate(f1.1, cut.1.lt, cut.1.rt, rel.tol=1e-14)$value)
      n3 <- try(integrate(f1.1, cut.1.rt, 1,        rel.tol=1e-14)$value)
      
      d1 <- try(integrate(f2.1, 0,        cut.2.lt, rel.tol=1e-14)$value)
      d2 <- try(integrate(f2.1, cut.2.lt, cut.2.rt, rel.tol=1e-14)$value)
      d3 <- try(integrate(f2.1, cut.2.rt, 1,        rel.tol=1e-14)$value)
    }
    else {
      n1 <- try(integ(f1.1(seq(0,        cut.1.lt, length=simpson.length)), a = 0,        b = cut.1.lt))
      n2 <- try(integ(f1.1(seq(cut.1.lt, cut.1.rt, length=simpson.length)), a = cut.1.lt, b = cut.1.rt))
      n3 <- try(integ(f1.1(seq(cut.1.rt, 1,        length=simpson.length)), a = cut.1.rt, b = 1))

      d1 <- try(integ(f2.1(seq(0,        cut.2.lt, length=simpson.length)), a = 0,        b = cut.2.lt))
      d2 <- try(integ(f2.1(seq(cut.2.lt, cut.2.rt, length=simpson.length)), a = cut.2.lt, b = cut.2.rt))
      d3 <- try(integ(f2.1(seq(cut.2.rt, 1,        length=simpson.length)), a = cut.2.rt, b = 1))      
    }
      
    result <- NA

    if (!((class(n1) == "try-error") | (class(n2) == "try-error") | (class(n3) == "try-error") |
          (class(d1) == "try-error") | (class(d2) == "try-error") | (class(d3) == "try-error"))) {
      
      # Correct for having added in x^2
      numerator <- n1 + n2 + n3 - 1/3
      denominator <- d1 + d2 + d3 - 1/3
      
      if (numerator==Inf) {
        pvar(numerator)
        cat("numerator will be set to 'NA'.\n")
        numerator <- NA
      }
    
      if (denominator==Inf) {
        pvar(denominator)
        cat("denominator will be set to 'NA'.\n")
        denominator <- NA
      }  
    
      if (numerator==0 & denominator==0) {
        cat("Integral is 0/0 which will be set to 1.\n")
        result <- 1
      }
      else
        result <- numerator / denominator
      
    }

    return(result)
    
  } # calc.int3()

  # Straight Simpsons approach, without dividing the domain into subintervals, no transformations, etc.
  # This method can be imprecise in extreme cases (extremely steep decrease in the numerator or denominator)
  # But at least it doesn't make any assumptions regarding the shape of the integrands
  calc.int4 <- function() {
    
    eval.seq <- seq(0, 1, length = simpson.length * 4 + 1)

    if (class(numerator <- try(integ(f1(eval.seq), a = 0, b = 1))) == "try-error")
      stop("numerator integ() failed in calc.int4()\n")
    if (class(denominator <- try(integ(f2(eval.seq), a = 0, b = 1))) == "try-error")
      stop("denominator integ() failed in calc.int4()\n")

    if (numerator==Inf) {
      pvar(numerator)
      cat("numerator will be set to 'NA'.\n")
      numerator <- NA
    }
  
    if (denominator==Inf) {
      pvar(denominator)
      cat("denominator will be set to 'NA'.\n")
      denominator <- NA
    }  
  
    if (numerator==0 & denominator==0) {
      cat("Integral is 0/0 which will be set to 1.\n")
      result <- 1
    }
    else
      result <- numerator / denominator
    
    return(result)
  }
  

  # Calculate the integrals
  if ((alpha == 1) & (beta >= 1)) {

    c3.error <- FALSE
    if (class(res <- try(calc.int3(), silent=TRUE)) == "try-error") {
      warning("calc.int3() failed in probRemain():\n", res, "\n",
              "Will attempt to calculate the integral using calc.int4()\n")
      res <- calc.int4()
      c3.error <- TRUE
    }

    # Check the method using Simpson's if requested
    if ((!c3.error) & simpson.check & (int.method == "quadrature")) {
      
      int.method <- "simpson"
      simpson.result <- calc.int3()
      m3 <- data.frame(x=res)
      simp <- data.frame(x=simpson.result)
    
      if (!dframeEquiv(m3, simp, maxAbsError=1e-05, maxRelError=1e-05, verbose=FALSE)$equiv) {
        quadrature.result <- res
        cat("Warning in probRemain check:  Integration results do not match up to maxAbsError=1e-05 and maxRelError=1e-05:\n")
        pvar(quadrature.result, simpson.result, abs(quadrature.result - simpson.result), digits=7)
      }
    }
    
  }
  else
    res <- calc.int4()

  if (is.na(res))
    warning("NA returned by probRemain()\n")

  return(res)

} # end probRemain()
