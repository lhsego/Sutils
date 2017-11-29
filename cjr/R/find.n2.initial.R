# Calculates the number of judgement samples needed to give the required level of
# confidence.


##'Finds the number of random samples needed to satisfy CJR input parameters
##'using the initial CJR model
##'
##'Finds the number of random samples needed to satisfy CJR input parameters,
##'using the initial CJR model of theta and theta/r.  Note, however, that the
##'CJR implementation in VSP is based on \code{\link{find.n2}}.
##'
##'When calling \code{find.n2.initial} directly, \code{arg.check} should always
##'be \code{TRUE}.
##'
##'@param conf A number in (0,1) giving the desired Bayesian confidence
##'@param \dots Arguments to \code{\link{cjrParms}}
##'@param method Only applies to the initial CJR model.  Method used to
##'calculate probabilities and sample sizes, see Details below.
##'@param int.method Only applies to the initial CJR model.  Indicates whether
##'numerical integrations are performed using Gaussian quadrature or Simpson's
##'method.  See Details below.
##'@param simpson.length Only applies to the initial CJR model.  An odd integer
##'indicating the number points at which the integrand function is evaluated
##'within the interval of integration when using Simpson's method. See Details
##'below.
##'@param round.up \code{=TRUE} rounds the sample size up to the nearest whole
##'integer
##'@param arg.check \code{=TRUE} checks the arguments supplied in \code{...}
##'using \code{\link{cjrParms}}
##'@return A list with the following components: \item{n2}{the number of random
##'samples} \item{conf.achieved}{The actual Bayesian confidence achieved by
##'\code{n2}}
##'@author Landon Sego
##'@seealso \code{find.n2} calls: \code{\link{cjrParms}} and
##'\code{\link{probRemain}}
##'@keywords misc
##'@examples
##'
##'# These produce the same result, since they use the same prior
##'# distribution, because beta = (1 - prior.prob) / prior.prob
##'find.n2.initial(0.95, n1=10, N=25000, pct.clean=0.99, r=2, prior.prob=0.05)
##'find.n2.initial(0.95, n1=10, N=25000, pct.clean=0.99, r=2, beta=19)
##'
find.n2.initial <- function(conf,
                            ...,
                            method = c("cont.binom", "floor"),        
                            int.method = c("quadrature", "simpson"),  
                            simpson.length = 3001,                    
                            round.up = FALSE,
                            arg.check = TRUE) {

  if (arg.check) {
    
    # Check that n2 is present
    if (missing(conf))
      stop("'conf', the first (and unnamed) argument, must be specified\n")
    
    # Check input for conf (the other values are checked in probRemain,
    if ((conf <= 0) | (conf > 1))
      stop("'conf' must be in (0,1].\n")
  
    # If conf >= 1, then uniroot will fail.  By setting conf to a specific
    # number very close to 1, we control the precision (instead of the machine)
    # that determines when the sample size is large enough to get confidence
    # 'close' to 1.
    if (conf >= 1) {
       cat("'conf >= 1' was requested, but 'conf' will be set to 0.9999999999999.\n")
       conf <- 0.9999999999999
    }
  }
  
  # Check the cjr arguments and assign them to this environment
  cjrParms(..., arg.check = arg.check)

  # Match args
  method <- match.arg(method)
  int.method <- match.arg(int.method)

  # checks
  stopifnot(is.numeric(simpson.length),
            simpson.length > 1)
  
  # Catch the silly cases
  if (n1 == N) {
    warning("n1 = N = ", n1, "\n")
    return(list(n2 = 0, conf.achieved = NA)) #, num.defective=NA))
  }

  # Make a one-variable wrapper for probRemain
  probRemainW <- function(n2) {
    probRemain(n2, ..., method = method, int.method = int.method,
               simpson.length = simpson.length, arg.check = FALSE)
  }

#  # Determine the number of defective cells
#  num.defective <- (1 - pct.clean) * N
#  if (method == "floor")
#    num.defective <- as.integer(floor(round(num.defective,14)))

  # Verify that confidence is not already acheived only with the judgement samples
  lo <- probRemainW(0)

  # Check to see if we have already achieved the confidence with just
  # the judgment samples
  if (lo >= conf) {
    n2 <- 0
    conf.achieved <- lo
  }

  # Otherwise, search for n2
  else {

    # Get a target value of n2 using uniroot (since it doesn't always give us
    # an n2 that results in P(Z >= (1-pct.clean)N | X=0, Y=0) >= conf
    if (class(try(
         n2.first <- uniroot(function(nrand) probRemainW(nrand) - conf, c(0, N - n1))
       )) == "try-error")
      stop("uniroot() failed in find.n2().\n")

    # A clumsy, discrete search for the n2 which just satisfies the confidence
    if (method == "floor") {

      get.n2 <- round(n2.first$root)
    
      # Using results from uniroot, iterate up one sample at a time
      # until confidence is achieved
  
      # Start out 10 samples below and make sure we're below the confidence level
      new.n2 <- max(0, get.n2 - 10)
      lo.2 <- probRemainW(new.n2)
  
      # If not low enough go lower and check
      if (lo.2 >= conf) {
  
        new.n2 <- max(0, get.n2 - 100)
        lo.2 <- probRemainW(new.n2)
  
        if (lo.2 >= conf) {
          pvar(lo.2,new.n2)
          stop("Should have gotten below the confidence level but did not.\n")
        }
      }
  
      # Now loop upward until confidence is achieved
      for (n2s in new.n2:(N - n1)) {
  
        conf.achieved <- probRemainW(n2s)
  
        if (conf.achieved >= conf) {
          n2 <- n2s
          break
        }
  
      } # for (n2s in n2.seq)
      
    } # if (method != "normal")

    # if method was continuous binomial
    else {

      n2 <- n2.first$root
      conf.achieved <- n2.first$f.root + conf

    }

  } # else

  # Because of the challenges we've had with the quadrature routine,
  # check that final confidence is still achieved using Simpson's method for the integration
  # if quadrature (the default) was used to calculate the integrals
  if (int.method == "quadrature") 
    conf.achieved.check <- probRemain(n2, ..., method = method, int.method = int.method,
                                      simpson.length = simpson.length, simpson.check = TRUE,
                                      arg.check = FALSE)

  # Round up the answer
  if (round.up) {
    
    n2.old <- n2
    n2 <- as.integer(ceiling(round(n2, 14)))

    # If rounding up has occured, then the acheived confidence will be slightly higher...
    if (abs(n2.old - n2) > 1e-06)
      conf.achieved <- probRemainW(n2)
  }

  # Return random sample size, the confidence acheived, and the number of defective
  # that can be tolerated
  return(list(n2 = n2, conf.achieved = conf.achieved)) #, num.defective=num.defective))
    
} # end find.n2()
