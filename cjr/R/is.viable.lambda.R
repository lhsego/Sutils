# Theorem based approach

##'Determine whether lambda produces a non-increasing confidence function
##'
##'Determine whether \code{lambda} produces a non-increasing confidence
##'function
##'
##'Determines whether the value of \code{target.lambda} is viable by applying
##'the theorem.
##'
##' @export
##' 
##'@param target.lambda The value of \code{lambda} to be tested.
##'@param n1 Number of judgmental samples
##'@param n2 Number of random samples
##'@param r Ratio of prior expecation between judgmental and random samples
##'@param beta Second shape parameter of the beta prior of the judgmental
##'samples
##'@param maxN A large integer indicating the transition from finite population
##'to infinite population formulas. This argument is now deprecated (not used)
##'but I've included it for backwards compatability with earlier versions.
##'@param adjust This is a small adjustment on the first condition so as to not
##'disqualify a viable lambda due to machine error.  The only time this value
##'should be greater than 0 is during validation excercises--e.g.
##'\code{\link{minimum.viable.lambda}} calls \code{is.viable.lambda} using
##'\code{adjust = 1e-14}.  This argument is now deprecated (not used) but I've
##'included it for backwards compatability with earlier versions.
##'@param decimal.precision The level of precision that will be used to compare
##'the \code{target.lambda} and the calculated viable lambda.
##'@param verbose Prints details. This argument is now deprecated (not used)
##'but I've included it for backwards compatability with earlier versions.
##'@return A list with the following elements \item{viable}{A logical (Boolean)
##'indicating whether \code{target.lambda} is viable (i.e. satisfies the
##'theorem)} \item{msg}{If \code{viable=FALSE}, a list containing the input
##'values of the function that might be used for diagnosis.}
##'@author Landon Sego
##'@seealso \code{\link{find.viable.lambda}}
##'@references PNNL-19315
##'@keywords misc
##'@examples
##'
##'is.viable.lambda(0.90, 10, 30, 3, 300, 10^6)
##'is.viable.lambda(0.99, 10, 30, 2, 5, 10^6, verbose=TRUE)
##'
is.viable.lambda <- function(target.lambda, n1, n2, r, beta, maxN, adjust=0, decimal.precision=5, verbose=FALSE) {

  k.n <- 0.5*(r*(n1+beta+1) - 2*n1 - n2)

  viable <- TRUE

  if (k.n > 1) {
    
    vl <- 1 - 1/k.n

    # Set the precision
    vl <- ceiling(vl * 10^decimal.precision) / 10^decimal.precision
    target.lambda <- ceiling(target.lambda * 10^decimal.precision) / 10^decimal.precision

#    pvar(k.n,vl,target.lambda,decimal.precision)
    
    if (vl > target.lambda)
      viable <- FALSE
  }

  # this is here for backward compatibility
  msgList <- list(target.lambda = target.lambda,
                  n1 = n1,
                  n2 = n2,
                  r = r,
                  beta = beta,
                  maxN = maxN,
                  conf.maxN = NULL,
                  conf.inf = NULL,
                  msg = NULL)
    
  return(list(viable = viable, msgList = msgList))

} #is.viable.lambda
  
# Determine whether this target value of lambda is viable or not...
# i.e., that it meets the following conditions:

# 1.  The conf at maxN is at least as great as the asymptotic confidence
# 2.  First derivative is negative for a sequence of N in [Nmin, maxN]

# If both conditions are met for the target lambda, then a value of TRUE is returned
# If not, a value of FALSE is returned, along with a list of parameter and calculated values
# which would hopefully explain why it is not a viable lambda

# John:  You won't need the 'adjust' argument, since it will always be 0 for the calls you make to
#        'is.viable.lambda'.  I just needed it for some of my validation functions

is.viable.lambda.old <- function(target.lambda, n1, n2, r, beta, maxN, adjust=0, verbose=FALSE) {

  # target.lambda of 1 is always viable
  if (target.lambda >= 1) {
    
    if (verbose)
      cat(pvar(target.lambda, verbose=FALSE), ", conditions are satisfied\n", sep="")
    
    return(list(viable = TRUE, msg = NULL))
  }

  # Initialze these variables
  viable <- FALSE
  msgList <- NULL
  msg <- NULL


  # Test the 2 conditions for a viable lambda
  
  # Initialize these variables (set to NA for the verbose option--so we can see
  # whether cond.2 actually was checked or not).
  cond.1 <- cond.2 <- NA

  # calculate confidences for large N (maxN) and infinite N (maxN + 1)
  cconf <- calcConf.raw(c(maxN, maxN + 1), n1, n2, r, beta, target.lambda, maxN, output="confidence")$confidence

  # Confidence at maxN
  conf.maxN <- cconf[1]

  # Confidence for infinite N
  conf.inf <- cconf[2]

  # Condition 1:  Confidence is "close" to the asymptotic confidence, but it can be above it--so maxN needs to be large
  cond.1 <- conf.maxN + adjust >= conf.inf

  # Test the derivative on the sequence of N if the first condition was satisfied
  # Conditin 2: The confidence function is decreasing for "all" N from Nmin to maxN
  if (cond.1) {
    
    cond.2 <- decreasingConf(target.lambda, n1, n2, r, beta, maxN)

    if (!cond.2) {
      
      msg <- c("Condition 2 failed:  Confidence is not decreasing over range of N:",
                pvar(target.lambda, n1, n2, r, beta, maxN, verbose=FALSE))

#      warning(paste(msg, collapse="\n"))
      
    }
  }

  # If conf is not sufficiently close to conf.inf, then maxN may not be large enough--and at maxN, this shouldn't happen, so:
  # if this warning takes place, we'll want to let the user know and ask them to contact us...
  else 
    msg <- c("Condition 1 failed: Asymptotic confidence greater than confidence at maxN",
              pvar(target.lambda, n1, n2, r, beta, maxN, conf.maxN, conf.inf, conf.maxN - conf.inf, verbose=FALSE))
    

  # Print values if asked, NA appearing for cond.2 indicates it wasn't tested
  if (verbose)
    pvar(target.lambda, cond.1, cond.2)

  # In case cond.1 was FALSE and cond.2 wasn't calculated (i.e. it's still NA)
  cond.2 <- ifelse(is.na(cond.2), FALSE, cond.2)

  # Determine whether viable
  viable <- cond.1 & cond.2
    

  # Add in a message (list with all the parameters and some of the computed quantities) to
  # be able to diagnose why the lambda was not viable.  Under some circumstances, this 'message' may need to
  # be recorded so that we can diagnose any potential problems.
  if (!viable) {
    
    msgList <- list(target.lambda = target.lambda,
                    n1 = n1,
                    n2 = n2,
                    r = r,
                    beta = beta,
                    maxN = maxN,
                    conf.maxN = conf.maxN,
                    conf.inf = conf.inf,
                    msg = msg)
    
  } # if (!viable) 
  

  return(list(viable = viable, msgList = msgList))

} # is.viable.lambda.old

