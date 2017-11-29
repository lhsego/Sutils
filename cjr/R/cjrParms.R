#  Validates the correct user input of the
#  CJR parameters and then assigns the
#  parameter objects into the environment (or scope) of the
#  parent functions which calculate
#  Bayesian confidence, determine number of random samples, etc.


##'Check and insert CJR parameters into parent functions
##'
##'Validates the correct user input of the CJR parameters and then assigns the
##'parameter objects into the environment (or scope) of the parent functions
##'which calculate Bayesian confidence, determine number of random samples,
##'etc.
##' 
##'\code{cjrParms} has little use in being called directly.  It is called by
##'other functions in the \code{cjr} package which pass arguments into
##'\code{cjrParms} using the \code{...} argument.
##'
##'Either \code{prior.prob} or \code{beta} must be specified.  If both are
##'specified, \code{prior.prob} is ignored and the value of \code{beta} is
##'used.
##'
##' Either \code{pct.clean} or \code{thresh} must be specified.  If both are
##' specified, \code{pct.clean} is ignored and the value of \code{thresh} is
##' used.
##'
##'The parameterization of the Beta prior distribution is as follows, where
##'\code{alpha} \eqn{= a} and \code{beta} \eqn{= b}.  The density is given by
##'\deqn{f(x)=\frac{\Gamma(a+b)}{\Gamma(a)\Gamma(b)}{x}^{a} {(1-x)}^{b}%
##'}{Gamma(a+b)/(Gamma(a)Gamma(b))x^(a-1)(1-x)^(b-1)}\deqn{
##'}{Gamma(a+b)/(Gamma(a)Gamma(b))x^(a-1)(1-x)^(b-1)} for \eqn{a > 0}, \eqn{b >
##'0} and \eqn{0 \le x \le 1}{0 <= x <= 1} where the boundary values at
##'\eqn{x=0} or \eqn{x=1} are defined as by continuity (as limits).
##'
##'For all CJR models, \code{alpha = 1}.
##'
##' @export
##' 
##'@param n1 The number of judgment samples
##' 
##'@param N The total number of cells in the decision area
##' 
##'@param pct.clean Proportion of the decision area that needs to be clean. Can be specified
##' instead of \code{thresh}.
##' 
##'@param thresh A non-negative integer indicating the number of possible
##' unacceptable grid cells that will be tolerated.  Can be specified intead of \code{pct.clean}.
##' 
##'@param prior.prob The expected prior probability that a single judgment
##'sample location will contain detectable contamination
##' 
##'@param beta The second shape parameter of the prior distribution (which is a
##'Beta distribution).  Can be specified instead of \code{prior.prob}.  See
##'Details below.
##' 
##'@param r A high risk judgment sample location is expected to be \code{r}
##'times more likely to be contaminated than a low risk random sample location
##' 
##@param alpha The first shape parameter of the prior beta distribution.
##Always set to 1.
##'
##'@param fnr Numerical value in [0, 1) indicating whether the false negative rate for each sample.
##' 
##'@param arg.check \code{=TRUE} verifies that the choice for each parameter
##'(argument) is valid. \code{=FALSE} skips the verification. If the parameters have already been checked
##' and \code{cjrParms} is called multiple times (e.g. if a confidence function that uses \code{cjrParms} is
##' called multiple times in a sample size function), then it would make sense to set \code{arg.check = FALSE}
##' for subsequent calls to \code{cjrParms}.
##' 
##'@return Assigns these objects into the environment of the function that
##'called \code{cjrParms} (the parent frame):
##'\describe{
##'\item{n1}{The number of judgment samples}
##' \item{N}{The total number of cells in the decision area}
##' \item{pct.clean}{Proportion of the decision area that needs to be clean}
##' \item{thresh}{The number of unacceptable grid cells that will be tolerated}
##' \item{prior.prob}{The prior probability that a single judgment sample location will contain
##' detectable contamination}
##'\item{beta}{The second shape parameter of the prior (which is a Beta distribution)}
##' \item{r}{A high risk judgment sample location is \code{r}
##'times more likely to be contaminated than a low risk random sample location}
##'\item{alpha}{The first shape parameter of the prior beta distribution, always set to 1}
##'\item{fnr}{The false negative rate}
##'}
##'@author Landon Sego
##'@keywords misc
##'@examples
##' # Checking the parameters and making the parameters available as objects inside a function:
##' fun1 <- function(...) {
##'
##'   # Check the cjr parameters and assign them to the environment of fun1
##'   cjrParms(...)
##'   
##'   # Print a few of the objects that now reside in this environment
##'   pvar(n1, N, r, pct.clean, thresh, prior.prob, beta, fnr)
##'
##'  } # fun1
##'
##' fun1(n1 = 10, N=1000, pct.clean=0.99, prior.prob = 0.25, r=3)
##' 
##' # See what happens when not all the arguments are provided correctly:
##' try(fun1(n1 = 10, N = 1000))
##' try(fun1(n1 = 10001, N = 1000, pct.clean = 0.99, prior.prob = 1.25, r = -1, fnr = -0.3))
##'
##'
cjrParms <- function(n1 = NA,
                     N = NA,
                     pct.clean = NA,
                     thresh = NA,
                     prior.prob = NA,
                     beta = NA,
                     r = NA,
                     fnr = 0,
#                     alpha = 1,
                     arg.check = TRUE) {

  # Bare minimum set of checks for data types
  stopifnot(is.logical(arg.check))
      
  # All args should be numeric and of length 1
  firstCheck <- function(vTxt) {
    
    v <- get(vTxt)

    out <- NULL
      
    if (!is.na(v))  {

      if ((!is.numeric(v)) | (length(v) != 1))
        out <- paste("'", vTxt, "' must be numeric and length 1\n", sep = "")

    }

    return(out)

  } # firstCheck

  errMsg <- unlist(lapply(c("n1", "N", "pct.clean", "thresh", "prior.prob", "beta", "r", "fnr"),
                          firstCheck))
  
  if (!is.null(errMsg))
    stop(errMsg)

  # alpha has to be 1
  alpha <- 1

  # If prior.prob is specified (and beta is not), use it to select beta
  if (!is.na(prior.prob) & is.na(beta)) {
      
    beta <- (1 - prior.prob) / prior.prob
    
  }

  # If prior.prob is not specified and beta is, use it to define prior.prob
  else if (is.na(prior.prob) & !is.na(beta)) {

    prior.prob <- 1 / (1 + beta)
      
  }

  # If both are defined
  else if (!is.na(prior.prob) & !is.na(beta)) {
      
    warning("Since 'beta = ", beta, "' was specified, the value of 'prior.prob = ", prior.prob,
            "' will be ignored\n")
    
    prior.prob <- 1 / (1 + beta)
    
  }

  # Just a filler for computing pct.clean and thresh if N is missing.  We'll check the validity of
  # N later and ensure it's not missing
  if (!is.na(N))
    NtoUse <- N
  else
    NtoUse <- 100
    
  # If pct.clean is specified (and thresh is not), use it to select thresh
  if (!is.na(pct.clean) & is.na(thresh)) {
      
    # Set the threshold.  The initial 'round' is called to avoid machine error that might
    # occur if, for example, (1 - pct.clean) * N = 19.9999999, and should end up being
    # 20, but the floor function rounds it down incorrectly to 19.
    thresh <- floor(round((1 - pct.clean) * NtoUse, 14))      
          
  }
  # If pct.clean is not specified (and thresh is), use it to select pct.clean
  else if (is.na(pct.clean) & !is.na(thresh)) {
    pct.clean <- 1 - thresh / NtoUse
  }

  # Else if both are specified
  else if (!is.na(pct.clean) & !is.na(thresh)) {
    warning("Since 'thresh = ", thresh, "' was specified, the value of 'pct.clean = ", pct.clean,
            "' will be ignored\n")
    pct.clean <- 1 - thresh / NtoUse
  }

  # Now check arguments for completeness and consistency in the parameter space
  if (arg.check) {

    # All arguments need to be entered.  If not, return an intelligible message
    # to indicate which arguments need to be entered
    if (any(NA.args <- is.na(c(n1, N, thresh, alpha, r, beta, fnr)))) {
  
      arg.names <- c("'n1'", "'N'", "'thresh' or 'pct.clean'", "'alpha'", "'r'", "'prior.prob' or 'beta'",
                     "fnr")
      missingArgs <- arg.names[NA.args]
      
      if (sum(NA.args) > 1) {
          
        missingArgs1 <- missingArgs[1:(length(missingArgs) - 1)]
        msg <- paste(paste(missingArgs1, collapse=", "), " and ", missingArgs[length(missingArgs)], sep="")
        
        if (length(missingArgs) < 7)
          stop("Values for ", msg, " must also be entered.\n")
        else
          stop("Values for ", msg, " must be entered.\n")
      }
      else
        stop("A value for ", missingArgs, " must also be entered.\n")
  
    }

    # More specific checks on all variables
    errMsg <- NULL
    
    # n1 should be a non-negative integer
    if ((n1%%1 != 0) | (n1 < 0))
      errMsg <- c(errMsg, "'n1' must be a non-negative integer.\n")
  
    # N must be positive
    if (any(N <= 0))
      errMsg <- c(errMsg, "'N' must be positive.\n")

    # n1 must be <= N
    if (any(n1 > N))
      errMsg <- c(errMsg, "'n1' must be less than or equal to 'N'\n")

    # pct.clean should be between 0 and 1
    if ((pct.clean <= 0) | (pct.clean > 1))
      errMsg <- c(errMsg, "'pct.clean' must be in (0, 1].\n")

    # thresh should be a non-negative integer less than N
    if ((thresh %% 1 != 0) | (thresh < 0) | (thresh > N - 1))
      errMsg <- c(errMsg, "'thresh' must an integer in [0, N - 1]\n")

    # prior.prob should range in (0, 0.5]
    if ((prior.prob <= 0) | (prior.prob > 0.5))
      errMsg <- c(errMsg, "'prior.prob' should be in (0, 0.5]\n")
    
    # beta should be >= 1
    if (beta < 1)
      errMsg <- c(errMsg, "'beta' must be >= 1.\n")

    # r should be >= 1
    if (r < 1)
      errMsg <- c(errMsg, "'r' must be >= 1.\n")

    # If n1 = 0, then r necessarily has to be 1
    if ((n1 == 0) & (r > 1)) {
      errMsg <- c(errMsg, "When 'n1 = 0', the value of 'r' must be 1\n")
#      r <- 1
#      warning("Since 'n1 = 0', the value of 'r' has been set to 1.\n")
    }

    # fnr should be between in [0, 1)
    if ((fnr < 0) | (fnr >= 1))
      errMsg <- c(errMsg, "'fnr' must be in [0, 1).\n")

    # alpha should be > 0
    ## if (alpha <= 0)
    ##   errMsg <- c(errMsg, "'alpha' must be > 0.\n")

    ## if (alpha != 1)
    ##   warning("Routines in the 'cjr' library have only been validated for 'alpha = 1'.\n",
    ##           "Results for other values of alpha may or may not be correct.\n")

    if (!is.null(errMsg))
      stop(errMsg)

  } # if (arg.check)

  # Assign variables into the environment where it can be used by the
  # parent function that calls cjrParms
  assign("n1", n1, envir = parent.frame())
  assign("N",  N, envir = parent.frame())
  assign("thresh", thresh, envir = parent.frame())
  assign("pct.clean", pct.clean, envir = parent.frame())
  assign("prior.prob", prior.prob, envir = parent.frame())
  assign("beta", beta, envir = parent.frame())
  assign("r", r, envir = parent.frame())
  assign("fnr", fnr, envir = parent.frame())
  assign("alpha", alpha, envir = parent.frame())
  
} # cjrParms
