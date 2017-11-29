#### THIS FUNCTION IS ANTIQUATED.  SHOULD USE 'designGammaCusum' or 'calcCusum'

################################################################################
#
# Filename:             gammaCusum.R
# Creator:              Landon Sego
# Date Created:         2007-01-09
# Description/Purpose:  Calculate the gamma based CUSUM chart statistics
#                       This was originally created for use with the NASA NRT
#                       study
#
# Modified by:
# Date modified:
# Brief summary of changes:
#
# Modified by:
# Date modified:
# Brief summary of changes:
# 
# Modified by:
# Date modified:
# Brief summary of changes:
#
#
################################################################################

################################################################################
#
# '*' denotes a required field
#
# * Function name:
#
# * Title (one line summary of function):
#
#
#
# * Brief Description (2-5 sentences):
#
#
#
# * Usage (function call which shows arguments and defaults):
#
#
#
# * Arguments (specify name, data type, and description for each argument):
#
#  X = named vector of data points in chronological order (names are time labels). 
#
#  beta.shift = a positive number indicating the magnitude in the shift of the
#               scale parameter, beta. Process shifts from beta_0 to
#               beta_1 = beta.shift * beta_0.
#
#  historical = a numeric value indicating the amount of historical data.  If < 1,
#               uses the first 'historical'*100% obs to estimate the in-control
#               parameter values, if > 1, it uses the first 'historical' obs.
#
#  cheb.level.1 = a numeric value between 0 and 1/6 indicating the probability
#                 to be used to identify the first cut of the upper-tailed 2-tiered Chebyshev limit
#
#  cheb.level.2 = a numeric value between 0 and 1/6 indicating the probability
#                 to be used to identify the second cut of the upper-tailed 2-tiered Chebyshev limit
#
#  exp.cheby = TRUE means that the the Chebyshev limit will be calculated on the exponentiated
#              data and then the the logarithm of that limit will be reported
#
#  arl.0 = the target, in-control ARL that will be used to set the control limit. If
#          NULL, then the value of h will be used.
#
#  h = the control-limit.  If NULL, the arl.0 will be used to find the control limit.
#      If both are specified, then arl.0 is given preference.
#
#  ic.parms = list of in-control estimate of alpha an beta for historical data.  If
#             this is left null (default), the parameters are estimated from the historical
#             data. If they are provided, all data is considered to be recent (non-historical).
#
#  process.historical = TRUE means that the CUSUM is also calculated for the historical
#                       data.  If FALSE, the CUSUM begins on the first observation
#                       after the historical data
#
#  reset = TRUE means the CUSUM will reset to 0 after crossing the control-limit.
#          If FALSE, is will just keep running.
#
#  verbose = TRUE prints extra details about the calculation of the CUSUM
#
#  title = character string to label the fitting plots
#
#  fit.hist.only = TRUE only performs the ML-fit of the Gamma distribution to
#                  the historical data and does not design nor calculate the CUSUM.
#
#  ... = additional arguments to findGammaCusumCL
#
#  
#
# 
# Details (extra details that were too extensive to include in the description):
#
#
#
# * Value (description of objects returned by the function):
#
#
#
# * Author:
#
# Notes (any further notes):
#
#
#
# See Also (reference any other related functions):
#
#
#
################################################################################

gammaCusum <- function(X, beta.shift,
                       historical=100,
                       cheb.level.1=0.01,
                       cheb.level.2=0.01,
                       exp.cheby=FALSE,
                       arl.0=NULL,
                       h=NULL,
                       ic.parms=list(alpha=NULL, beta=NULL),
                       process.historical=FALSE,
                       reset=TRUE,
                       verbose=FALSE,
                       title=NULL,
                       fit.hist.only=FALSE,
                       ...) {

  ##################################################
  # Check arguments
  ##################################################
  
  # Verify X is valid
  if (!is.numeric(X))
    stop(deparse(substitute(X)), " must be a numeric vector.\n")
  if (is.null(names(X)))
    stop(deparse(substitute(X)), " must have time stamps (or other labels) for names.\n")

  # Verify that beta is valid
  if (!is.numeric(beta.shift))
    stop("'beta.shift' must be a positive real number.\n")
  if (beta.shift <= 0)
    stop("'beta.shift' must be a positive real number.\n")

  # Check historical
  if (!is.numeric(historical))
    stop("'historical' must be a positive real number.\n")
  if (historical < 0)
    stop("'historical' must be a positive real number.\n")

  # Check cheb levels
  if (!is.numeric(cheb.level.1))
    stop("'cheb.level.1' must be a real number in (0, 1/6).\n")
  if ((cheb.level.1 <= 0) | (cheb.level.1 > 1/6))
    stop("'cheb.level.1' must be a real number in (0, 1/6).\n")
  if (!is.numeric(cheb.level.2))
    stop("'cheb.level.2' must be a real number in (0, 1/6).\n")
  if ((cheb.level.2 <= 0) | (cheb.level.2 > 1/6))
    stop("'cheb.level.2' must be a real number in (0, 1/6).\n")  

  # Both arl.0 and h cannot be null
  if (is.null(arl.0) & is.null(h))
    stop("Either of 'arl.0' or 'h' must be specified.\n")

  # If both are specifed, arl.0 takes precedence
  if (!is.null(arl.0) & !is.null(h))
    h <- NULL

  # Check arl.b0.  Must be > 0
  if (!is.null(arl.0)) {
    if (!is.numeric(arl.0))
      stop("'arl.0' must be a positive real number.\n")
    if (arl.0 <= 0)
      stop("'arl.0' must be a positive real number.\n")
  }

  # Or Check h.  Must be > 0
  else {
    if (!is.numeric(h))
      stop("'h' must be a positive real number.\n")
    if (h <= 0)
      stop("'h' must be a positive real number.\n")
  }

  # Check ic.parms
  if (!(all(names(ic.parms) == c("alpha","beta"))))
    stop("'ic.parms' must be a list of 2 with names 'alpha' and 'beta'\n")

  if (!is.null(ic.parms$alpha)) {
    if (!is.numeric(ic.parms$alpha))
      stop("'ic.parms$alpha' must be a positive real number.\n")
    if (ic.parms$alpha <= 0)
      stop("'ic.parms$alpha' must be a positive real number.\n")
  }

  if (!is.null(ic.parms$beta)) {
    if (!is.numeric(ic.parms$beta))
      stop("'ic.parms$beta' must be a positive real number.\n")
    if (ic.parms$beta <= 0)
      stop("'ic.parms$beta' must be a positive real number.\n")
  }  

  ##################################################
  # Get historical data vector
  ##################################################

  if (historical <= 1) 
    hX <- X[1:ceiling(historical*length(X))]
  else {
    if (historical <= length(X))
      hX <- X[1:ceiling(historical)]
    else {
      warning("'historical' = ", historical, " is larger than the number of data points.\n",
              "All the data will be used as historical data.\n")
      hX <- X
    }
  }

  # Record observation at which the recent (non-historical) data begins
  recent.begins <- length(hX) + 1

  # If in-control parameter estimates are not provided, use historical data
  if (is.null(ic.parms$alpha) | is.null(ic.parms$beta)) {
  
    ##################################################
    # Identify and remove out-of-control data points using
    # 2-tiered Chebyshev
    ##################################################

    if (!exp.cheby)
      cheby <- chebyshev.2step.od(hX, first.rejval=cheb.level.1,
                                  second.rejvals=cheb.level.2,
                                  one.sided=TRUE)$output["one.sided.upper.cv"]     
    else
      cheby <- log(chebyshev.2step.od(exp(hX), first.rejval=cheb.level.1,
                                      second.rejvals=cheb.level.2,
                                      one.sided=TRUE)$output["one.sided.upper.cv"])
  
    # Identify and remove out-of-control points from historical
    ooc.points <- hX > cheby
    hX.ic <- hX[!ooc.points] # ic for 'in-control'

    if (fit.hist.only)
      verbose <- TRUE
    
    # Show ooc points
    if (verbose) {
      if (any(ooc.points)) {
        cat("The two-tiered upper Chebyshev limit was", round(cheby,3), "\n")
        cat("The following points were designated as 'out-of-control' in the",
            "historical data:\n")
        print(hX[ooc.points])
      }
    }
  
    
    # Get method-of-moments estimates for alpha and beta parameters
    # to use as starting values in the ML-fit
    alpha.hat <- mean(hX.ic, na.rm=TRUE)^2 / var(hX.ic, na.rm=TRUE)
    beta.hat <- var(hX.ic, na.rm=TRUE) / mean(hX.ic, na.rm=TRUE)
  
  
    # Find the maximum likelihood estimates
    if (class(fit1 <- try(
        fitdistr(hX.ic, "gamma", list(shape=alpha.hat, scale=beta.hat), lower=0.0001)
        , silent=TRUE)) != "try-error") {
      fit <- fit1
      alpha.ic <- fit$estimate["shape"]
      beta.ic <- fit$estimate["scale"]
    }
    else if (class(fit2 <- try(fitdistr(hX.ic, "gamma"), silent=TRUE)) != "try-error") {
      fit <- fit2
      alpha.ic <- fit$estimate["shape"]
      beta.ic <- 1 / fit$estimate["rate"]
    }
    else
      stop("'fitdistr' failed twice in gammaCusum:\n", fit1, fit2)

    if (verbose) {
      cat("In-control parameter estimates and standard errors (below):\n")
      print(fit)

      if (dev.cur() == 1) 
        X11(height=12,width=12)
    
      # current device is windows
      if (names(dev.cur()) == "windows")
        par(ask=TRUE)

      if (!is.null(title))
        title.1 <- paste(title,"\nGamma parameter estimates:   alpha.0 = ",
                         round(alpha.ic,3), ",  beta.0 = ", round(beta.ic,3), sep="")
      else
        title.1 <- paste("Gamma parameter estimates:   alpha.0 = ", round(alpha.ic,3),
                         ",  beta.0 = ", round(beta.ic,3), sep="")
        
      my.histogram(hX, relative.hist=TRUE, xlab="All values of historic data",
                   ylab="Relative Freq", main=title.1, nclass=30,
                   xlim=range(hX, cheby), font.main=1, given.xlimits=TRUE)
      
      abline(v=cheby, lwd=2, col="Purple")

      x.pts <- seq(min(hX),max(hX,cheby),length=300)
      lines(x.pts, dgamma(x.pts, shape=alpha.ic, scale=beta.ic),
            col="Red", lwd=2)
  
      qqplotGamma(hX.ic, list(scale=alpha.ic, shape=beta.ic), title=title,
                  ylab="Sample quantiles of historic, in-control data")
  
      par(ask=FALSE)

    } # if verbose

  } # if (!is.null(ic.parms$alpha) | !is.null(ic.parms$beta)) {

  # If no historical  data
  else {
    alpha.ic <- ic.parms$alpha
    beta.ic <- ic.parms$beta
    recent.begins <- 1
  }

  if (!fit.hist.only) {
  
    # Calculate the reference value, k
  
    # eq (6.12) of page 143 of Hawkins and Olwell has a typo, missing a minus sign
    # eqation at the bottom of page 92 appears to be correct
    # for the form:  C_[i] = max(0 , C_[i-1] + X[i] - k)
    # k is represented below as a function of beta.ic, beta.shift, and alpha.ic
  
    k <- alpha.ic * beta.ic * beta.shift * log(beta.shift) / (beta.shift - 1)
    names(k) <- "reference value"
    
  
    # Find the control limit
    if (is.null(h)) {
      findCL <- findGammaCusumCL(arl.0, alpha.ic, beta.ic, k, ...)
      h <- findCL$h
      arl.0.a <- findCL$arl.0.a
    }
  
    # If the control limit is provided, calculate the achieved arl.0
    else 
      arl.0.a <- gammaCusumARL(alpha.ic, beta.ic, k, h)
  
    names(h) <- "control limit"
    names(arl.0.a) <- "achieved in-control ARL"
    
    # Select the data to process
    if (!process.historical)
      X <- X[recent.begins:length(X)]
  
    if (!length(X))
      stop("The value of 'historical' = ", historical, " left no recent data.\n",
           "Cusum cannot be calculated.\n")
    
    # Run the CUSUM over the data
    cusum <- calcCusum(X, k, h, reset=reset)
  
    # output
    out <- list(cusum=cusum$cusum, resetCounter=cusum$resetCounter, data=X,
                h=h, k=k, alpha.0=alpha.ic, beta.0=beta.ic, arl.0.a=arl.0.a)
  
  
    return(out)

  } # if (!fit.hist.only)

} # end gammaCUSUM
