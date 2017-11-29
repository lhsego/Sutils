################################################################################
#
# Filename:             designGammaCusum.R
# Creator:              Landon Sego
# Date Created:         2007-06-18
# Description/Purpose:  Design the gamma based CUSUM chart using historical data
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

designGammaCusum <- function(X, beta.shift, arl.0,
                             filter.data=TRUE,
                             nonpar.fit=FALSE,
                             cheb.level.1=0.01,
                             cheb.level.2=0.01,
                             unimodal=TRUE,
                             exp.cheby=FALSE,
                             ic.parms=list(alpha=NULL, beta=NULL),
                             verbose=FALSE,
                             title=NULL,
                             ...) {

  ##################################################
  # Check arguments
  ##################################################
  
  # Verify X is valid
  if (!is.numeric(X))
    stop(deparse(substitute(X)), " must be a numeric vector.\n")

  # Verify that beta is valid
  if (!is.numeric(beta.shift))
    stop("'beta.shift' must be a positive real number.\n")
  if (beta.shift <= 0)
    stop("'beta.shift' must be a positive real number.\n")
  

  # Check cheb levels
  if (!is.numeric(cheb.level.1))
    stop("'cheb.level.1' must be a real number in (0, 1/6).\n")
  if ((cheb.level.1 <= 0) | (cheb.level.1 > 1/6))
    stop("'cheb.level.1' must be a real number in (0, 1/6).\n")
  if (!is.numeric(cheb.level.2))
    stop("'cheb.level.2' must be a real number in (0, 1/6).\n")
  if ((cheb.level.2 <= 0) | (cheb.level.2 > 1/6))
    stop("'cheb.level.2' must be a real number in (0, 1/6).\n")  

  # Check arl.b0.  Must be > 0
  if (!is.numeric(arl.0))
    stop("'arl.0' must be a vector of positive real numbers.\n")
  if (any(arl.0 <= 0))
    stop("All values of 'arl.0' must be positive real numbers.\n")

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

  # If in-control parameter estimates are not provided, use historical data
  # to estimate the in-control gamma parameters
  if (is.null(ic.parms$alpha) | is.null(ic.parms$beta)) {


    if (filter.data) {
      
      ##################################################
      # Identify and remove out-of-control data points using
      # 2-tiered Chebyshev
      ##################################################
  
      if (!exp.cheby)
        cheby <- cheb.2step.od(X, unimodal=unimodal,
                               first.rejval=cheb.level.1,
                               second.rejvals=cheb.level.2,
                               one.sided="upper")$output[1,"one.sided.upper.cv"]     
      else
        cheby <- log(cheb.2step.od(exp(X), unimodal=unimodal,
                                   first.rejval=cheb.level.1,
                                   second.rejvals=cheb.level.2,
                                   one.sided="upper")$output[1,"one.sided.upper.cv"])
    
      # Identify and remove out-of-control points from historical
      ooc.points <- X > cheby
      X.ic <- X[!ooc.points] # ic for 'in-control'
  
      # Show ooc points
      if (verbose) {
        if (any(ooc.points)) {
          cat("The two-tiered upper Chebyshev limit was", round(cheby,3), "\n")
          cat("The following points were designated as 'out-of-control' in the",
              "historical data:\n")
          print(X[ooc.points])
        }
      }

    } # if (filter.data)

    else {
      X.ic <- X
      cheby <- NULL
    }
  

    # Get method-of-moments estimates for alpha and beta parameters
    # to use as starting values in the ML-fit
    alpha.hat <- mean(X.ic, na.rm=TRUE)^2 / var(X.ic, na.rm=TRUE)
    beta.hat <- var(X.ic, na.rm=TRUE) / mean(X.ic, na.rm=TRUE)
  
  
    # Find the maximum likelihood estimates
    if (class(fit1 <- try(
        fitdistr(X.ic, "gamma", list(shape=alpha.hat, scale=beta.hat), lower=0.0001)
        , silent=TRUE)) != "try-error") {
      fit <- fit1
      alpha.ic <- fit$estimate["shape"]
      beta.ic <- fit$estimate["scale"]
    }
    else if (class(fit2 <- try(fitdistr(X.ic, "gamma"), silent=TRUE)) != "try-error") {
      fit <- fit2
      alpha.ic <- fit$estimate["shape"]
      beta.ic <- 1 / fit$estimate["rate"]
    }
    else {
      warning("'fitdistr' failed twice in gammaCusum:\n", fit1, fit2,
              "\n Will use the moment estimators of alpha and beta.\n")
      alpha.ic <- alpha.hat
      beta.ic <- beta.hat
      fit <- list(estimate=c(shape=alpha.ic,scale=beta.ic), type="Moment estimators")
    }

    if (verbose) {
      cat("In-control parameter estimates and standard errors (below):\n")
      print(fit)

      # If no graphical devices are active, open the graphical window
      if (dev.cur() == 1) 
        X11(height=12,width=12)
    
      # if current device is windows
      if (names(dev.cur()) == "windows")
        par(ask=TRUE)

      if (!is.null(title))
        title.1 <- paste(title,"\nGamma parameter estimates:   alpha.0 = ",
                         round(alpha.ic,3), ",  beta.0 = ", round(beta.ic,3), sep="")
      else
        title.1 <- paste("Gamma parameter estimates:   alpha.0 = ", round(alpha.ic,3),
                         ",  beta.0 = ", round(beta.ic,3), sep="")

      # my.histogram doesn't actually give the density...so, drawing the density
      # on top of this graph isn't correct...
#      my.histogram(X, relative.hist=TRUE, xlab="All values of historic data",
#                   ylab="Relative Freq", main=title.1, nclass=30,
#                   xlim=range(X, cheby), font.main=1, given.xlimits=TRUE)

      hist(X, probability=TRUE, xlab="All values of historic data",
           ylab="Relative Freq", main=title.1, nclass=30,
           xlim=range(X, cheby), font.main=1)

      if (!is.null(cheby))
        abline(v=cheby, lwd=2, col="Purple")

      x.pts <- seq(min(X),max(X,cheby),length=300)
      lines(x.pts, dgamma(x.pts, shape=alpha.ic, scale=beta.ic),
            col="Red", lwd=2)
  
      qqplotGamma(X.ic, list(shape=alpha.ic, scale=beta.ic), title=title,
                  ylab="Sample quantiles of historic, in-control data")
  
      par(ask=FALSE)

    } # if verbose

  } # if (!is.null(ic.parms$alpha) | !is.null(ic.parms$beta)) {

  # If parameter values are provided
  else {
    alpha.ic <- ic.parms$alpha
    beta.ic <- ic.parms$beta
    names(alpha.ic) <- "shape"
    names(beta.ic) <- "scale"
  }

  # Calculate the reference value, k
  
  # eq (6.12) of page 143 of Hawkins and Olwell has a typo, missing a minus sign
  # eqation at the bottom of page 92 appears to be correct
  # for the form:  C_[i] = max(0 , C_[i-1] + X[i] - k)
  # k is represented below as a function of beta.ic, beta.shift, and alpha.ic
  
  k <- alpha.ic * beta.ic * beta.shift * log(beta.shift) / (beta.shift - 1)

  if (nonpar.fit)
    fit <- logspline(X.ic, lbound=0, nknots=3, maxknots=5)
  else
    fit <- list(shape=alpha.ic, scale=beta.ic)

  
  # Find the control limit
  h <- arl.0.a <- NULL

  for (target in arl.0) {
    
    if (class(try(
        findCL <- findGammaCusumCL(target, fit, k, verbose=verbose, ...)
        )) == "try-error")
      stop("findGammaCusumCL() failed in designGammaCusum()\n")
  
    h <- c(h, findCL$h)
    arl.0.a <- c(arl.0.a, findCL$arl.0.a)

  }

  # output
  out <- list(h=h, k=k, alpha.0=alpha.ic, beta.0=beta.ic, arl.0.a=arl.0.a)

  # Set the inner names of the list to NULL
  for (i in names(out)) 
    names(out[[i]]) <- NULL

  out$fit <- fit
  
  out

} # end designGammaCUSUM
