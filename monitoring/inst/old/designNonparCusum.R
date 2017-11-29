################################################################################
#
# Filename:             designNonparCusum.R
# Creator:              Landon Sego
# Date Created:         2007-07-05
# Description/Purpose:  Design a non-parametric CUSUM chart using historical data
#                       The chart can detect and shift in scale and/or location
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

designNonparCusum <- function(X, arl.0,
                              scale.shift.design=1,
                              location.shift.design=0,
                              filter.data=TRUE,
                              cheb.level.1=0.01,
                              cheb.level.2=0.005,
                              one.sided=NULL,
                              unimodal=TRUE,
                              verbose=FALSE,
                              ...) {

  # X = vector of historical data
  # arl.0 = Target arl.0
  # scale.shift.design = shift in the scale
  # location.shift.design = shift in the location
  # ... arguments to logsplines.  In particular, lbound and ubound.  This
  #   is especially important when designing a distribution with a scale shift
  
  # Basic checks
  # Verify X is valid
  if (!is.numeric(X))
    stop(deparse(substitute(X)), " must be a numeric vector.\n")

  # Verify that shift parameters are valid
  if (!is.numeric(scale.shift.design))
    stop("'scale.shift.design' must be a positive real number > 0.\n")
  if (scale.shift.design <= 0)
    stop("'scale.shift.design' must be a positive real number.\n")
  if (!is.numeric(location.shift.design))
    stop("'location.shift.design' must be a real number.\n")

  if ((scale.shift.design==1) & (location.shift.design==0))
    stop("One of 'scale.shift.design = 1' and 'location.shift.design = 0' needs to be changed ",
         "so that the CUSUM will be designed to detect some type of shift.\n")

  # Check cheb levels
  if (!is.numeric(cheb.level.1))
    stop("'cheb.level.1' must be a real number in (0, 1/6).\n")
  if ((cheb.level.1 <= 0) | (cheb.level.1 > 1/6))
    stop("'cheb.level.1' must be a real number in (0, 1/6).\n")
  if (!is.numeric(cheb.level.2))
    stop("'cheb.level.2' must be a real number in (0, 1/6).\n")
  if ((cheb.level.2 <= 0) | (cheb.level.2 > 1/6))
    stop("'cheb.level.2' must be a real number in (0, 1/6).\n")  

  # Check arl.0.  Must be > 0
  if (!is.numeric(arl.0))
    stop("'arl.0' must be a vector of positive real numbers.\n")
  if (any(arl.0 <= 0))
    stop("All values of 'arl.0' must be positive real numbers.\n")
  
  # Filter data to remove OOC data points
  if (filter.data) {

    # Calculate 2-tiered Chebyshev limits
    limits <- cheb.2step.od(X, unimodal=unimodal,
                            first.rejval=cheb.level.1,
                            second.rejvals=cheb.level.2,
                            one.sided=one.sided)$output
    
    # If the limits were one-sided upper
    if (any(colnames(limits) %in% "one.sided.upper.cv")) 
      in.control <- X < limits[1,"one.sided.upper.cv"]
    
    # If the limits were one-sided lower      
    else if (any(colnames(limits) %in% "one.sided.lower.cv")) 
      in.control <- X > limits[1,"one.sided.lower.cv"]
    
    # If the limits were two-sided
    else
      in.control <- (X > limits[1,"lower.cv"]) & (X < limits[1,"upper.cv"])
    
    # Remove out of control points
    X.ic <- X[in.control]
    
    # Report values that were trimmed
    if (verbose) {
      if (any(!in.control)) {
        cat("The two-tiered Chebyshev limits were\n")
        print(limits)
        cat("The following points were designated as 'out-of-control' in the",
            "historical data:\n")
        print(X[!in.control])
      }        
    }

  } # if (filter.data)
  else
    X.ic <- X
  
  # Obtain the in-control fit of the logspline
  fit0 <- logspline(X.ic, ...)
#  fit0 <- logspline(X.ic, ...)#nknots=5, maxknots=6, ...)  


  fit.0 <<-fit0
  
  # Plot of the densities and the CUSUM weights
  if (verbose) {

    op <- par(mfrow=c(2,1))

    x.seq <- seq(min(X.ic),max(X.ic),length=500)

    y.0 <- dlogspline(x.seq, fit0)
    y.1 <- dlogspline((x.seq-location.shift.design)/scale.shift.design,
                      fit0) / scale.shift.design

    hist(X.ic, nclass=30, probability=TRUE,
         main="Historical In-control data", xlab="",
         font.main=1, ylim=c(0,max(y.0,y.1)))
    
    lines(x.seq, y.0, col="Blue")
    lines(x.seq, y.1, col="Red")
    legend(min(X.ic) + 0.5*diff(range(X.ic)), max(y.0,y.1),
           c("In-control","Out-of-control"), lty=1, lwd=2,
           col=c("Blue", "Red"), cex=0.9)

    cusumS <- function(X) {
      return(log(dlogspline((X-location.shift.design)/scale.shift.design, fit0))
             - log(scale.shift.design)
             - log(dlogspline(X, fit0)))
    }

    plot(x.seq, cusumS(x.seq), type="l", font.main=1, main="Cusum Scoring Function",
         xlab="Data value", ylab="Cusum Score")
    abline(h=0, col="Blue")

    par(op)

  }

  # Find control limit

  # Return design
  
  

} #
