# A Wrapper for the c method 'cusum'
# Calculates the CUSUM for a vector of data
# CUSUM of the form: C[i] = max(0, C[i-1] + X[i] - k)
# Signal occurs when C[i] > h

calcCusum <- function(X, k, h, initial=0, type="upper", reset=TRUE) {

  # Verify that method is loaded
  
  # Verify inputs
  if (!is.numeric(X))
    stop(deparse(substitute(X)), " must be a numeric vector.\n")
  if (!is.numeric(k))
    stop("'k' must be a real number\n")
  if (!is.numeric(h))
    stop("'h' must be a real number\n")
  if (!is.numeric(initial))
    stop("'initial' must be a real number\n")    
  if (!(tolower(type) %in% c("lower","upper")))
    stop("'type' must be one of 'upper' or 'lower'.\n")
  if ((type=="upper") & (initial > h))
    stop("Cusum initialized above the control limit.\n")

  if (type=="upper")
    upper <- TRUE
  else
    upper <- FALSE

  if (!upper)
    stop("Lower cusum not yet implemented\n")

  cusum <- .C("calcCusum",
              as.double(X),
              as.double(k),
              as.double(h),
              as.double(initial),
              as.integer(reset),
              as.integer(upper),
              as.integer(length(X)),
              cusum=double(length(X)),
              stagger=double(length(X)),
              resetCounter=integer(length(X)),
              PACKAGE="monitoring")[c(1,3,8:10)]

  names(cusum)[1:2] <- c("data","h")
  
  if (!is.null(names(X)))
    names(cusum$cusum) <- names(cusum$resetCounter) <- names(cusum$data) <- names(X)

  class(cusum) <- "cusum"

  # Return the list
  cusum

} # calcCusum
