gammaCusumARL <- function(fit, k, h, numPartitions=500) {

  # Basic checks
  if (!is.numeric(k))
    stop("'k' must be a real number\n")
  if (!is.numeric(h))
    stop("'h' must be a real number\n")
  if (!is.numeric(numPartitions))
    stop("'numPartitions' must be a positive integer\n")
  if (numPartitions <= 0)
    stop("'numPartitions' must be a positive integer\n")

  # if fit is a logspline object
  if (class(fit) == "logspline")
     tMatrix <- nonparTransMatrix(fit, h, k, numPartitions=numPartitions)
  
  else {  

    if (!all(sort(names(fit)) == c("scale","shape")))
      stop("If 'fit' is not of class 'logspline', then 'fit' ",
           "must have names 'shape' and 'scale'\n")
    
    shape <- fit$shape
    scale <- fit$scale
    
  
    # Verify that method is loaded
    
    if (!is.numeric(shape))
      stop("'shape' must be a positive real number\n")
    if (shape <= 0)
      stop("'shape' must be a positive real number\n")    
    if (!is.numeric(scale))
      stop("'scale' must be a positive real number\n")
    if (scale <= 0)
      stop("'scale' must be a positive real number\n")    
  
    # Make sure numPartitions is a whole number
    numPartitions <- round(numPartitions)
  
    # Calculate the transition matrix
    tMatrix <- matrix(.C("transMatrixGamma",
                         as.integer(numPartitions),
                         as.double(shape),
                         as.double(scale),
                         as.double(h),
                         as.double(k),
                         Q=double((numPartitions+1)^2),
                         PACKAGE="monitoring")$Q,
                      nrow=numPartitions + 1)
  
    # Checks
    # After running this a bunch of times, I haven't had this produce a warning
    # yet--so I'll comment it out for efficiency
  #  if (any(apply(tMatrix, 1, sum) > 1 + 1e-12))
  #    warning("One or more of the rows in the transition matrix summed to > 1.\n")
  
  } # else
  
  arl.vec <- solve(diag(numPartitions + 1) - tMatrix) %*% rep(1, numPartitions + 1)

  return(arl.vec[1])

} # gammaCusumARL


