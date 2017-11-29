normalCusumARL <- function(k, h, mu=0, sigma=1, twoSided=TRUE, numPartitions=500) {

  # Something is wrong with transMatrixNormal, I think.  Note how results don't agree with
  # pg 415 of Montgomery (4th ed) or simulations when sigma > 1
  
  # Basic checks
  if (!is.numeric(k))
    stop("'k' must be a real number\n")
  if (!is.numeric(h))
    stop("'h' must be a real number\n")
  if (!is.numeric(numPartitions))
    stop("'numPartitions' must be a positive integer\n")
  if (numPartitions <= 0)
    stop("'numPartitions' must be a positive integer\n")

    
  if (!is.numeric(mu))
    stop("'mu' must be a positive real number\n")
  if (!is.numeric(sigma))
    stop("'sigma' must be a positive real number\n")
  if (sigma <= 0)
    stop("'sigma' must be a positive real number\n")    

  # Make sure numPartitions is a whole number
  numPartitions <- as.integer(round(numPartitions))

  # Calculate the transition matrix
  tMatrix <- matrix(.C("transMatrixNormal",
                       as.integer(numPartitions),
                       as.double(h),
                       as.double(k),
                       as.double(mu),
                       as.double(sigma),
                       Q = double((numPartitions + 1)^2),
                       PACKAGE = "cusum")$Q,
                    nrow = numPartitions + 1)

  # Checks
  # After running this a bunch of times, I haven't had this produce a warning
  # yet--so I'll comment it out for efficiency
  if (any(apply(tMatrix, 1, sum) > 1 + 1e-12))
    warning("One or more of the rows in the transition matrix summed to > 1.\n")

  arl.vec <- solve(diag(numPartitions + 1) - tMatrix) %*% rep(1, numPartitions + 1)

  if (twoSided)
    arl <- arl.vec[1] / 2
  else
    arl <- arl.vec[1]

  return(arl)

} # normalCusumARL


