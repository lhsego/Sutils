## Simulates the initial state ARL of the standard normal CUSUM
normalCusumSim <- function(k, h, mu=0, sigma=1, twoSided=TRUE, reps=10^4) {

  # Preliminary checks
  if (!is.numeric(k))
    stop("'k' must be a real number\n")
  if (!is.numeric(h))
    stop("'h' must be a real number\n")
  if (!is.numeric(reps))
    stop("'reps' must be a positive integer\n")
  if (reps <= 0)
    stop("'reps' must be a positive integer\n")

  if (!is.numeric(mu))
    stop("'mu' must be a positive real number\n")
  if (!is.numeric(sigma))
    stop("'sigma' must be a positive real number\n")
  if (sigma <= 0)
    stop("'sigma' must be a positive real number\n")      

  if (twoSided)
    RL <- .C("normal2CusumSim",
             as.integer(round(reps)),
             as.double(k),
             as.double(h),
             as.double(mu),
             as.double(sigma),
             arl=integer(round(reps)),
             PACKAGE="monitoring")$arl
  else
    RL <- .C("normalCusumSim",
             as.integer(round(reps)),
             as.double(k),
             as.double(h),
             as.double(mu),
             as.double(sigma),
             arl=integer(round(reps)),
             PACKAGE="monitoring")$arl

  return(list(arl = mean(RL),
              se.arl = sqrt(var(RL) / length(RL)),
              n = length(RL)))

} # normalCusumSim
