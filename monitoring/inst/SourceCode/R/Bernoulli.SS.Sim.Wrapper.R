# Simulate the SS ARL of the CUSUM using Bernoulli or Geometric RV's
Bernoulli.SS.Sim.Wrapper <- function(reps, p0, gamma, m, h, shift.frac=1, verbose=FALSE){

  # Checks
  if ((reps%%1) | (reps < 1)) stop("\nreps = ", reps, " is not valid.\n")
  if ((m < 1) | m%%1) stop("\nm must be an integer >=1\n")

  limit <-  2^31 - 1
  
  if ((reps > limit) | (m > limit) | (round(m*h) > limit))
    stop("reps = ",reps,"  m = ",m,"  or round(m*h) = ",
         round(m*h)," is larger than long integer limit of 2^31-1.\n")

  if (gamma < 1) stop("\ngamma = ", gamma, " is < 1.\n")
  p1 <- p0*gamma
  if (p1 > 1) stop("\np =",p," which is > 1.\n")

  # Find arl.b0
  # Will introduce the shift immediately after 1.5*arl.b0  
  shiftTime <- ceiling(shift.frac * linear.arl(m, h, p0))

  if (verbose) cat("\nshiftTime =", shiftTime, "\n\n")

  sim <- .C("BernoulliSSCusumSim",
            as.integer(reps),
            as.double(p0),
            as.double(p1),
            as.integer(m),
            as.integer(round(m*h)),
            as.integer(shiftTime),
            as.integer(verbose),
            ssrl=integer(reps),
            nFalse=as.integer(0),
            success=as.integer(1))

  if (!sim$success) stop("C method 'BernoulliSSCusumSim' failed.\n")
  
  return(list(ss.arl.b=mean(sim$ssrl), ss.sdrl.b=sd(sim$ssrl), n=reps, nFalse=sim$nFalse))
  
} # end ssCusumWrapper
