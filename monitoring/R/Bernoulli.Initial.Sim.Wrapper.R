# Simulate the initial state ARL of the CUSUM using Bernoulli or Geometric RV's
Bernoulli.Initial.Sim.Wrapper <- function(reps, p0, gamma, m, h, initial.state=1, verbose=FALSE){

  # Initial state can range from 1, 2, ..., m*h - 1, m*h
  # So m*h is the largest state the chart can take without signaling
  
  # Checks
  if ((reps%%1) | (reps < 1))
    stop("\nreps = ", reps, " is not valid.\n")
  if ((m < 1) | m%%1)
    stop("\nm must be an integer >=1\n")
  if ((initial.state%%1) | (initial.state < 1) | (initial.state > (m*h)))
      stop("\n initial.state = ", initial.state, " is not valid.\n")

  limit <-  2^31 - 1
  
  if ((reps > limit) | (m > limit) | (round(m*h) > limit))
    stop("reps = ",reps,"  m = ",m,"  or round(m*h) = ",
         round(m*h)," is larger than long integer limit of 2^31-1.\n")

  if (gamma <= 0)
    stop("\ngamma = ", gamma, " is <= 0.\n")

  p1 <- p0 * gamma
      
  if (p1 >= 1)
    stop("\np1 =", p1, " which is => 1.\n")

  sim <- .C("BernoulliInitialCusumSim",
            as.integer(reps),
            as.double(p1),
            as.integer(m),
            as.integer(round(m*h)),
            as.integer(initial.state),
            as.integer(verbose),
            arl=integer(reps),
            success=as.integer(1))

  if (!sim$success)
    stop("C Method 'BernoulliInitialCusumSim' failed.\n")

  return(list(arl.b=mean(sim$arl), sdrl.b=sd(sim$arl), n=reps))
  
} # end ssCusumWrapper

