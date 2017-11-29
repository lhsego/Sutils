## Simulates the initial state ARL of the GAMMA cusum
gammaCusumSim <- function(fit, k, h, reps=10^4) {

  # Preliminary checks
  if (!is.numeric(k))
    stop("'k' must be a real number\n")
  if (!is.numeric(h))
    stop("'h' must be a real number\n")
  if (!is.numeric(reps))
    stop("'reps' must be a positive integer\n")
  if (reps <= 0)
    stop("'reps' must be a positive integer\n")    

  # if fit is a log spline, use that for the simulation
  if (class(fit) == "logspline") {

    # Length of simulated runs
    segment.length <- 10^6
    
    # Initialize for the loop
    counter <- last.cusum.value <- prev.ending.reset.length <- 0
    RL <- hazard <- prev.lambda <- NULL
  
    # Simulate the CUSUM
    while ((length(RL) < reps) & (counter < 10^5)) {
  
#      pvar(counter) 
      
      # Obtain observations
      X <- rlogspline(segment.length, fit)
  
#      pvar(scores, digits=3) 
  
      # Calculate the CUSUM 
      cCusum <- calcCusum(X, k, h, initial=last.cusum.value)
  
#      plot(cCusum)
#      pvar(cCusum$cusum, digits=2) 
#      pvar(cCusum$resetCounter)
#      pvar(cCusum$stagger, digits=2)

      # Calculate the beginnings of the total hazard, add in the previous ending
      lambda <- c(prev.lambda, 1 - plogspline(k + h - cCusum$stagger, fit))

      # Join the previous ending reset to the current one
      resets <- c(rep(1,prev.ending.reset.length), cCusum$resetCounter)
  
#      pvar(length(resets),length(lambda))
#      pvar(lambda, digits=2)
  
      # Calculate the run lengths of the various sequences
      run.lengths <- table(resets)
      hazard.seq <- tapply(lambda, as.factor(resets), sum)
      
#      pvar(run.lengths)
#      pvar(hazard.seq, digits=2)
  
      # Prepare the unfinished sequence for inclusion in the next iteration
      last.cusum.value <- cCusum$cusum[segment.length]
  
      # If the last observation is not a signal
      if (last.cusum.value <= h)  {
        prev.ending.reset.length <- run.lengths[length(run.lengths)]
        prev.lambda <- lambda[resets==cCusum$resetCounter[segment.length]]
        RL <- c(RL, run.lengths[-length(run.lengths)])
        hazard <- c(hazard, hazard.seq[-length(run.lengths)])
      }
      # Else the last obervation was a signal
      else {
        last.cusum.value <- prev.ending.reset.length <- 0
        prev.lambda <- NULL
        RL <- c(RL, run.lengths)
        hazard <- c(hazard, hazard.seq)
      }
  
#      pvar(last.cusum.value,prev.ending.reset.length)
#      pvar(prev.lambda, digits=2)
#      pvar(RL)
#      pvar(hazard, digits=2)
      
      counter <- counter + 1

    } # while

    # Sanity check
    if (length(RL) != length(hazard))
      stop("length(RL) != length(hazard), this should not happen.\n")

    pvar(cor(RL, hazard))

    return(list(
      arl = mean(RL),
      arl.rv = mean(RL) - cov(RL, hazard) * (mean(hazard) - 1) / var(hazard),
      se.arl = sqrt(var(RL) / length(RL)),
      se.arl.rv = sqrt(var(RL) * (1 - (cor(RL, hazard))^2) / length(RL)),
      n = length(RL)))
    
    
  } #  if (class(fit) == "logspline") {

  # Use Gamma distribution for simulation
  else {

    if (!all(sort(names(fit)) == c("scale","shape")))
      stop("If 'fit' is not of class 'logspline', then 'fit' ",
           "must have names 'shape' and 'scale'\n")
    
    shape <- fit$shape
    scale <- fit$scale

    
    if (!is.numeric(shape))
      stop("'shape' must be a positive real number\n")
    if (shape <= 0)
      stop("'shape' must be a positive real number\n")    
    if (!is.numeric(scale))
      stop("'scale' must be a positive real number\n")
    if (scale <= 0)
      stop("'scale' must be a positive real number\n")    
  
    RL <- .C("gammaCusumSim",
             as.integer(round(reps)),
             as.double(shape),
             as.double(scale),
             as.double(k),
             as.double(h),
             as.double(0),
             arl=integer(round(reps)),
             PACKAGE="monitoring")$arl


    return(list(
      arl = mean(RL),
      arl.rv = NULL,
      se.arl = sqrt(var(RL) / length(RL)),
      se.arl.rv = NULL,
      n = length(RL)))

    
  } # else use Gamma Distribution Simulation

  # Calculate the usual and the variance reduced estimates of the ARL
  
} # gammaCusumSim
