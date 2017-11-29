# Decided that this doesn't work because the CUSUM scoring function is badly behaved
# for the logspline density fits.

# Simulates the ARL of the Non parametric CUSUM
nonparCusumARL <- function(fit0, h,
                           location.shift.design = 0,
                           scale.shift.design = 1,
                           location.shift = 0,
                           scale.shift = 1,
                           reps=10^5,
                           segment.length=10^6) {

  # Preliminary checks
  if (class(fit0) != "logspline")
    stop(deparse(substitute(fit0)), " must be the object returned by 'logspline()'\n")
  if (!is.numeric(h))
    stop("h must be a positive real number\n")
  if (h <= 0)
    stop("h must be a positive real number\n")
  if (!is.numeric(scale.shift.design))
    stop("'scale.shift.design' must be a positive real number > 0.\n")
  if (scale.shift.design <= 0)
    stop("'scale.shift.design' must be a positive real number.\n")
  if (!is.numeric(location.shift.design))
    stop("'location.shift.design' must be a real number.\n")
  if ((scale.shift.design==1) & (location.shift.design==0))
    stop("One of 'scale.shift.design = 1' and 'location.shift.design = 0' needs to be changed ",
         "so that the CUSUM will be designed to detect some type of shift.\n")  
  if (!is.numeric(scale.shift))
    stop("'scale.shift' must be a positive real number > 0.\n")
  if (scale.shift <= 0)
    stop("'scale.shift' must be a positive real number.\n")
  if (!is.numeric(location.shift))
    stop("'location.shift' must be a real number.\n")

  # The non-parametric CUSUM score function
  cusumS <- function(X) {
    return(log(dlogspline((X-location.shift.design)/scale.shift.design, fit0))
           - log(scale.shift.design)
           - log(dlogspline(X, fit0)))
  }

  # Initialize for the loop
  counter <- last.cusum.value <- prev.ending.reset.length <- 0
  RL <- NULL

  # Simulate the CUSUM
  while ((length(RL) < reps) & (counter < 10^5)) {

#    pvar(counter) 
    
    # Obtain a million observations and calculate their CUSUM scores
    scores <- cusumS(location.shift + scale.shift * rlogspline(segment.length, fit0))

#    pvar(scores, digits=3) 

    # Calculate the CUSUM 
    cCusum <- calcCusum(scores, 0, h, initial=last.cusum.value)

#    plot(cCusum)
#    pvar(cCusum$cusum, digits=3) 
#    pvar(cCusum$resetCounter) 

    # Join the previous ending reset to the current one
    resets <- c(rep(1,prev.ending.reset.length), cCusum$resetCounter)

#    pvar(length(resets)) 

    # Calculate the run lengths of the various sequences
    run.lengths <- table(resets)

#    pvar(run.lengths) 

    # Prepare the unfinished sequence for inclusion in the next iteration
    last.cusum.value <- cCusum$cusum[segment.length]

    # If the last observation is not a signal
    if (last.cusum.value <= h)  {
      prev.ending.reset.length <- run.lengths[length(run.lengths)]
      RL <- c(RL, run.lengths[-length(run.lengths)])      
    }
    # Else the last obervation was a signal
    else {
      last.cusum.value <- prev.ending.reset.length <- 0
      RL <- c(RL, run.lengths)      
    }

#    pvar(last.cusum.value,prev.ending.reset.length) 
#    pvar(RL) 
    
    counter <- counter + 1

  } # while

  # Return a list summarizing the ARL
  list(arl=mean(RL), sdrl=sd(RL), n=length(RL))
  
} # nonparCusumARL
