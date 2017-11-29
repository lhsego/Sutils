findGammaCusumCL <- function(target, fit, k,
                             upper.lim=100, lower.lim=2,
                             firstPartition=100, secondPartition=500,
                             check=TRUE, verbose=FALSE) {

  # Check arguments
  if (!is.numeric(target))
    stop("'target' must be a positive real number\n")
  if (target <= 0)
    stop("'target' must be a positive real number\n")
  if (!is.numeric(k))
    stop("'k' must be a real number\n")
  if (class(fit) != "logspline") {
    if (!all(sort(names(fit)) == c("scale","shape")))
      stop("If 'fit' is not of class 'logspline', then 'fit' ",
           "must have names 'shape' and 'scale'.\n")
  }
  if (!is.numeric(upper.lim))
    stop("'upper.lim' must be a positive real number\n")
  if (upper.lim <= 0)
    stop("'upper.lim' must be a positive real number\n")
  if (!is.numeric(lower.lim))
    stop("'lower.lim' must be a positive real number\n")
  if (lower.lim <= 0)
    stop("'lower.lim' must be a positive real number\n")       
  if (upper.lim <= lower.lim)
    stop("'upper.lim' must be greater than 'lower.lim'\n")
  if (!is.numeric(firstPartition))
    stop("'firstPartition' must be a positive real number\n")
  if (firstPartition <= 0)
    stop("'firstPartition' must be a positive real number\n")       
  if (!is.numeric(secondPartition))
    stop("'secondPartition' must be a positive real number\n")
  if (secondPartition <= 0)
    stop("'secondPartition' must be a positive real number\n")       

  
  # define the function that we use to search for a root
  f <- function(h, ...) 
    log(gammaCusumARL(fit, k, h, ...)) - log(target)

  first.fail <- TRUE
  iter <- 0
  double.firstPartition <- FALSE

  # Make a rough 'first' search for h
  while (first.fail) {

    if ((iter > 100) & (!double.firstPartition)) {
      cat("findGammaCusumCL:  First 100 attempts to solve for 'h' failed. Last error:\n", first)
      cat("Will attempt the search by doubling 'firstPartition'\n")
      firstPartition <- min(2*firstPartition, secondPartition)
      double.firstPartition <- TRUE
    }

    if (iter > 300) 
      stop("findGammaCusumCL:  First 300 attempts to solve for 'h' failed. Last error:\n", first)
    
    # Protect against the limits being too low
    first <- try(uniroot(f, lower=lower.lim, upper=upper.lim, tol=5,
                         numPartitions=firstPartition)$root,
                 silent=TRUE)

    if (class(first) == "try-error") {

      # If it failed because the limits were too narrow
      if (length(grep("values at end points not of opposite sign", first))) {
        lower.lim <- max(0.5, 0.75 * lower.lim)
        upper.lim <- 1.25 * upper.lim
        if (verbose) {
          cat("Range of control limits too narrow\n")
          pvar(lower.lim, upper.lim, digits=2)
        }
      }

      # If it failed because the limits were too wide
      else if (length(grep("system is computationally singular", first))) {
        lower.lim <- 1.3 * lower.lim
        upper.lim <- max(1.2 * lower.lim, 0.7 * upper.lim)
        if (verbose) {
          cat("Range of control limits was too wide\n")
          pvar(lower.lim, upper.lim, digits=2)
        }
      }

      else
        stop(first)

    }
    # If it didn't fail, the loop will end
    else
      first.fail <- FALSE

    iter <- iter + 1

  }

  if (verbose)
    cat(iter, "iterations were used in the first search for h.\n")

  # Protect the second call
  second <- try(uniroot(f, lower=max(lower.lim, 0.85*first),
                        upper=min(upper.lim, 1.15*first),
                        tol=0.1, numPartitions=secondPartition), silent=TRUE)

  # Second attempt of second call
  if (class(second) == "try-error") {
    if (length(grep("values at end points not of opposite sign", second))) {
       second <- uniroot(f, lower=max(lower.lim, 0.3*first),
                         upper=min(upper.lim, 1.7*first),
                         tol=0.1, numPartitions=secondPartition)
    }
    else
      stop(second)
  }

  arl0.a <- exp(second$f.root + log(target))
  h <- second$root

  # Check the result using simulation
  if (check) {

    if (class(fit) == "logspline")
      reps <- 2000
    else
      reps <- 10^4
    
    arl0.a.sim <- gammaCusumSim(fit, k, h, reps=reps)

#    cat("\narl.0.a.sim output from gammaCusumSim()\n")
#    print(arl0.a.sim)

    # Use the reduced variation estimates if possible
    if (!is.null(arl0.a.sim$arl.rv)) {
      sim.arl <- arl0.a.sim$arl.rv
      sim.arl.se <- arl0.a.sim$se.arl.rv
    }
    else {
      sim.arl <- arl0.a.sim$arl
      sim.arl.se <- arl0.a.sim$se.arl
    }
    # Z score for comparing simulated ARL to Markov Chain ARL
    z <- (sim.arl - arl0.a) / sim.arl.se

    # 2.33 is around the 99th percentile
    if (abs(z) > 3) {
      p.value <- 2 * pnorm(abs(z), lower.tail=FALSE)
      cat("Note in 'findGammaCusumCL':\n",
          "Simulated and Markov Chain estimates of ARL0 differ significantly\n")
      pvar(arl0.a, sim.arl, sim.arl.se, z, p.value, digits=3)
    }
  } # if (check)
  else
    arl0.a.sim <- NULL
  
  return(list(h=h, arl.0.a=arl0.a, arl.0.a.sim=arl0.a.sim))
  
} # findGammaCusumCL
