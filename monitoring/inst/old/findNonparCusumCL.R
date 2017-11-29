findNonparCusumCL <- function(target, fit0,
                              location.shift.design = 0,
                              scale.shift.design = 1,
                              location.shift = 0,
                              scale.shift = 1,                              
                              firstPassRep=100,
                              secondPassRep=10^4) {


  # Check arguments
  if (!is.numeric(target))
    stop("'target' must be a positive real number\n")
  if (target <= 0)
    stop("'target' must be a positive real number\n")
  if (class(fit0) != "logspline")
    stop(deparse(substitute(fit0)), " must be the object returned by 'logspline()'\n")    
  if (!is.numeric(lower.lim.start))
    stop("'lower.lim.start' must be a positive real number\n")
  if (lower.lim.start <= 0)
    stop("'lower.lim.start' must be a positive real number\n")
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
  if (!is.numeric(firstPassRep))
    stop("'firstPassRep' must be a positive real number\n")
  if (firstPassRep <= 0)
    stop("'firstPassRep' must be a positive real number\n")       
  if (!is.numeric(secondPassRep))
    stop("'secondPassRep' must be a positive real number\n")
  if (secondPassRep <= 0)
    stop("'secondPassRep' must be a positive real number\n")       
  

  # define the function that we use to search for a root
  f <- function(h, ...) {
    log(nonparCusumARL(fit0, h,
                       location.shift.design = location.shift.design,
                       scale.shift.design = scale.shift.design,
                       location.shift = location.shift,
                       scale.shift = scale.shift,
                       ...)) - log(target)
  }

  # On the first pass, do small simulations to look for the CL, increasing by 1 unit
  h.increment <- 1
  gt0.counter <- lt0.counter <- 0
  h.vec <- arl.target <- NULL
  gt0.ind <- lt0.ind <- FALSE

  # First pass:  Want at least 3 points above and 3 points below
  while ((!gt0.ind) | (!lt0.ind)) {

    

  } # while

  
  
  return(list(h=h, arl.0.a=arl0.a, arl.0.a.sim=arl0.a.sim))
  
} # findNonparCusumCL
