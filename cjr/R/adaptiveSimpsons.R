##' Adaptive Simpson's Rule
##' @export 
##' @param fn The function to integrate
##' @param a The lower bound of integration
##' @param b The Upper bound of integration
##' @param eps The tolerance for stopping the integration routine
##' @param subdivisions The maximum number of simpson's rule estimates
##' to compute on the interval [a, b]
##' @description This algorithm was derived from a publication by the 
##' MathWorks team that produces Matlab and class notes from Simon Foucart's
##' Introduction to Numerical Mathematics course
##' @note It is assumed that subdivisions always double as each existing
##' interval is split into two equally sized subintevals. So 512 subdivisions
##' will increase to 1024 subdivisions if the tolerance is not met up to 
##' the maximum number of subdivisions.   

adaptiveSimpsons <- function(fn, a, b, eps, subdivisions = 1024){
  
  # The length of the range of integrantion
  delta <- b-a
  
  h <- delta/2
  
  # The midpoint of interval [a, b]
  c <- (a+b)/2
  
  # Initialize parameter vector
  params <- c(a, b)
  
  # The Simpson's rule estimate of integral f over (a,b)
  # Note that Simpson's rule estimates polynomials of degree
  # 3 or less exactly
  S_0 <- simpsonsRule(fn, a, b, h)
  
  flag <- TRUE
    
  while(flag){
    
    # For each iteration, the step size is cut in half
    h <- h/2
    
    # update parameters
    params <- paramSplit(params)
    
    # Calculate new Simpson's Rule estimate
    S_1 <- sRecurse(fn, params, h)
    
    # abs(New estimate - previous estimate)
    error.check <- abs(S_1 - S_0)
    
    # Stopping criterion adapted from Foucart's notes
    if(error.check < 30*(eps*h)/delta){
        
      Sigma <- S_1 + 1/15*(S_1 - S_0)
        
      flag <- FALSE
        
    } else {
      
      # current estimate is stored 
      S_0 <- S_1
      
    } 
    
    # If max number of subdivisions is reached, then 
    # exit the algorithm and return the estimate with 
    # max number of subdivisions
    
    if((length(params)-1) >= subdivisions){
      
      warning('Maximum subdivisions reached without convergence.')
      
      Sigma <- S_1 + 1/15*(S_1 - S_0)
      
      flag <- FALSE
      
    }
    
  }
  
  
  return(Sigma)
  
}