##' Recursively apply simpson's rule to a set of closed intervals
##' @export
##' @param fn The function to integrate
##' @param params The set of intervals on which to apply 
##' the 3 point Simpson's Rule
##' @param h (x_i - x_j)/2 where x_i > x_j and [x_j, x_i] 
##' is a subinterval contained in params

sRecurse <- function(fn, params, h){
  
  vals <- sapply(2:length(params), function(i) simpsonsRule(fn, params[i-1], params[i], h))
  
  estimate <- sum(vals)
  
  return(estimate)
  
}