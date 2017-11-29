##' The 3 point Simpson's Rule
##' @param fn The function to integrate
##' @param a The lower bound of integration
##' @param b The upper bound of integration
##' @param h (b-a)/2
##' @export
##' 

simpsonsRule <- function(fn, a, b, h){
  c <- (a+b)/2
  h/3*(fn(a) + 4*fn(c) + fn(b))
}  
