##' Split the domain of integration for adaptive Simpson's Rule
##' @export
##' @param params A set of closed intervals. Each interval will be split in two 
##' such that [a, b] = {[a, c], [c, b]} where c = midpoint(a, b)
##' @note It is assumed that a <= b for each subinterval. 
##' If this is not true, calculate the integral multiplied by -1.

paramSplit <- function(params){
  
  newParams <- sapply(2:length(params), function(i) (params[i] + params[i-1])/2)
  
  params <- sort(c(newParams, params))
  
  return(params)
  
}