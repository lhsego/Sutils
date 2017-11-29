##' Calculate the upper bound or lower bound estimate of the 
##' confidence for the stratified sampling model
##' @export
##' @param N The sum of all grid cells within the population
##' @param n The sum of all sampled grid cells within the population
##' @param B The value of the hyperparameter associated with the beta prior
##' @param ub The number of unacceptable items one is willing to accept
##' @return conf The confidence 
##' @note To produce a lower bound estimate, set rho = min(rho).
##' To produce an upper bound estimate, set rho = 1.

boundaryConf <- function(N, n, B, ub, rho = 1, simpsonSample = 5e04+1){
  
  stopifnot(length(N) == 1,
            length(n) == 1,
            length(rho) == 1)
  
  # force integer as root finding algorithm will generate continuous
  # values of n
  n <- as.integer(n)
    
  # In R, a = 1 means the default value for a if no formal argument
  # is provided is a = 1. The same goes for any other "defined" arguments.
#   denom <- function(x, a = 1, b = -n, d = B + 1, z = rho){
#     # swap a and b (permissible by power series representation)
#     1/beta(a, d-a)*(x^(a-1)*(1-x)^(d-a-1)*(1-z*x)^(-b))
#   }

  denom <- function(x) (1-x)^(B-1)*(1-rho*x)^n
  
  # integ = simpson's rule
  # Need to sample densely when prior -> 0 
  if(simpsonSample %% 2 == 0){
    simpsonSample <- simpsonSample + 1
  }
  
  theta <- seq(0, 1, length = simpsonSample)

  denom <- integ(sapply(theta, denom), a = 0, b = 1)
    
  # pbinom is CDF of binomial distribution
  integrand <- function(x) pbinom(ub, N-n, rho*x)*(1-x)^(B-1)*(1-rho*x)^(n)
    
  conf <- integ(sapply(theta, integrand), a = 0, b = 1)/denom
    
  return(conf)
  
}